{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Extensions to Aeson parsing of objects.
module Data.Aeson.Extended (
    module Export
  -- * Extended failure messages
  , (.:)
  , (.:?)
  -- * JSON Parser that emits warnings
  , DescriptiveParser
  , JSONWarning (..)
  , withObjectWarnings
  , jsonSubWarnings
  , jsonSubWarningsT
  , jsonSubWarningsTT
  , logJSONWarnings
  , tellJSONField
  , unDescriptiveParser
  , jsonValidate
  , chainMaybe
  , Chain(..)
  , (..:)
  , (..:?)
  , (..!=)
  ) where

import Control.Applicative
import Control.Monad.Logger (MonadLogger, logWarn)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, mapWriterT, runWriterT, tell)
import Data.Aeson as Export hiding ((.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types hiding ((.:), (.:?))
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (unpack, Text)
import qualified Data.Text as T
import Data.Traversable
import qualified Data.Traversable as Traversable
import Prelude -- Fix redundant import warnings

-- | Extends @.:@ warning to include field name.
(.:) :: FromJSON a => Object -> Text -> Parser a
(.:) o p = modifyFailure (("failed to parse field '" <> unpack p <> "': ") <>) (o A..: p)
{-# INLINE (.:) #-}

-- | Extends @.:?@ warning to include field name.
(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) o p = modifyFailure (("failed to parse field '" <> unpack p <> "': ") <>) (o A..:? p)
{-# INLINE (.:?) #-}

-- | 'DescriptiveParser' version of @.:@.
(..:)
    :: FromJSON a
    => Object -> Text -> DescriptiveParser a
o ..: k = tellJSONField k *> DescriptiveParser (lift (o .: k))

-- | 'DescriptiveParser' version of @.:?@.
(..:?)
    :: FromJSON a
    => Object -> Text -> DescriptiveParser (Maybe a)
o ..:? k = tellJSONField k *> DescriptiveParser (lift (o .:? k))

-- | 'DescriptiveParser' version of @.!=@.
(..!=) :: DescriptiveParser (Maybe a) -> a -> DescriptiveParser a
wp ..!= d =
    DescriptiveParser(flip mapWriterT (runDescriptiveParser wp) $
                      \p ->
                           do a <- fmap snd p
                              fmap (, a) (fmap fst p .!= d))

-- | Tell warning parser about an expected field, so it doesn't warn about it.
tellJSONField :: Text -> DescriptiveParser ()
tellJSONField key = DescriptiveParser(tell (mempty { wpmExpectedFields = Set.singleton key}))

-- | 'DescriptiveParser' version of 'withObject'.
withObjectWarnings :: String
                   -> (Object -> DescriptiveParser a)
                   -> Value
                   -> Parser (a, [JSONWarning])
withObjectWarnings expected f =
    withObject expected $
    \obj ->
         do (a,w) <- runWriterT (runDescriptiveParser (f obj))
            let unrecognizedFields =
                    Set.toList
                        (Set.difference
                             (Set.fromList (HashMap.keys obj))
                             (wpmExpectedFields w))
            return
                ( a
                , wpmWarnings w ++
                  case unrecognizedFields of
                      [] -> []
                      _ -> [JSONUnrecognizedFields expected unrecognizedFields])

-- | Convert a 'DescriptiveParser' to a 'Parser'.
unDescriptiveParser :: DescriptiveParser a -> Parser a
unDescriptiveParser wp = do
    (a,_) <- runWriterT (runDescriptiveParser wp)
    return a

-- | Log JSON warnings.
logJSONWarnings
    :: MonadLogger m
    => FilePath -> [JSONWarning] -> m ()
logJSONWarnings fp =
    mapM_ (\w -> $logWarn ("Warning: " <> T.pack fp <> ": " <> T.pack (show w)))

-- | Handle warnings in a sub-object.
jsonSubWarnings :: DescriptiveParser (a, [JSONWarning]) -> DescriptiveParser a
jsonSubWarnings f =
    DescriptiveParser
        (do (result,warnings) <- runDescriptiveParser f
            tell
                (mempty
                 { wpmWarnings = warnings
                 })
            return result)

-- | Handle warnings in a @Traversable@ of sub-objects.
jsonSubWarningsT
    :: Traversable t
    => DescriptiveParser (t (a, [JSONWarning])) -> DescriptiveParser (t a)
jsonSubWarningsT f =
    DescriptiveParser
        (Traversable.mapM
             (runDescriptiveParser .
              jsonSubWarnings . DescriptiveParser . return) =<<
         runDescriptiveParser f)

-- | Handle warnings in a @Maybe Traversable@ of sub-objects.
jsonSubWarningsTT
    :: (Traversable t, Traversable u)
    => DescriptiveParser (u (t (a, [JSONWarning])))
    -> DescriptiveParser (u (t a))
jsonSubWarningsTT f =
    DescriptiveParser
        (Traversable.mapM
             (runDescriptiveParser .
              jsonSubWarningsT . DescriptiveParser . return) =<<
         runDescriptiveParser f)

-- | JSON parser that warns about unexpected fields in objects.
newtype DescriptiveParser a = DescriptiveParser
    { runDescriptiveParser :: WriterT DescriptiveParserMonoid Parser a
    } deriving (Applicative,Functor,Alternative)

-- | Monoid used by 'DescriptiveParser' to track expected fields and warnings.
data DescriptiveParserMonoid = DescriptiveParserMonoid
    { wpmExpectedFields :: !(Set Text)
    , wpmWarnings :: [JSONWarning]
    }
instance Monoid DescriptiveParserMonoid where
    mempty = DescriptiveParserMonoid Set.empty []
    mappend a b =
        DescriptiveParserMonoid
        { wpmExpectedFields = Set.union
              (wpmExpectedFields a)
              (wpmExpectedFields b)
        , wpmWarnings = wpmWarnings a ++ wpmWarnings b
        }

-- | Warning output from 'DescriptiveParser'.
data JSONWarning = JSONUnrecognizedFields String [Text]
instance Show JSONWarning where
    show (JSONUnrecognizedFields obj [field]) =
        "Unrecognized field in " <> obj <> ": " <> T.unpack field
    show (JSONUnrecognizedFields obj fields) =
        "Unrecognized fields in " <> obj <> ": " <> T.unpack (T.intercalate ", " fields)

-- | Run a validation and fail if the Either returns Left. This
-- allows us to have some dependency without monads.
jsonValidate :: DescriptiveParser a
             -> (a -> Either String b)
             -> DescriptiveParser b
jsonValidate p f =
    DescriptiveParser
        (do v <- runDescriptiveParser p
            case f v of
                Left e -> fail e
                Right k -> return k)

-- | An index into an object.
data Chain
    = Link !Text
    | Chain !Text
            !Chain

-- | Chain the list of parsers. This allows us to have some dependency
-- without monads.
chainMaybe :: FromJSON a => Object -> Chain -> DescriptiveParser (Maybe a)
chainMaybe o =
    \case
        (Link k) -> o ..:? k
        (Chain k c) ->
            DescriptiveParser
                (do r <- runDescriptiveParser (o ..:? k)
                    case r of
                        Nothing -> return Nothing
                        Just o' -> runDescriptiveParser (chainMaybe o' c))
