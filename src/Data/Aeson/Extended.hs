{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
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
  , Describe(..)
  , JSONWarning (..)
  , withObjectWarnings
  , prettyDesc
  , jsonSubWarnings
  , jsonSubWarningsT
  , jsonSubWarningsTT
  , logJSONWarnings
  , tellJSONField
  , unDescriptiveParser
  , describeDescriptiveParser
  , jsonValidate
  , chainMaybe
  , Chain(..)
  , (..:)
  , (..:?)
  , (..!=)
  ) where

import           Control.Applicative
import           Control.Monad.Logger (MonadLogger, logWarn)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Writer.Strict (WriterT, mapWriterT, runWriterT, tell)
import qualified Data.Aeson as A
import           Data.Aeson as Export hiding ((.:), (.:?))
import           Data.Aeson.Types hiding ((.:), (.:?))
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (unpack, Text)
import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Traversable as Traversable
import           Prelude -- Fix redundant import warnings

-- | Extends @.:@ warning to include field name.
(.:) :: FromJSON a => Object -> Text -> Parser a
(.:) o p = modifyFailure (("failed to parse field '" <> unpack p <> "': ") <>) (o A..: p)
{-# INLINE (.:) #-}

-- | Extends @.:?@ warning to include field name.
(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) o p = modifyFailure (("failed to parse field '" <> unpack p <> "': ") <>) (o A..:? p)
{-# INLINE (.:?) #-}

class Describe a  where
    describe :: Proxy a -> Text

instance Describe Object where describe _ = "object"
instance Describe Bool where describe _ = "boolean"
instance Describe Text where describe _ = "string"
instance Describe Char where describe _ = "character"
instance Describe Int where describe _ = "integer"
instance Describe a => Describe [a] where
    describe _ = case describe (Proxy :: Proxy a) of
                   "character" -> "string"
                   _ -> "list of " <> describe (Proxy :: Proxy a) <> "s"
instance Describe a => Describe (Set a) where
    describe _ = "set of " <> describe (Proxy :: Proxy a) <> "s"
instance Describe (Map k v) where describe _ = "mapping"

-- | 'DescriptiveParser' version of @.:@.
(..:)
    :: forall a. (FromJSON a,Describe a)
    => Object -> Text -> DescriptiveParser a
o ..: k =
    tellJSONField k *>
    DescriptiveParser
    { runDescriptiveParser = lift (o .: k)
    , describeDescriptiveParser = DescField (Link k) (describe (Proxy :: Proxy a))
    }

-- | 'DescriptiveParser' version of @.:?@.
(..:?)
    :: forall a. (FromJSON a,Describe a)
    => Object -> Text -> DescriptiveParser (Maybe a)
o ..:? k =
    tellJSONField k *>
    DescriptiveParser
    { runDescriptiveParser = (lift (o .:? k))
    , describeDescriptiveParser = DescOptionalField k (describe (Proxy :: Proxy a))
    }

-- | 'DescriptiveParser' version of @.!=@.
(..!=) :: Show a => DescriptiveParser (Maybe a) -> a -> DescriptiveParser a
wp ..!= d =
    DescriptiveParser
    { runDescriptiveParser = flip mapWriterT (runDescriptiveParser wp) $
      \p ->
           do a <- fmap snd p
              fmap (, a) (fmap fst p .!= d)
    , describeDescriptiveParser = DescNotEqual
          (describeDescriptiveParser wp)
    }

-- | Tell warning parser about an expected field, so it doesn't warn about it.
tellJSONField :: Text -> DescriptiveParser ()
tellJSONField key =
    DescriptiveParser
    { runDescriptiveParser = tell
          (mempty
           { wpmExpectedFields = Set.singleton key
           })
    , describeDescriptiveParser = descEmpty
    }

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
    { runDescriptiveParser = do (result,warnings) <- runDescriptiveParser f
                                tell
                                    (mempty
                                     { wpmWarnings = warnings
                                     })
                                return result
    , describeDescriptiveParser = describeDescriptiveParser f
    }

-- | Handle warnings in a @Traversable@ of sub-objects.
jsonSubWarningsT
    :: Traversable t
    => DescriptiveParser (t (a, [JSONWarning])) -> DescriptiveParser (t a)
jsonSubWarningsT f =
    DescriptiveParser
    { runDescriptiveParser = (Traversable.mapM
                                  (runDescriptiveParser .
                                   jsonSubWarnings . mkRunDesc . return) =<<
                              runDescriptiveParser f)
    , describeDescriptiveParser = describeDescriptiveParser f
    }
  where
    mkRunDesc m =
        DescriptiveParser
        { runDescriptiveParser = m
        , describeDescriptiveParser = descEmpty
        }

-- | Handle warnings in a @Maybe Traversable@ of sub-objects.
jsonSubWarningsTT
    :: (Traversable t, Traversable u)
    => DescriptiveParser (u (t (a, [JSONWarning])))
    -> DescriptiveParser (u (t a))
jsonSubWarningsTT f =
    DescriptiveParser
    { runDescriptiveParser = Traversable.mapM
          (runDescriptiveParser . jsonSubWarningsT . mkRunDesc . return) =<<
      runDescriptiveParser f
    , describeDescriptiveParser = describeDescriptiveParser f
    }
  where
    mkRunDesc m =
        DescriptiveParser
        { runDescriptiveParser = m
        , describeDescriptiveParser = descEmpty
        }

-- | A self-describing JSON parser that warns about unexpected fields
-- in objects.
data DescriptiveParser a = DescriptiveParser
    { runDescriptiveParser :: WriterT DescriptiveParserMonoid Parser a
    , describeDescriptiveParser :: Desc
    } deriving (Functor)

instance Applicative DescriptiveParser where
    pure x =
        DescriptiveParser
        { runDescriptiveParser = pure x
        , describeDescriptiveParser = descEmpty
        }
    f <*> a =
        DescriptiveParser
        { runDescriptiveParser = runDescriptiveParser f <*>
          runDescriptiveParser a
        , describeDescriptiveParser = descAnd (describeDescriptiveParser f)
          (describeDescriptiveParser a)
        }

instance Alternative DescriptiveParser where
    empty =
        DescriptiveParser
        { runDescriptiveParser = empty
        , describeDescriptiveParser = descEmpty
        }
    x <|> y =
        DescriptiveParser
        { runDescriptiveParser = runDescriptiveParser x <|>
          runDescriptiveParser y
        , describeDescriptiveParser = descOr
              (describeDescriptiveParser x)
              (describeDescriptiveParser y)
        }

-- | Description of a JSON parser.
data Desc
    = DescEmpty
    | DescAnd ![Desc]
    | DescOr ![Desc]
    | DescField !Chain !Text
    | DescOptionalField !Text !Text
    | DescNotEqual !Desc
    deriving (Show)

-- | Monoidal empty, for either AND or OR.
descEmpty :: Desc
descEmpty = DescEmpty

-- | Mappend for concatenation.
descAnd :: Desc -> Desc -> Desc
descAnd DescEmpty x = x
descAnd x DescEmpty = x
descAnd (DescAnd x) (DescAnd y) = DescAnd (x ++ y)
descAnd (DescAnd x) y = DescAnd (x ++ [y])
descAnd x (DescAnd y) = DescAnd ([x] ++ y)
descAnd x y = DescAnd [x,y]

-- | Mappend for disjunction.
descOr :: Desc -> Desc -> Desc
descOr DescEmpty x = x
descOr x DescEmpty = x
descOr (DescOr x) (DescOr y) = DescOr (x ++ y)
descOr (DescOr x) y = DescOr (x ++ [y])
descOr x (DescOr y) = DescOr ([x] ++ y)
descOr x y = DescOr [x,y]

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
jsonValidate :: DescriptiveParser a -- ^ Parser from which value to work on.
             -> (a -> Either String b) -- ^ Validating function.
             -> DescriptiveParser b
jsonValidate p f =
    DescriptiveParser
    { runDescriptiveParser = do v <- runDescriptiveParser p
                                case f v of
                                    Left e -> fail e
                                    Right k -> return k
    , describeDescriptiveParser = describeDescriptiveParser p
    }

-- | An index into an object.
data Chain
    = Link !Text
    | Chain !Text
            !Chain
  deriving (Show)

-- | Chain the list of parsers. This allows us to have some dependency
-- without monads.
chainMaybe
    :: forall a. (FromJSON a, Describe a)
    => Object -> Chain -> DescriptiveParser (Maybe a)
chainMaybe o chain =
    case chain of
        (Link k) -> o ..:? k
        (Chain k c) ->
            DescriptiveParser
            { runDescriptiveParser = do r <- runDescriptiveParser (o ..:? k)
                                        case r of
                                            Nothing -> return Nothing
                                            Just o' ->
                                                runDescriptiveParser
                                                    (chainMaybe o' c)
            , describeDescriptiveParser = DescField chain (describe (Proxy :: Proxy a))
            }

prettyDesc :: Desc -> Text
prettyDesc = go
  where
    go (DescAnd xs) = T.unlines (map go xs)
    go (DescOr xs) = "One of:\n" <> indent (T.intercalate "\nOR\n" (map go xs))
    go DescEmpty = ""
    go (DescField f t) = renderChain f <> ": " <> t <> "" <> " [optional]"
    go (DescOptionalField f t) =
        f <> ": " <> t <> " [optional]"
    go (DescNotEqual n) = go n
    indent = T.unlines . map ("  " <>) . T.lines
    renderChain (Link x) = x
    renderChain (Chain this that) = this <> "." <> renderChain that
