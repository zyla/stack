-- FIXME See how much of this module can be deleted.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (getGlobalDB
  ,EnvOverride
  ,envHelper
  ,createDatabase
  ,unregisterGhcPkgName
  ,unregisterGhcPkgId
  ,getCabalPkgVer
  ,ghcPkgExeName
  ,ghcPkgCheck
  ,mkGhcPackagePath)
  where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Maybe.Extra (mapMaybeM)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Encoding.Error (lenientDecode)
import           Path (Path, Abs, Dir, toFilePath, parent)
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import           Prelude hiding (FilePath)
import           Stack.Constants
import           Stack.Types
import           System.FilePath (searchPathSeparator)
import           System.Process.Read

-- | Get the global package database
getGlobalDB :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
            => EnvOverride -> WhichCompiler -> m (Path Abs Dir)
getGlobalDB menv wc = do
    $logDebug "Getting global package database location"
    -- This seems like a strange way to get the global package database
    -- location, but I don't know of a better one
    bs <- ghcPkg menv wc [] ["list", "--global"] >>= either throwM return
    let fp = S8.unpack $ stripTrailingColon $ firstLine bs
    resolveDir' fp
  where
    stripTrailingColon bs
        | S8.null bs = bs
        | S8.last bs == ':' = S8.init bs
        | otherwise = bs
    firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
       => EnvOverride
       -> WhichCompiler
       -> [Path Abs Dir]
       -> [String]
       -> m (Either ReadProcessException S8.ByteString)
ghcPkg menv wc pkgDbs args = do
    eres <- go
    case eres of
          Left _ -> do
              mapM_ (createDatabase menv wc) pkgDbs
              go
          Right _ -> return eres
  where
    go = tryProcessStdout Nothing menv (ghcPkgExeName wc) args'
    args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
               => EnvOverride -> WhichCompiler -> Path Abs Dir -> m ()
createDatabase menv wc db = do
    exists <- doesDirExist db
    unless exists $ do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        ensureDir (parent db)
        _ <- tryProcessStdout Nothing menv (ghcPkgExeName wc) ["init", toFilePath db]
        return ()

-- | Get the name to use for "ghc-pkg", given the compiler version.
ghcPkgExeName :: WhichCompiler -> String
ghcPkgExeName Ghc = "ghc-pkg"
ghcPkgExeName Ghcjs = "ghcjs-pkg"

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
          "--no-user-package-db"
        : map (\x -> "--package-db=" ++ toFilePath x) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride
    -> WhichCompiler
    -> [Path Abs Dir] -- ^ package databases
    -> String -- ^ package identifier, or GhcPkgId
    -> Text
    -> m (Maybe Text)
findGhcPkgField menv wc pkgDbs name field = do
    result <-
        ghcPkg
            menv
            wc
            pkgDbs
            ["field", "--simple-output", name, T.unpack field]
    return $
        case result of
            Left{} -> Nothing
            Right lbs ->
                fmap (stripCR . T.decodeUtf8) $ listToMaybe $ S8.lines lbs
  where
    stripCR t = fromMaybe t (T.stripSuffix "\r" t)

-- | Get the version of the package
findGhcPkgVersion :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
                  => EnvOverride
                  -> WhichCompiler
                  -> [Path Abs Dir] -- ^ package databases
                  -> PackageName
                  -> m (Maybe Version)
findGhcPkgVersion menv wc pkgDbs name = do
    mv <- findGhcPkgField menv wc pkgDbs (packageNameString name) "version"
    case mv of
        Just !v -> return (parseVersion v)
        _ -> return Nothing

unregisterGhcPkgName :: (MonadIO m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
                     => EnvOverride
                     -> WhichCompiler
                     -> [Path Abs Dir] -- ^ package databases
                     -> PackageName
                     -> m ()
unregisterGhcPkgName menv wc pkgDbs pkgName =
    either throwM ($logDebug . T.decodeUtf8With lenientDecode) =<<
    ghcPkg menv wc pkgDbs ["unregister", "--user", "--force", packageNameString pkgName]

unregisterGhcPkgId :: (MonadIO m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
                    => EnvOverride
                    -> WhichCompiler
                    -> CompilerVersion
                    -> [Path Abs Dir] -- ^ package databases
                    -> GhcPkgId
                    -> PackageIdentifier
                    -> m ()
unregisterGhcPkgId menv wc cv pkgDbs gid ident = do
    eres <- ghcPkg menv wc pkgDbs args
    case eres of
        Left e -> $logWarn $ T.pack $ show e
        Right _ -> return ()
  where
    -- TODO ideally we'd tell ghc-pkg a GhcPkgId instead
    args = "unregister" : "--user" : "--force" :
        (case cv of
            GhcVersion v | v < $(mkVersion "7.9") ->
                [packageIdentifierString ident]
            _ -> ["--ipid", ghcPkgIdString gid])

-- | Get the version of Cabal from the global package database.
getCabalPkgVer :: (MonadThrow m, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
               => EnvOverride -> WhichCompiler -> m Version
getCabalPkgVer menv wc = do
    $logDebug "Getting Cabal package version"
    mres <- findGhcPkgVersion
        menv
        wc
        [] -- global DB
        cabalPackageName
    maybe (throwM $ Couldn'tFindPkgId cabalPackageName) return mres

ghcPkgCheck :: (MonadIO m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
            => EnvOverride
            -> WhichCompiler
            -> [Path Abs Dir] -- ^ package databases
            -> m [PackageName] -- ^ broken packages
ghcPkgCheck menv wc pkgDbs = do
    eres <- ghcPkg menv wc pkgDbs ["check"]
    case eres of
        Left err -> throwM err
        Right bs -> mapMaybeM toBrokenPackage (S8.splitWith (`elem` ("\n\r" :: [Char])) bs)
  where
    toBrokenPackage ( T.stripPrefix "There are problems in package " . T.decodeUtf8
                    -> Just (T.stripSuffix ":" -> Just name)
                    ) = do
        case parsePackageName name of
            -- TODO: custom error
            Left (fromException -> Just (err :: PackageNameParseFail)) ->
                error ("Error parsing package name from 'ghc-pkg check' output: " ++ show err)
            Left err -> throwM err
            Right x -> return (Just x)
    toBrokenPackage _ = return Nothing

-- | Get the value for GHC_PACKAGE_PATH
mkGhcPackagePath :: Bool -> Path Abs Dir -> Path Abs Dir -> [Path Abs Dir] -> Path Abs Dir -> Text
mkGhcPackagePath locals localdb deps extras globaldb =
  T.pack $ intercalate [searchPathSeparator] $ concat
    [ [toFilePathNoTrailingSep localdb | locals]
    , [toFilePathNoTrailingSep deps]
    , [toFilePathNoTrailingSep db | db <- reverse extras]
    , [toFilePathNoTrailingSep globaldb]
    ]
