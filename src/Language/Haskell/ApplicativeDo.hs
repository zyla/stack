{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- | Provides a handy translation from do-notation to
-- applicative, as in the upcoming ApplicativeDo.

module Language.Haskell.ApplicativeDo where

import Language.Haskell.TH

-- | Convert a do-notation @do {x <- a; y <- b; foo x y}@ into @(\a b -> f) <$> x <*> y@.
ado :: Q Exp -> Q Exp
ado get = do
    e <- get
    parseDo e

-- | Parse a @do {x <- a; y <- b; foo x y}@ into @(\a b -> f) <$> x <*> y@.
parseDo :: Exp -> Q Exp
parseDo e = do
    case e of
        DoE stmts -> parseStmts stmts
        _ -> error "Expected a `do' expression."

-- | Parse the statements list [a <- x,b <- y,f] into @(\a b -> f) <$> x <*> y@.
parseStmts :: [Stmt] -> Q Exp
parseStmts = go []
  where
    go :: [(Pat, Exp)] -> [Stmt] -> Q Exp
    go [] [_] = error "Expected at least one argument to the final function."
    go (reverse -> args) [NoBindS f] =
        let pats = map fst args
            apargs = map (pure . snd) args
            first_aparg = head apargs
        in foldl
               (\left right ->
                     [|$(left) <*> $(right)|])
               [|$(lamE (map pure pats) (pure f)) <$> $(first_aparg)|]
               (drop 1 apargs)
    go args (BindS p e:xs) = go ((p, e) : args) xs
    go _ _ = error "Expected expression of form: do {x <- a; y <- b; foo x y}"
