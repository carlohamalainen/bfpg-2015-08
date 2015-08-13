{-# LANGUAGE Rank2Types, OverloadedStrings, CPP #-}

module Utils where

import Data.Generics hiding (typeOf)
import GHC
import GHC.SYB.Utils

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))
