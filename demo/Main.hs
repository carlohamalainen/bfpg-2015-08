module Main where

import Control.Monad.Reader
import Control.Monad
import Data.Typeable()
import Desugar()
import DynFlags (unsafeGlobalDynFlags)
import GHC
import GHC.Paths (libdir)
import GHC.SYB.Utils()
import HscTypes
import Outputable
import RdrName
import System.Environment()
import TcRnTypes()

import qualified DynFlags()
import qualified GhcMonad
import qualified MonadUtils()
import qualified Packages

import Utils (listifySpans)

startHere :: Ghc ()
startHere = do

    -- modSummary <- getModSummary undefined       :: Ghc ModSummary
    -- p          <- parseModule modSummary
    -- t          <- typecheckModule p                     :: Ghc TypecheckedModule
    -- d          <- desugarModule t                       :: Ghc DesugaredModule


    -- let guts = coreModule (undefined :: DesugaredModule) :: ModGuts

    let gre = HscTypes.mg_rdr_env undefined :: GlobalRdrEnv

    return ()


foo :: Ghc ()
foo = do
    dflags <- getSessionDynFlags

    _ <- setSessionDynFlags dflags
    _ <- GhcMonad.liftIO $ Packages.initPackages dflags

    target <- guessTarget "Hiding.hs" Nothing
    setTargets [target]
    s <- load LoadAllTargets

    -- GhcMonad.liftIO $ print $ showSDoc dflags (ppr s)

    modSummary <- getModSummary (mkModuleName "Hiding") :: Ghc ModSummary
    p          <- parseModule modSummary                :: Ghc ParsedModule
    t          <- typecheckModule p                     :: Ghc TypecheckedModule
    d          <- desugarModule t                       :: Ghc DesugaredModule

    let guts = coreModule d             :: ModGuts
        gre  = HscTypes.mg_rdr_env guts :: GlobalRdrEnv

    GhcMonad.liftIO $ putStrLn $ showSDoc dflags (ppr gre)

    setContext $ map (IIDecl . simpleImportDecl . mkModuleName) ("Hiding":["Data.List", "System.Environment"])

    modSummary <- getModSummary $ mkModuleName "Hiding" :: Ghc ModSummary
    p <- parseModule modSummary   :: Ghc ParsedModule
    t <- typecheckModule p        :: Ghc TypecheckedModule

    let lineNr = 9
        colNr  = 5

    let TypecheckedModule{tm_typechecked_source = tcs} = t
        bs = listifySpans tcs (lineNr, colNr) :: [LHsBind Id]
        es = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
        ps = listifySpans tcs (lineNr, colNr) :: [LPat Id]

    let foo x = showSDoc unsafeGlobalDynFlags $ ppr x
        bs' = map foo bs
        es' = map foo es
        ps' = map foo ps

    GhcMonad.liftIO $ forM_ (filter (not . (' ' `elem`)) $ bs' ++ es' ++ ps') putStrLn

    return ()


main = runGhc (Just libdir) foo


blerp :: Ghc ()
blerp = do

    modSummary <- getModSummary undefined       :: Ghc ModSummary
    p          <- parseModule modSummary
    t          <- typecheckModule p                     :: Ghc TypecheckedModule
    d          <- desugarModule t                       :: Ghc DesugaredModule



    -- let guts = coreModule (undefined :: DesugaredModule) :: ModGuts
    let guts = coreModule d :: ModGuts
        gre  = HscTypes.mg_rdr_env guts                  :: GlobalRdrEnv

    return ()

_blerp :: Ghc ()
_blerp = do

    -- modSummary <- getModSummary undefined       :: Ghc ModSummary
    -- p          <- parseModule modSummary
    -- t          <- typecheckModule p                     :: Ghc TypecheckedModule
    -- d          <- desugarModule t                       :: Ghc DesugaredModule

    d <- desugarModule undefined :: Ghc DesugaredModule

    let guts = coreModule d :: ModGuts
    let -- guts = coreModule undefined :: ModGuts
        gre  = HscTypes.mg_rdr_env undefined :: GlobalRdrEnv

    return ()


_____blerp :: Ghc ()
_____blerp = do

    -- modSummary <- getModSummary undefined       :: Ghc ModSummary
    -- p          <- parseModule modSummary
    -- t          <- typecheckModule p                     :: Ghc TypecheckedModule
    -- d          <- desugarModule t                       :: Ghc DesugaredModule

    d <- desugarModule undefined :: Ghc DesugaredModule

    let guts = coreModule d :: ModGuts
    let gre  = HscTypes.mg_rdr_env guts :: GlobalRdrEnv

    return ()
{-
blah :: Ghc ()
blah = do

    -- p <- parseModule undefined     :: Ghc ParsedModule
    -- t <- typecheckModule p         :: Ghc TypecheckedModule
    -- d <- desugarModule t           :: Ghc DesugaredModule
    let d = undefined :: DesugaredModule
    let guts = coreModule d        :: ModGuts

    return ()
-}

type MyThing = ReaderT Int IO

sdff :: MyThing String
sdff = do
    x <- ask

    let foo = foldl (\x y -> x + length y) 0 ["foo", ""]

    if x == 1
        then undefined
        else liftIO $ print "foo"

    return undefined





























blah :: Ghc ()
blah = do

    p <- parseModule undefined     :: Ghc ParsedModule
    t <- typecheckModule p
    d <- desugarModule t
    -- let d = undefined :: DesugaredModule
    let guts = coreModule d        :: ModGuts

    return ()











