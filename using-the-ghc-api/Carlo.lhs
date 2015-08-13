% \section{Goals}

% TODO Static slides in case demo breaks!

% sed -i 's/-- documentation: False/documentation: True/g'         ~/.cabal/config
% sed -i 's/-- library-profiling: False/library-profiling: True/g' ~/.cabal/config
% sed -i 's/-- jobs:/jobs: $ncpus/g' ~/.cabal/config
%
% cabal install --haddock-hyperlink-source foobar

\begin{frame}
\frametitle{Motivation}

\begin{itemize}
\item
``Haskell in the Financial District: an Experience Report'' (BFPG May 2013 -- Sam Roberts)

\item
Sam said something like:

\begin{quote}
"People learned Haskell (Mu) in a Visual Studio environment; those
who used the editor tools learned quicker than those who did not."
\end{quote}

\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Motivation}

Seems to make sense!

\begin{itemize}


\item Haskell has a great type system. It does a lot.
\item Programming is hard.
\item My brain is small.
\item I'll write a Vim plugin to help!
\item Profit?

\end{itemize}

\end{frame}


\begin{frame}[fragile]
\frametitle{What is this symbol?}

\begin{itemize}

\item You see \texttt{mapM\_}. What is it?

\item Hoogle says:

\begin{verbatim}
    mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
    base Prelude, base Control.Monad

    mapM_ :: (Foldable t, Monad m) => (a -> m b)
                                   -> t a -> m ()
    base Data.Foldable
\end{verbatim}

\item Hoogle doesn't mention:

\begin{verbatim}
    mapM_ :: Monad m => (a -> m ()) -> Consumer a m ()
    Data.Conduit.List
\end{verbatim}

\item Not to mention local packages that are not on Hoogle/Hackage!

\item Simple idea: use ghc-mod to run :info on a symbol.

\end{itemize}

\end{frame}

\begin{frame}[fragile]
\frametitle{Use :info}

\begin{verbatim}
*Demo> :info mapM_
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  	-- Defined in ‘Control.Monad’

*Demo> :info map
map :: (a -> b) -> [a] -> [b] 	-- Defined in ‘GHC.Base’
\end{verbatim}

Now find the package that exports Control.Monad:

{\tiny \texttt{http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html} }

But I had locally built documentation:

{\tiny \texttt{/opt/ghc-7.8.4\_build/share/doc/ghc/html/libraries/base-4.7.0.2/Control-Monad.html} }

\end{frame}


\begin{frame}[fragile]
\frametitle{:info gives defined in, not exported from}

\begin{itemize}

\item
\begin{verbatim}
module Hiding where

import Data.List hiding (map)

m = map (+1) [1, 2, 3]
h = head [1, 2, 3]
\end{verbatim}

\item
In ghci:

\begin{verbatim}
*Hiding> :info map
map :: (a -> b) -> [a] -> [b] 	-- Defined in ‘GHC.Base’

*Hiding> :info head
head :: [a] -> a 	-- Defined in ‘GHC.List’
\end{verbatim}

\item No page for GHC.Base (it's internal).
\item GHC.List isn't quite right, it should be Data.List.

\end{itemize}

\end{frame}


\begin{frame}
\frametitle{What we have to find}

\begin{itemize}

\item Imports in a module, e.g. \texttt{import Data.List}
\item Names in a module, e.g. \texttt{head}
\item Module that a name is {\em imported from}, not defined in.
\item Haddock URL to the module where the symbol is imported from.
\end{itemize}


\end{frame}

\begin{frame}[fragile]
\frametitle{Load a module with the GHC API}
\begin{verbatim}
main = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags

  GhcMonad.liftIO $ Packages.initPackages dflags

  target <- guessTarget "Hiding.hs" Nothing
  setTargets [target]
  load LoadAllTargets

  modSummary <- getModSummary (mkModuleName "Hiding")
  p <- parseModule modSummary :: Ghc ParsedModule
  t <- typecheckModule p      :: Ghc TypecheckedModule
  d <- desugarModule t        :: Ghc DesugaredModule
\end{verbatim}
\end{frame}

\begin{frame}[fragile]
\frametitle{Dump the guts!}
\begin{verbatim}
let guts = coreModule d             :: ModGuts
    gre  = HscTypes.mg_rdr_env guts :: GlobalRdrEnv

GhcMonad.liftIO $ putStrLn $ showSDoc dflags (ppr gre)
\end{verbatim}
\end{frame}

\begin{frame}[fragile]
\frametitle{The guts!}
%Data.List exports map but is hidden, so we get the
%GHC.Base.map from the Prelude:

% Qualified name for \texttt{head} in \texttt{Hiding.hs} is \texttt{GHC.List.head}.

\begin{verbatim}
ihZ :-> [GHC.Base.map
           imported from ‘Prelude’ at Hiding.hs:1:8-13
           (and originally defined in ‘base:GHC.Base’)],
\end{verbatim}

%Data.List exports head, so this overrides head from the Prelude:

\begin{verbatim}
ik7 :-> [GHC.List.head
           imported from ‘Data.List’ at Hiding.hs:3:1-29
           (and originally defined in ‘base:GHC.List’)],
\end{verbatim}

1. http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html

2. http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-List.html

\end{frame}



\begin{frame}[fragile,t]
\frametitle{Work backwards...}
\begin{verbatim}
blah :: Ghc ()
blah = do



    let d = undefined               :: DesugaredModule
    let guts = coreModule d         :: ModGuts

    return ()
\end{verbatim}
\end{frame}

\begin{frame}[fragile,t]
\frametitle{Work backwards...}
\begin{verbatim}
blah :: Ghc ()
blah = do



    d <- desugarModule undefined    :: Ghc DesugaredModule
    let guts = coreModule d         :: ModGuts

    return ()
\end{verbatim}
\end{frame}

\begin{frame}[fragile,t]
\frametitle{Work backwards...}
\begin{verbatim}
blah :: Ghc ()
blah = do


    t <- typecheckModule undefined  :: Ghc TypecheckedModule
    d <- desugarModule t            :: Ghc DesugaredModule
    let guts = coreModule d         :: ModGuts

    return ()
\end{verbatim}
\end{frame}

\begin{frame}[fragile,t]
\frametitle{Work backwards...}
\begin{verbatim}
blah :: Ghc ()
blah = do

    p <- parseModule undefined      :: Ghc ParsedModule
    t <- typecheckModule p          :: Ghc TypecheckedModule
    d <- desugarModule t            :: Ghc DesugaredModule
    let guts = coreModule d         :: ModGuts

    return ()
\end{verbatim}
\end{frame}

\begin{frame}[fragile,t]
\frametitle{Work backwards...}
\begin{verbatim}
blah :: Ghc ()
blah = do
    modSum <- getModSummary undefined
    p <- parseModule undefined      :: Ghc ParsedModule
    t <- typecheckModule p          :: Ghc TypecheckedModule
    d <- desugarModule t            :: Ghc DesugaredModule
    let guts = coreModule d         :: ModGuts

    return ()
\end{verbatim}
\end{frame}


%\begin{frame}[fragile]
%\frametitle{Dump the guts!}
%\begin{verbatim}
%let guts = coreModule d             :: ModGuts
%    gre  = HscTypes.mg_rdr_env guts :: GlobalRdrEnv
%
%GhcMonad.liftIO $ putStrLn $ showSDoc dflags (ppr gre)
%\end{verbatim}
%\end{frame}

\begin{frame}[fragile]
\frametitle{The lookup process}

Input: \texttt{head} at (11, 17) in \texttt{Demo.hs}

\begin{enumerate}

\item Partially compile \texttt{Demo.hs}: list of qualified names (e.g.  \texttt{GHC.List.head}).

\item Match \texttt{head} to \texttt{GHC.List.head} using heuristics. Module load order!?

\item Discover \texttt{GHC.List.head} imported from \texttt{Data.List}.

\item \texttt{ghc-pkg find-module Data.List --simple-output --global --user}

\item Package could be \texttt{haskell98-2.0.0.3} or \texttt{haskell2010-1.1.2.0}.

\item Final answer could be
{\tiny \texttt{file:///home/carlo/opt/ghc-7.8.4\_build/share/doc/ghc/html/libraries/haskell98-2.0.0.3/Prelude.html}}
or
{\tiny \texttt{file:///home/carlo/opt/ghc-7.8.4\_build/share/doc/ghc/html/libraries/haskell2010-1.1.2.0/Data-List.html}}

\end{enumerate}

\end{frame}

\begin{frame}
\frametitle{This is what we've done}

\centering
\includegraphics[width=70mm]{happy_tree_happy_friends_pinata.jpg}

\end{frame}

\begin{frame}
\frametitle{Conclusion}

\begin{itemize}

\item Useful for me, especially when using Yesod.

\item Useful for others (e.g. Emacs plugin contributed).

\item Has some corner cases - Haskell module system more complicated than I realised.

\item GHC API wasn't too hard to use. Use ghc-mod and glue things together.

\item Latest GHC API has plugins? Might help?

\end{itemize}

\medskip

\end{frame}



\begin{frame}
\frametitle{Links}

\texttt{https://github.com/carlohamalainen/ghc-imported-from}
\medskip

\texttt{https://github.com/carlohamalainen/ghcimportedfrom-vim}
\medskip

\texttt{https://github.com/david-christiansen/ghc-imported-from-el}
\medskip

\texttt{http://www.mew.org/\~{}kazu/proj/ghc-mod/en}
\medskip

\texttt{https://github.com/ndmitchell/ghcid}


\end{frame}

