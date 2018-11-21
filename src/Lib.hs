{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wall #-}

module Lib where

import Control.Applicative
import Control.Monad.State
import Data.Text (pack)
import Text.LaTeX
import Text.LaTeX.Base.Syntax

type Latex = LaTeX


newtype MathJob = MathJob { getMathJob :: Latex }


extractMath :: Latex -> [MathJob]
extractMath (TeXRaw _)         = []
extractMath (TeXComm _ ns)     = extractMath =<< extractArgs =<< ns
extractMath (TeXCommS _)       = []
extractMath (TeXEnv _ ns t)    = extractMath =<< ((++ [t]) . extractArgs) =<< ns
extractMath t@(TeXMath _ _)    = pure $ MathJob t
extractMath (TeXLineBreak _ _) = []
extractMath (TeXBraces t)      = extractMath t
extractMath (TeXComment _)     = []
extractMath (TeXSeq a b)       = extractMath =<< [a, b]
extractMath (TeXEmpty)         = []

extractArgs :: TeXArg -> [Latex]
extractArgs (FixArg t)  = pure t
extractArgs (OptArg t)  = pure t
extractArgs (MOptArg t) = t
extractArgs (SymArg t)  = pure t
extractArgs (MSymArg t) = t
extractArgs (ParArg t)  = pure t
extractArgs (MParArg t) = t


blowoutMath :: String -> Latex -> Latex
blowoutMath prefix =
    fst . flip runState (0 :: Int) . texmapM isMath makeInput
  where
    isMath (TeXMath _ _) = True
    isMath _             = False

    makeInput = \(TeXMath a _) -> do
      v <- get
      modify (+1)
      pure . TeXComm "input"
           . pure
           . FixArg
           . TeXRaw
           . pack
           $ prefix ++ show v ++ "." ++ show a


frack :: String -> Latex -> (Latex, [MathJob])
frack prefix = liftA2 (,) (blowoutMath prefix) extractMath

