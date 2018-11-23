{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wall #-}

module Lib where

import Control.Monad.RWS
import Data.Text (pack)
import Text.LaTeX
import Text.LaTeX.Base.Syntax

type Latex = LaTeX


data MathJob = MathJob
  { mathJobGiven :: FilePath
  , getMathJob :: Latex
  }


runRWS' :: Monoid w => s -> RWS () w s a -> (a, w)
runRWS' s m =
  let (a, _, w) = runRWS m () s
   in (a, w)


frack :: String -> String -> Latex -> (Latex, [MathJob])
frack given taken =
    runRWS' (0 :: Int) . texmapM isMath makeInput
  where
    isMath (TeXMath _ _) = True
    isMath _             = False

    getName prefix v a = prefix ++ show v ++ "." ++ show a ++ ".tex"

    makeInput job@(TeXMath a _) = do
      v <- get
      modify (+1)
      let givenName = getName given v a
          takenName = getName taken v a

      tell . pure $ MathJob givenName job
      pure . TeXComm "input"
           . pure
           . FixArg
           . TeXRaw
           $ pack takenName


