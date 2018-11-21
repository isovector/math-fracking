module Main where

import Data.Foldable
import Lib
import qualified Data.Text.IO as TIO
import System.Environment
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Render


parserConf :: ParserConf
parserConf = ParserConf
  { verbatimEnvironments = ["code", "verbatim", "lstlisting"]
  }


main :: IO ()
main = do
  [prefix, file] <- getArgs
  Right latex <- parseLaTeXFileWith parserConf file
  let (result, jobs) = frack prefix latex

  for_ (zip [0..] jobs) $ \(i, job) ->
    TIO.writeFile (prefix ++ show i ++ ".tex")
      . render
      $ getMathJob job

  TIO.putStrLn $ render result

