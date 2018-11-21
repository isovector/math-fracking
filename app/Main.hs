module Main where

import Data.Foldable
import Lib
import qualified Data.Text.IO as TIO
import System.Environment
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax


parserConf :: ParserConf
parserConf = ParserConf
  { verbatimEnvironments = ["code", "verbatim", "lstlisting"]
  }


main :: IO ()
main = do
  (ifile : prefix1 : prefix2' : z) <- getArgs
  let (prefix2, file) =
        case z of
          [] -> (prefix1, prefix2')
          [f] -> (prefix2', f)

  Right injected <- parseLaTeXFileWith parserConf ifile
  Right latex <- parseLaTeXFileWith parserConf file
  let (result, jobs) = frack prefix2 latex

  for_ (zip [0..] jobs) $ \(i, job@(MathJob (TeXMath a _))) ->
    TIO.writeFile (prefix1 ++ show i ++ "." ++ show a ++ ".tex")
      . render
      . getMathJob
      $ inject job injected

  TIO.putStrLn $ render result


inject :: MathJob -> Latex -> MathJob
inject (MathJob (TeXMath a b)) l =
  MathJob $ TeXMath a $ TeXSeq l b


