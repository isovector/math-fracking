module Main where

import           Data.Foldable
import qualified Data.Text.IO as TIO
import           Lib
import           System.Environment
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Render
import           Text.LaTeX.Base.Syntax


parserConf :: ParserConf
parserConf = ParserConf
  { verbatimEnvironments = ["code", "verbatim", "lstlisting", "dorepl"]
  }


main :: IO ()
main = do
  (ifile : given : taken' : z) <- getArgs
  let (taken, file) =
        case z of
          [] -> (given, taken')
          [f] -> (taken', f)

  Right injected <- parseLaTeXFileWith parserConf ifile
  Right latex <- parseLaTeXFileWith parserConf file
  let (result, jobs) = frack given taken latex

  for_ jobs $ \(MathJob fn l) ->
    TIO.writeFile fn
      . render
      $ inject l injected

  TIO.putStrLn $ render result


inject :: Latex -> Latex -> Latex
inject (TeXMath a b) l =
  TeXMath a $ TeXSeq l b


