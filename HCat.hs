import System.IO
import System.Environment
import System.Console.ANSI
import Text.ParserCombinators.Parsec
import Data.Either
import Data.List
import Control.Monad
import Text.Highlighting.Kate hiding (Color)

help :: String
help = "Usage: hcat file"

setColor :: String -> ColorIntensity -> Color -> IO ()
setColor s i c = do setSGR [SetColor Foreground i c]
                    putStr s
                    setSGR [Reset]

lineNumber :: Parser String
lineNumber = do num <- many1 digit
                spc <- many1 space
                return $ num ++ spc

source :: Parser String
source = do many1 digit
            many1 space
            many (noneOf "\n")

getSource :: String -> String
getSource line = let Right src = parse source "error" line in src

getLineNumber :: String -> String
getLineNumber line = let Right num = parse lineNumber "error" line in num

addLineNumber :: String -> [String]
addLineNumber input = map process [1..count]
  where process n = align n ++ lines input !! (n - 1)
        align n   = show n ++ replicate (length (show count) - length (show n) + 1) ' '
        count     = length $ lines input

parseSyntax :: [String] -> String -> [SourceLine]
parseSyntax ext = highlightAs lang
  where lang = if null ext then "" else head ext

selectColor :: (TokenType, String) -> IO ()
selectColor (FunctionTok, s) = setColor s Vivid Green
selectColor (KeywordTok , s) = setColor s Vivid Magenta
selectColor (CommentTok , s) = setColor s Vivid Black
selectColor (DataTypeTok, s) = setColor s Vivid Cyan
selectColor (CharTok    , s) = setColor s Dull Red
selectColor (StringTok  , s) = setColor s Dull Red
selectColor (DecValTok  , s) = setColor s Dull Cyan
selectColor (FloatTok   , s) = setColor s Dull Cyan
selectColor (_          , s) = putStr s

highlightSyntax :: [SourceLine] -> IO ()
highlightSyntax source = if null source
                         then putStrLn " "
                         else mapM_ selectColor (head source ++ [(NormalTok, "\n")])

colorize :: [String] -> [String] -> IO ()
colorize ext = mapM_ process
  where process line = do setColor (getLineNumber line) Vivid Black
                          highlightSyntax . parseSyntax ext $ getSource line

process :: [String] -> String -> IO ()
process ext input = colorize ext $ addLineNumber input

main :: IO ()
main = do args <- getArgs
          if length args > 0
            then withFile (head args) ReadMode (hGetContents >=> process (languagesByFilename $ head args))
            else putStrLn help