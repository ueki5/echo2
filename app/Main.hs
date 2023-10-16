{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Protolude.Monad(unless)
import Options.Applicative

data Argument = Argument
    { noTrailingNewline :: Bool
    , text :: String
    } deriving (Read, Show)

argumentParser :: Parser Argument
argumentParser = Argument
    <$> switch (short 'n' <> help "Do not output the trailing newline")
    <*> strArgument (metavar "STRING" <> value "")

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

argumentParserInfo :: ParserInfo Argument
argumentParserInfo = argumentParser `withInfo` "display a line of text"

main :: IO ()
main = execParser argumentParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain Argument {..} = do
    putStr text
    unless noTrailingNewline $ putChar '\n'