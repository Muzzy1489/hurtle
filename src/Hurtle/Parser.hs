{-# OPTIONS_GHC -Wno-type-defaults #-}
module Hurtle.Parser where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

{-# LANGUAGE OverloadedStrings #-}

import Data.Char ( toLower, toUpper )

-------------------------------------------Cleaning Data Functions--------------------------------------------------------

-- Correct implementation of caseInsensitiveString
caseInsensitiveString :: String -> Parser String
caseInsensitiveString = mapM (\c -> char (toLower c) <|> char (toUpper c))

-- Skips whitespace and comments, including end-of-line comments (if implemented)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- Parser for floats and integers, treating everything as a float for simplicity
number :: Parser Float
number = L.signed sc (try L.float <|> fmap fromIntegral L.decimal)

----------------------------------------------------------------------------------------------------------------------------


---------------------------------------------Normal Command Functions---------------------------------------------------------

-- Generic command parser
command :: String -> (Float -> HogoCode) -> Parser HogoCode
command name constructor = constructor <$> (caseInsensitiveString name *> sc *> number)

-- Updated command parsers using the generic 'command' function
goForward :: Parser HogoCode
goForward = command "forward" GoForward

goBackward :: Parser HogoCode
goBackward = command "back" GoBackward

turnLeft :: Parser HogoCode
turnLeft = command "left" TurnLeft

turnRight :: Parser HogoCode
turnRight = command "right" TurnRight

------------------------------------------------------------------------------------------------------------------------------


----------------------------------------------Simple Command Functions--------------------------------------------------------

-- Generic non-parameter command parser
simpleCommand :: String -> HogoCode -> Parser HogoCode
simpleCommand name constructor = constructor <$ caseInsensitiveString name

-- Updated non-parameter command parsers using the generic 'simpleCommand' function
penUp :: Parser HogoCode
penUp = simpleCommand "penup" PenUp

penDown :: Parser HogoCode
penDown = simpleCommand "pendown" PenDown

clearScreenP :: Parser HogoCode
clearScreenP = simpleCommand "clearscreen" ClearScreen

goHome :: Parser HogoCode
goHome = simpleCommand "home" GoHome

------------------------------------------------------------------------------------------------------------------------


----------------------------------------------Repeat functions---------------------------------------------------------

-- Generic parser for repeated patterns
repeatCommands :: Parser a -> Parser [a]
repeatCommands p = do
  _ <- char '['
  _ <- sc -- Consume any space or newline after '['
  cmds <- many (p <* sc) -- Parse many of 'p', separated by space consumer 'sc'
  _ <- char ']'
  return cmds

-- Updated repeatParser using the generic 'repeatCommands' function for handling the repeated pattern
repeatParser :: Parser HogoCode
repeatParser = do
  _ <- caseInsensitiveString "repeat"
  _ <- sc -- Consume any space before the number
  n <- L.decimal
  _ <- sc -- Consume any space before the '['
  cmds <- repeatCommands hogoCodeParser
  return $ Repeat n cmds

-------------------------------------------------------------------------------------------------------------------------


------------------------------------------Subroutine Functions-----------------------------------------------------------

parseDefineSub :: Parser HogoCode
parseDefineSub = do
  _ <- caseInsensitiveString "define"
  _ <- sc
  name <- some letterChar  -- Or adjust to your language's identifier rules
  _ <- sc
  body <- between (char '[') (char ']') (many (hogoCodeParser <* sc))
  return $ DefineSub name body

parseCallSub :: Parser HogoCode
parseCallSub = do
  _ <- caseInsensitiveString "call"
  _ <- sc
  name <- some letterChar  -- Or adjust to match your identifier rules
  return $ CallSub name

-------------------------------------------------------------------------------------------------------------------------


hogoCodeParser :: Parser HogoCode
hogoCodeParser = choice
  [ try goForward
  , try goBackward
  , try turnLeft
  , try turnRight
  , try penUp
  , try penDown
  , try clearScreenP
  , try goHome
  , try repeatParser
  , try parseDefineSub
  , try parseCallSub
  ]
  <* sc


parseHogo :: Parser HogoProgram
parseHogo = between sc eof (many (hogoCodeParser <* sc))
