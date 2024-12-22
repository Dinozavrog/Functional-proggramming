module Language.Colon.Parsing (ParseError(..), parse) where

import Language.Colon.Syntax
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec (Parsec, choice, try, manyTill, oneOf, (<?>), eof)
import Text.Megaparsec.Char (char, string, string', alphaNumChar, space1)
import Text.Megaparsec.Char.Lexer (charLiteral, space, lexeme, skipBlockCommentNested)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Void (Void)
import Control.Monad (void)
import Control.Applicative (empty, many, optional, some)
import Control.Exception (Exception(..))
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import Text.Megaparsec.Char (digitChar)

-- Основная функция для парсинга исходного кода
-- Возвращает либо ошибку, либо успешно обработанную программу
parse :: String -> Either ParseError Code
parse input =
  case Megaparsec.parse (fullProgramParser <* whitespace <* eof) "" input of
    Left err      -> throwError (SyntaxError (errorBundlePretty err))
    Right commands -> pure commands

-- Тип данных для описания ошибок, возникших при парсинге
newtype ParseError = SyntaxError String
  deriving stock (Show)

-- Реализация обработки исключений для ошибок парсинга
instance Exception ParseError where
    displayException (SyntaxError details) = "Синтаксическая ошибка: " <> details

-- Упрощённое имя для парсера, не учитывающего специфические ошибки
type SimpleParser = Parsec Void String

-- Парсер, распознающий отдельные команды
commandParser :: SimpleParser Command
commandParser = choice
  [ parseIntegerValue, parseStringValue, try parseConditionalElse, parseConditionalThen
  , parseLoopStructure, parseDefinition, parseVariableCommand, parseConstantCommand
  , parseIdentifierCommand ]

-- Парсер для полного набора команд в программе
fullProgramParser :: SimpleParser Code
fullProgramParser = many commandParser

-- Парсер для работы с целыми числами
parseIntegerValue :: SimpleParser Command
parseIntegerValue = withToken $ do
    sign <- optional (char '-')
    let factor = if isJust sign then -1 else 1
    number <- some digitChar
    pure $ Int (read number * factor)

-- Парсер для строковых данных
parseStringValue :: SimpleParser Command
parseStringValue = withToken $ do
    void (string ".\" ")
    content <- manyTill charLiteral (char '"')
    pure $ Str content

-- Парсер условных конструкций с "if-else"
parseConditionalElse :: SimpleParser Command
parseConditionalElse = do
    parseKeyword "if"
    thenBranch <- manyTill commandParser (parseKeyword "else")
    elseBranch <- manyTill commandParser (parseKeyword "then")
    pure $ IfElse thenBranch elseBranch

-- Парсер условных конструкций без "else"
parseConditionalThen :: SimpleParser Command
parseConditionalThen = do
    parseKeyword "if"
    thenBranch <- manyTill commandParser (parseKeyword "then")
    pure $ IfElse thenBranch []

-- Парсер для цикла "do-loop"
parseLoopStructure :: SimpleParser Command
parseLoopStructure = do
    parseKeyword "DO"
    loopBody <- manyTill commandParser (parseKeyword "looper")
    pure $ DoILoop loopBody

-- Парсер для идентификаторов
parseIdentifierCommand :: SimpleParser Command
parseIdentifierCommand = withToken $ Name <$> identifierParser

-- Парсер для определения новых имён
parseDefinition :: SimpleParser Command
parseDefinition = do
    parseKeyword ":"
    name <- withToken identifierParser <?> "имя определения"
    body <- manyTill commandParser (parseKeyword ";") <?> "тело определения"
    pure $ NameDef name body

-- Парсер для переменных
parseVariableCommand :: SimpleParser Command
parseVariableCommand = do
    parseKeyword "var"
    varName <- withToken identifierParser
    pure $ VarString varName

-- Парсер для констант
parseConstantCommand :: SimpleParser Command
parseConstantCommand = do
    parseKeyword "const_val"
    constName <- withToken identifierParser
    pure $ Const constName

-- Вспомогательный парсер для идентификаторов
identifierParser :: SimpleParser String
identifierParser = some (choice [alphaNumChar, oneOf "+-*/<>=&|.?!@"])
-- Лексер для применения пробельных символов
withToken :: SimpleParser a -> SimpleParser a
withToken = lexeme whitespace

-- Парсер для обработки пробелов и комментариев
whitespace :: SimpleParser ()
whitespace = space space1 empty (skipBlockCommentNested "(" ")")

-- Парсер ключевых слов
parseKeyword :: String -> SimpleParser ()
parseKeyword kw = void (withToken (string' kw))
