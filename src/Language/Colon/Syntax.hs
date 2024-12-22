module Language.Colon.Syntax
  ( StringSource
  , Command (..)
  , Code
  ) where

type StringSource = String

data Command
  
  = Int Int --Числовой литерал
  | Name StringSource --Вызов по имени (функция или переменная)
  | Str String -- Вывод текста
  | DoILoop Code -- Цикл
  | IfElse Code Code --Условие if else
  | NameDef StringSource Code -- Определение нового слова
  | VarString StringSource -- Объявление переменной
  | Const StringSource -- Объявление константы
  
  deriving stock (Eq, Show)

type Code = [Command]
