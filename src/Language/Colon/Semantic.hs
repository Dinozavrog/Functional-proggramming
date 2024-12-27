module Language.Colon.Semantic (
    Value,
    ThrowException(..),
    CaseInsensitiveString(..),
    EvalState(..),
    LoopFrame(..),
    emptyEvalState,
    denote,
    builtInWords
) where

import Language.Colon.Syntax
import Prelude hiding (putChar, getChar)
import Control.Exception (Exception(..))
import Control.Monad (when)
import Control.Monad.Except (throwError, ExceptT, runExceptT)
import Control.Monad.State.Strict (get, put, State, runState)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Function ((&), fix)
import Data.Foldable (for_)
import Data.Functor (void)

-- | Функция для выполнения программы в монаде EvalM
-- Комбинирует управление состоянием и обработку ошибок.
denote :: Code -> (EvalState -> Either ThrowException EvalState)
denote programCode initialState =
    let (result, state) = runEvalM (program programCode) initialState
    in case result of
        Right () -> pure state
        Left err -> throwError err

-- Типы

type Value = Int

type Stack = [Value]
type Pointer = Value

-- | Тип данных для работы с циклами
-- Хранит информацию об индексе и пределе цикла.
data LoopFrame = MkLoopFrame
    { index :: Value
    , limit :: Value
    } deriving stock (Eq, Show)

-- | Оболочка для нечувствительных к регистру строк
newtype CaseInsensitiveString = MkCaseInsensitiveString String
    deriving newtype (Eq, Ord, Show)

-- | Преобразование строки в нечувствительную к регистру
caseInsensitiveString :: String -> CaseInsensitiveString
caseInsensitiveString = MkCaseInsensitiveString . map Char.toLower

-- | Типы исключений для интерпретатора
-- Описывают возможные ошибки выполнения программы.
data ThrowException
    = ThrowDivisionByZero
    | ThrowLoopStackUnderFlow
    | ThrowUnknownWord String
    | ThrowStackUnderFlow
    | ThrowUnknownMemorySlot Pointer
    | ThrowInvalidArgument Value
    deriving stock (Show, Eq)

instance Exception ThrowException where
    displayException = \case
        ThrowStackUnderFlow -> "Stack underflow"
        ThrowLoopStackUnderFlow -> "Loop stack overflow"
        ThrowUnknownWord name -> "Unknown word: " ++ name
        ThrowDivisionByZero -> "Division by zero"
        ThrowUnknownMemorySlot pointer -> "Unknown memory slot: " ++ prettyValue pointer
        ThrowInvalidArgument value -> "Invalid argument: " ++ prettyValue value

-- | Форматирование значения для вывода
prettyValue :: Value -> String
prettyValue = show

-- | Состояние интерпретатора
-- Содержит информацию о стеке, циклах, вводе/выводе и т.д.
data EvalState = MkEvalState
    { stack :: Stack
    , loops :: [LoopFrame]
    , input :: [Char]
    , output :: [Char]
    , definitions :: Map CaseInsensitiveString Code
    , variables :: Map CaseInsensitiveString Pointer
    , constants :: Map CaseInsensitiveString Value
    , memory :: Map Pointer Value
    } deriving stock (Eq, Show)

-- | Начальное пустое состояние интерпретатора
emptyEvalState :: EvalState
emptyEvalState = MkEvalState
    { stack = []
    , loops = []
    , input = []
    , output = []
    , definitions = Map.empty
    , variables = Map.empty
    , constants = Map.empty
    , memory = Map.empty
    }

-- Монада для выполнения команд

type EvalM = ExceptT ThrowException (State EvalState)
-- | Выполнение команды
command :: Command -> EvalM ()
command = \case
    Int x -> pushInt x  -- Добавить число в стек
    Name name -> lookupWord name >>= \case
        WordIsBuiltIn action -> action  -- Выполнить встроенное слово
        WordIsDefinition body -> program body  -- Выполнить пользовательское слово
        WordIsVariable pointer -> pushPointer pointer  -- Записать указатель переменной
        WordIsConstant value -> push value  -- Записать константу
        WordIsUnknown -> throwError (ThrowUnknownWord name)  -- Ошибка: неизвестное слово
    Str chars -> for_ chars putChar  -- Вывести строку
    IfElse then_ else_ -> do
        condition <- popBool
        program (if condition then then_ else else_)  -- Выполнить ветку then или else
    DoILoop body -> do
        frame <- do
            index <- popInt
            limit <- popInt
            pure MkLoopFrame {index, limit}
        pushLoopFrame frame
        fix $ \loop -> do
            program body
            currentLoopStep >>= \case
                Done -> pure ()  -- Цикл завершён
                Repeat -> loop  -- Повторить цикл
        void popLoopFrame  -- Удалить рамку цикла
    NameDef name body -> defineWord name body  -- Определить новое слово
    VarString name -> declareVariable name  -- Объявить переменную
    Const name -> do
        value <- pop
        declareConstant name value  -- Объявить константу

-- | Выполнение программы (списка команд)
program :: Code -> EvalM ()
program = mapM_ command

-- Встроенные слова

builtInWords :: Set String
builtInWords = Map.keysSet builtInCommands
    & Set.map (\(MkCaseInsensitiveString name) -> map Char.toLower name)

builtInCommands :: Map CaseInsensitiveString (EvalM ())
builtInCommands = Map.fromList
  [ --Сложение
   "+" ==> do
      y <- popInt
      x <- popInt
      pushInt (x + y)
   --Вычитание
  , "-" ==> do
      y <- popInt
      x <- popInt
      pushInt (x - y)
  --Умножение
  , "*" ==> do
      y <- popInt
      x <- popInt
      pushInt (x * y)
    --Деление
  , "/" ==> do
      y <- popInt
      x <- popInt
      when (y == 0) do
        throwError ThrowDivisionByZero
      pushInt (x `div` y)
    --Остаток от деления
  , "DIGITMOD" ==> do
      y <- popInt
      x <- popInt
      when (y == 0) do
        throwError ThrowDivisionByZero
      pushInt (x `rem` y)
    --Удаление первого элемента стека
  , "DROP" ==> do
      _ <- pop
      pure ()
    --Свапает индексы двух первых элемнтов между собой
  , "SWAP" ==> do
      x <- pop
      y <- pop
      push x
      push y
    --Добавляет 2-ой элемент в стек
  , "OVER" ==> do
      x <- pop
      y <- pop
      push y
      push x
      push y
    --Было xyz -> yxz
  , "ROT" ==> do
      x <- pop
      y <- pop
      z <- pop
      push y
      push x
      push z
    --Проверка на равнество
  , "=" ==> do
      y <- pop
      x <- pop
      pushBool (x == y)
    --Меньше
  , "<" ==> do
      y <- popInt
      x <- popInt
      pushBool (x < y)
    --Больше
  , ">" ==> do
      y <- popInt
      x <- popInt
      pushBool (x > y)
    --Логическое И
  , "AND" ==> do
      y <- popBool
      x <- popBool
      pushBool (x && y)
    --Логическое ИЛИ
  , "OR" ==> do
      y <- popBool
      x <- popBool
      pushBool (x || y)
    --Значение меняется на противоположное
  , "INVERT" ==> do
      x <- popBool
      pushBool (not x)
    --Вывести значение
  , "PRINT" ==> do
      value <- pop
      printValue value
    --Печатает первый символ строки
  , "CR" ==> do
      putChar '\n'
    --Снимает число со стека проверяет его код и печатает этот символ
  , "EMIT" ==> do
      x <- popInt
      when (x < Char.ord minBound || x > Char.ord maxBound) do
        throwError (ThrowInvalidArgument x)
      putChar (Char.chr x)
    --Считывает символ с клавиатура и кладет его в стек
  , "KEY" ==> do
      char <- getChar
      pushInt (Char.ord char)
    --Возвращает текущий индекс итерации вложенных циклов
  , "I" ==> do
      MkLoopFrame{index} <- getLoopFrame 0
      pushInt index
    --Индекс второго цикла по вложенности
  , "J" ==> do
      MkLoopFrame{index} <- getLoopFrame 1
      pushInt index
    --Индекс третьего цикла по вложенности
  , "K" ==> do
      MkLoopFrame{index} <- getLoopFrame 2
      pushInt index
    --Сохраняет значение в памяти по указанному адресу
  , "!" ==> do
      pointer <- popPointer
      value <- pop
      memorySet pointer value
    --Извлекает значение из памяти по адресу и кладет его в стек
  , "@" ==> do
      pointer <- popPointer
      value <- memoryGet pointer
      push value
    --Извлекает значение из памяти по адресу и печатает его
  , "?" ==> do
      pointer <- popPointer
      value <- memoryGet pointer
      printValue value
    --Сложение из памяти на значение из стека результат сохранятся
  , "+!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      memorySet pointer (oldValue + value)
   --Вычитание из значения из памяти на значение из стека результат сохранятся
  , "-!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      memorySet pointer (oldValue - value)
   --Умножение значения из памяти на значение из стека результат сохранятся
  , "*!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      memorySet pointer (oldValue * value)
   --Деление значения из памяти на значение из стека результат сохранятся
  , "/!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      when (value == 0) do
        throwError ThrowDivisionByZero
      memorySet pointer (oldValue `div` value)
    --Остаток от деления значение из стека на значение из памяти результат сохранятся
  , "%!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      when (value == 0) do
        throwError ThrowDivisionByZero
      memorySet pointer (oldValue `rem` value)
  ]
  where
    name ==> action = (caseInsensitiveString name, action)

-- Вспомогательные функции

-- | Выполняет вычисление в монаде EvalM с начальным состоянием
runEvalM :: EvalM a -> EvalState -> (Either ThrowException a, EvalState)
runEvalM action state =
    action & runExceptT & flip runState state

-- | Добавляет символ в выходной поток
putChar :: Char -> EvalM ()
putChar char = do
    MkEvalState{output, ..} <- get
    put MkEvalState{output = char : output, ..}

-- | Читает символ из входного потока
getChar :: EvalM Char
getChar = do
    MkEvalState{input, ..} <- get
    case input of
        [] -> pure eof
        char : input -> do
            put MkEvalState{input, ..}
            pure char
  where
    eof = Char.chr 0

-- | Кладёт значение в стек
push :: Value -> EvalM ()
push value = do
    MkEvalState{stack, ..} <- get
    put MkEvalState{stack = value : stack, ..}

-- | Извлекает значение из стека
pop :: EvalM Value
pop = do
    MkEvalState{stack, ..} <- get
    case stack of
        [] -> throwError ThrowStackUnderFlow
        value : stack -> do
            put MkEvalState{stack, ..}
            pure value

-- | Кладёт целое число в стек
pushInt :: Int -> EvalM ()
pushInt = push

-- | Извлекает целое число из стека
popInt :: EvalM Int
popInt = pop

-- | Кладёт указатель в стек
pushPointer :: Pointer -> EvalM ()
pushPointer = push

-- | Извлекает указатель из стека
popPointer :: EvalM Pointer
popPointer = pop

-- | Кладёт логическое значение в стек
pushBool :: Bool -> EvalM ()
pushBool = \case
    True -> pushInt (-1)
    False -> pushInt 0

-- | Извлекает логическое значение из стека
popBool :: EvalM Bool
popBool = popInt >>= \case
    0 -> pure False
    _ -> pure True

-- | Добавляет рамку цикла в стек
pushLoopFrame :: LoopFrame -> EvalM ()
pushLoopFrame frame = do
    MkEvalState{loops, ..} <- get
    put MkEvalState{loops = frame : loops, ..}

-- | Удаляет рамку цикла из стека
popLoopFrame :: EvalM LoopFrame
popLoopFrame = do
    MkEvalState{loops, ..} <- get
    case loops of
        [] -> throwError ThrowLoopStackUnderFlow
        frame : frames -> do
            put MkEvalState{loops = frames, ..}
            pure frame

-- | Получает рамку цикла по индексу
getLoopFrame :: Int -> EvalM LoopFrame
getLoopFrame i = do
    MkEvalState{loops} <- get
    case loops List.!? i of
        Just frame -> pure frame
        Nothing -> throwError ThrowLoopStackUnderFlow

-- | Тип результата выполнения цикла
-- Done означает завершение, Repeat - продолжение
data LoopResult = Done | Repeat

-- | Выполняет текущий шаг цикла
currentLoopStep :: EvalM LoopResult
currentLoopStep = do
    MkLoopFrame{index, ..} <- popLoopFrame
    let nextFrame = MkLoopFrame{index = index + 1, ..}
    pushLoopFrame nextFrame
    let MkLoopFrame{index, limit} = nextFrame
    if index >= limit
        then pure Done
        else pure Repeat

-- | Определяет новое слово в словаре
defineWord :: String -> Code -> EvalM ()
defineWord name body = do
    MkEvalState{definitions, ..} <- get
    put MkEvalState{definitions = Map.insert (caseInsensitiveString name) body definitions, ..}

-- | Объявляет новую переменную
declareVariable :: String -> EvalM ()
declareVariable name = do
    MkEvalState{variables, memory, ..} <- get
    let pointer = Map.size variables + 1000
    put MkEvalState
        { variables = Map.insert (caseInsensitiveString name) pointer variables
        , memory = Map.insert pointer 0 memory
        , .. }

-- | Объявляет новую константу
declareConstant :: String -> Value -> EvalM ()
declareConstant name value = do
    MkEvalState{constants, ..} <- get
    put MkEvalState{constants = Map.insert (caseInsensitiveString name) value constants, ..}

-- | Устанавливает значение в память
memorySet :: Pointer -> Value -> EvalM ()
memorySet pointer value = do
    MkEvalState{memory, ..} <- get
    put MkEvalState{memory = Map.insert pointer value memory, ..}
-- | Извлекает значение из памяти
memoryGet :: Pointer -> EvalM Value
memoryGet pointer = do
    MkEvalState{memory} <- get
    case Map.lookup pointer memory of
        Just value -> pure value
        Nothing -> throwError (ThrowUnknownMemorySlot pointer)

-- | Печатает значение
printValue :: Value -> EvalM ()
printValue value = do
    for_ (prettyValue value) putChar
    putChar ' '

-- | Результат поиска слова
-- Может быть встроенным, определённым, переменной, константой или неизвестным
data WordLookupResult
    = WordIsBuiltIn (EvalM ())
    | WordIsDefinition Code
    | WordIsVariable Pointer
    | WordIsConstant Value
    | WordIsUnknown

-- | Ищет слово в словаре
lookupWord :: String -> EvalM WordLookupResult
lookupWord caseSensitiveName = do
    let name = caseInsensitiveString caseSensitiveName
    MkEvalState{constants, variables, definitions} <- get
    pure $ if
        | Just value <- Map.lookup name constants -> WordIsConstant value
        | Just pointer <- Map.lookup name variables -> WordIsVariable pointer
        | Just program <- Map.lookup name definitions -> WordIsDefinition program
        | Just action <- Map.lookup name builtInCommands -> WordIsBuiltIn action
        | otherwise -> WordIsUnknown

