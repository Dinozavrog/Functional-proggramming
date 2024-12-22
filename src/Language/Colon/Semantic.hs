module Language.Colon.Semantic
  ( Value
  , ThrowException (..)
  , CaseInsensitiveString (..)
  , EvalState (..)
  , LoopFrame (..)
  , emptyEvalState
  , denote
  , builtInWords
  ) where

import Language.Colon.Syntax
import Prelude hiding (putChar, getChar)
import Control.Exception (Exception (..))
import Control.Monad (when)
import Control.Monad.Except (throwError, ExceptT, runExceptT)
import Control.Monad.State.Strict (get, put, State, runState)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Function ((&), fix)
import Data.Foldable (for_)
import Data.Functor (void)

-- Здесь используется функция runEvalM, которая запускает монаду EvalM, комбинируя работу с состоянием и обработку ошибок.
denote :: Code -> (EvalState -> Either ThrowException EvalState)
denote p initialState = do
  let (result, state) = runEvalM (program p) initialState
  case result of
    Right () -> pure state
    Left err -> throwError err

type Value = Int
type Stack = [Value]
type Pointer = Value

data LoopFrame = MkLoopFrame
  { index :: Value
  , limit :: Value }
  deriving stock (Eq, Show)

newtype CaseInsensitiveString = MkCaseInsensitiveString StringSource
  deriving newtype (Eq, Ord, Show)

caseInsensitiveString :: StringSource -> CaseInsensitiveString
caseInsensitiveString = MkCaseInsensitiveString . map Char.toLower

data ThrowException
  = ThrowDivisionByZero
  | ThrowLoopStackUnderFlow
  | ThrowUnknownWord StringSource
  | ThrowStackUnderFlow
  | ThrowUnknownMemorySlot Pointer
  | ThrowInvalidArgument Value

  deriving stock (Show, Eq)

instance Exception ThrowException where
  displayException = \case
    ThrowStackUnderFlow -> "Stack underflow"
    ThrowLoopStackUnderFlow -> "Loop stack overflow"
    ThrowUnknownWord name -> "Unknown name: " ++ name
    ThrowDivisionByZero -> "Division by zero"
    ThrowUnknownMemorySlot pointer -> "Unknown memory slot: " ++ prettyValue pointer
    ThrowInvalidArgument value -> "Invalid arqument" ++ prettyValue value

prettyValue :: Value -> String
prettyValue = show

data EvalState = MkEvalState
  { stack :: Stack
  , loops :: [LoopFrame]
  , input :: [Char]
  , output :: [Char]
  , definitions :: Map CaseInsensitiveString Code
  , variables :: Map CaseInsensitiveString Pointer
  , constants :: Map CaseInsensitiveString Value
  , memory :: Map Pointer Value }
  deriving stock (Eq, Show)

emptyEvalState :: EvalState
emptyEvalState = MkEvalState
  { stack = []
  , loops = []
  , input = []
  , output = []
  , definitions = Map.empty
  , variables = Map.empty
  , constants = Map.empty
  , memory = Map.empty }

-- Монада, которая комбинирует исключения (ExceptT) и состояние (State)
type EvalM = ExceptT ThrowException (State EvalState)

-- Пример использования монад для выполнения команды
command :: Command -> EvalM ()
command = \case

  Int x ->
    pushInt x

  Name name ->
    lookupWord name >>= \case
      WordIsBuiltIn action ->
        action
      WordIsDefinition body ->
        program body
      WordIsVariable pointer -> do
        pushPointer pointer
      WordIsConstant value -> do
        push value
      WordIsUnknown ->
        throwError (ThrowUnknownWord name)

  Str chars ->
    for_ chars putChar

  IfElse then_ else_ -> do
    condition <- popBool
    program (if condition then then_ else else_)

  DoILoop body -> do
    frame <- do
      index <- popInt
      limit <- popInt
      pure MkLoopFrame{index, limit}

    pushLoopFrame frame

    fix \loop -> do
        program body
        currentLoopStep >>= \case
          Done -> pure ()
          Repeat -> loop

    void popLoopFrame

  NameDef name body -> do
    defineWord name body

  VarString name -> do
    declareVariable name

  Const name -> do
    value <- pop
    declareConstant name value

program :: Code -> EvalM ()
program = mapM_ command

builtInWords :: Set StringSource
builtInWords = Map.keysSet builtInCommands
  & Set.map \(MkCaseInsensitiveString name) -> map Char.toLower name

builtInCommands :: Map CaseInsensitiveString (EvalM ())
builtInCommands = Map.fromList
  [ "+" ==> do
      y <- popInt
      x <- popInt
      pushInt (x + y)
  , "-" ==> do
      y <- popInt
      x <- popInt
      pushInt (x - y)
  , "*" ==> do
      y <- popInt
      x <- popInt
      pushInt (x * y)
  , "/" ==> do
      y <- popInt
      x <- popInt
      when (y == 0) do
        throwError ThrowDivisionByZero
      pushInt (x `div` y)
  , "DIGITMOD" ==> do
      y <- popInt
      x <- popInt
      when (y == 0) do
        throwError ThrowDivisionByZero
      pushInt (x `rem` y)
  , "DROP" ==> do
      _ <- pop
      pure ()
  , "SWAP" ==> do
      x <- pop
      y <- pop
      push x
      push y
  , "OVER" ==> do
      x <- pop
      y <- pop
      push y
      push x
      push y
  , "ROT" ==> do
      x <- pop
      y <- pop
      z <- pop
      push y
      push x
      push z

  , "=" ==> do
      y <- pop
      x <- pop
      pushBool (x == y)
  , "<" ==> do
      y <- popInt
      x <- popInt
      pushBool (x < y)
  , ">" ==> do
      y <- popInt
      x <- popInt
      pushBool (x > y)

  , "AND" ==> do
      y <- popBool
      x <- popBool
      pushBool (x && y)
  , "OR" ==> do
      y <- popBool
      x <- popBool
      pushBool (x || y)
  , "INVERT" ==> do
      x <- popBool
      pushBool (not x)

  , "." ==> do
      value <- pop
      printValue value
  , "CR" ==> do
      putChar '\n'
  , "EMIT" ==> do
      x <- popInt
      putChar (Char.chr x)
  , "KEY" ==> do
      char <- getChar
      pushInt (Char.ord char)

  , "I" ==> do
      MkLoopFrame{index} <- getLoopFrame 0
      pushInt index
  , "J" ==> do
      MkLoopFrame{index} <- getLoopFrame 1
      pushInt index
  , "K" ==> do
      MkLoopFrame{index} <- getLoopFrame 2
      pushInt index

  , "!" ==> do
      pointer <- popPointer
      value <- pop
      memorySet pointer value
  , "@" ==> do
      pointer <- popPointer
      value <- memoryGet pointer
      push value
  , "?" ==> do
      pointer <- popPointer
      value <- memoryGet pointer
      printValue value

  , "+!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      memorySet pointer (oldValue + value)
  , "-!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      memorySet pointer (oldValue - value)
  , "*!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      memorySet pointer (oldValue * value)
  , "/!" ==> do
      pointer <- popPointer
      value <- pop
      oldValue <- memoryGet pointer
      when (value == 0) do
        throwError ThrowDivisionByZero
      memorySet pointer (oldValue `div` value)
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

-- Helpers

runEvalM :: EvalM a -> EvalState -> (Either ThrowException a, EvalState)
runEvalM action state = action
  & runExceptT
  & flip runState state

putChar :: Char -> EvalM ()
putChar char = do
  MkEvalState{output, ..} <- get
  put MkEvalState{output = char : output, ..}

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

push :: Value -> EvalM ()
push value = do
  MkEvalState{stack, ..} <- get
  put MkEvalState{stack = value : stack, ..}

pop :: EvalM Value
pop = do
  MkEvalState{stack, ..} <- get
  case stack of
    [] -> throwError ThrowStackUnderFlow
    value : stack -> do
      put MkEvalState{stack, ..}
      pure value

pushInt :: Int -> EvalM ()
pushInt = push

popInt :: EvalM Int
popInt = pop

pushPointer :: Pointer -> EvalM ()
pushPointer = push

popPointer :: EvalM Pointer
popPointer = pop

pushBool :: Bool -> EvalM ()
pushBool = \case
  True -> pushInt -1
  False -> pushInt 0

popBool :: EvalM Bool
popBool = popInt >>= \case
  0 -> pure False
  _ -> pure True

pushLoopFrame :: LoopFrame -> EvalM ()
pushLoopFrame frame = do
  MkEvalState{loops, ..} <- get
  put MkEvalState{loops = frame : loops, ..}

popLoopFrame :: EvalM LoopFrame
popLoopFrame = do
  MkEvalState{loops, ..} <- get
  case loops of
    [] -> throwError ThrowLoopStackUnderFlow
    frame : frames -> do
      put MkEvalState{loops = frames, ..}
      pure frame

getLoopFrame :: Int -> EvalM LoopFrame
getLoopFrame i = do
  MkEvalState{loops} <- get
  case loops List.!? i of
    Just frame -> pure frame
    Nothing -> throwError ThrowLoopStackUnderFlow

data LoopResult = Done | Repeat

currentLoopStep :: EvalM LoopResult
currentLoopStep = do
  MkLoopFrame{index, ..} <- popLoopFrame
  let nextFrame = MkLoopFrame{index = index + 1, ..}
  pushLoopFrame nextFrame

  let MkLoopFrame{index, limit} = nextFrame
  if index >= limit
    then pure Done
    else pure Repeat

defineWord :: StringSource -> Code -> EvalM ()
defineWord name body = do
  MkEvalState{definitions, ..} <- get
  put MkEvalState{definitions = Map.insert (caseInsensitiveString name) body definitions, ..}

-- эти переменные и константы управляются через состояние EvalState:
declareVariable :: StringSource -> EvalM ()
declareVariable name = do
  MkEvalState{variables, memory, ..} <- get
  let pointer = Map.size variables + 1000
  put MkEvalState
    { variables = Map.insert (caseInsensitiveString name) pointer variables
    , memory = Map.insert pointer 0 memory
    , .. }

declareConstant :: StringSource -> Value -> EvalM ()
declareConstant name value = do
  MkEvalState{constants, ..} <- get
  put MkEvalState{constants = Map.insert (caseInsensitiveString name) value constants, ..}

memorySet :: Pointer -> Value -> EvalM ()
memorySet pointer value = do
  MkEvalState{memory, ..} <- get
  put MkEvalState{memory = Map.insert pointer value memory, ..}

memoryGet :: Pointer -> EvalM Value
memoryGet pointer = do
  MkEvalState{memory} <- get
  case Map.lookup pointer memory of
    Just value -> pure value
    Nothing -> throwError (ThrowUnknownMemorySlot pointer)

printValue :: Value -> EvalM ()
printValue value = do
  for_ (prettyValue value) putChar
  putChar ' '

data WordLookupResult
  = WordIsBuiltIn (EvalM ())
  | WordIsDefinition Code
  | WordIsVariable Pointer
  | WordIsConstant Value
  | WordIsUnknown

lookupWord :: StringSource -> EvalM WordLookupResult
lookupWord caseSensativeName = do
  let name = caseInsensitiveString caseSensativeName
  MkEvalState{constants, variables, definitions} <- get
  pure if
    | Just value <- Map.lookup name constants ->
        WordIsConstant value
    | Just pointer <- Map.lookup name variables ->
        WordIsVariable pointer
    | Just program <- Map.lookup name definitions ->
        WordIsDefinition program
    | Just action <- Map.lookup name builtInCommands ->
        WordIsBuiltIn action
    | otherwise ->
        WordIsUnknown
