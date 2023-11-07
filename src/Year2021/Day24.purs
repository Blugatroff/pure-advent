module Year2021.Day24 (day) where

import MeLude

import Control.Monad.State (StateT)
import Control.Monad.State as State
import Data.Bifunctor (bimap)
import Data.List as List
import Day (makeDay)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing
import Util (nonEmptyLines, parseInt, splitStringOnce, trace)

data Register = W | X | Y | Z

instance showRegister :: Show Register where
  show W = "w"
  show X = "x"
  show Y = "y"
  show Z = "z"

instance eqRegister :: Eq Register where
  eq W W = true
  eq X X = true
  eq Y Y = true
  eq Z Z = true
  eq _ _ = false

data Expr = Literal Int | Register Register

instance showExpr :: Show Expr where
  show (Literal v) = show v
  show (Register r) = show r

data Instr
  = InpInstr Register
  | AddInstr Register Expr
  | MulInstr Register Expr
  | DivInstr Register Expr
  | ModInstr Register Expr
  | EqlInstr Register Expr

instance showInstr :: Show Instr where
  show (InpInstr dst) = "input " <> show dst
  show (AddInstr dst src) = "add " <> show dst <> " " <> show src
  show (MulInstr dst src) = "mul " <> show dst <> " " <> show src
  show (DivInstr dst src) = "div " <> show dst <> " " <> show src
  show (ModInstr dst src) = "mod " <> show dst <> " " <> show src
  show (EqlInstr dst src) = "eql " <> show dst <> " " <> show src

registerParser =
  (W <$ Parsing.char 'w')
    <|> (X <$ Parsing.char 'x')
    <|> (Y <$ Parsing.char 'y')
    <|> (Z <$ Parsing.char 'z')

exprParser = (Literal <$> Parsing.intDecimal) <|> (Register <$> registerParser)

oneArgInstrParser :: forall a b. (b -> a) -> String -> Parser String b -> Parser String a
oneArgInstrParser constructor name argParser = do
  void $ Parsing.string name
  void $ Parsing.whiteSpace
  arg <- argParser
  pure $ constructor arg

twoArgInstrParser :: forall a b c. (b -> c -> a) -> String -> Parser String b -> Parser String c -> Parser String a
twoArgInstrParser constructor name arg1Parser arg2Parser = do
  void $ Parsing.string name
  void $ Parsing.whiteSpace
  arg1 <- arg1Parser
  void $ Parsing.whiteSpace
  arg2 <- arg2Parser
  pure $ constructor arg1 arg2

addParser :: Parser String Instr
addParser = twoArgInstrParser AddInstr "add" registerParser exprParser

inpParser :: Parser String Instr
inpParser = oneArgInstrParser InpInstr "inp" registerParser

mulParser :: Parser String Instr
mulParser = twoArgInstrParser MulInstr "mul" registerParser exprParser

divParser :: Parser String Instr
divParser = twoArgInstrParser DivInstr "div" registerParser exprParser

modParser :: Parser String Instr
modParser = twoArgInstrParser ModInstr "mod" registerParser exprParser

eqlParser :: Parser String Instr
eqlParser = twoArgInstrParser EqlInstr "eql" registerParser exprParser

instrParser :: Parser String Instr
instrParser = inpParser <|> addParser <|> mulParser <|> divParser <|> modParser <|> eqlParser

parser :: Parser String (List Instr)
parser = Parsing.sepEndBy instrParser Parsing.whiteSpace

data AluError = NoInput | DivByZero

instance showAluError :: Show AluError where
  show NoInput = "not enough inputs for inp instruction"
  show DivByZero = "division by zero"

parse = lmap show <$> flip Parsing.runParser parser
data Tree
  = GetInp
  | Let Register Tree Tree
  | Expr Expr
  | Add Tree Tree
  | Mul Tree Tree
  | Div Tree Tree
  | Mod Tree Tree
  | Eql Tree Tree

type Registers = { w :: Int, x :: Int, y :: Int, z :: Int }

emptyRegisters :: Registers
emptyRegisters = { w: 0, x: 0, y: 0, z: 0 }

get :: Register -> Registers -> Int
get W = _.w
get X = _.x
get Y = _.y
get Z = _.z

set :: Register -> Int -> Registers -> Registers
set W v registers = registers { w = v }
set X v registers = registers { x = v }
set Y v registers = registers { y = v }
set Z v registers = registers { z = v }

eval :: List Int -> Tree -> Either AluError Int
eval inputs tree = State.evalStateT (go emptyRegisters tree) inputs
  where
  go :: Registers -> Tree -> StateT (List Int) (Either AluError) Int
  go _ (Expr (Literal v)) = pure v
  go registers (Expr (Register reg)) = pure $ get reg registers
  go registers (Add a b) = go registers a >>= \a -> go registers b <#> add a
  go registers (Mul a b) = go registers a >>= \a -> go registers b <#> mul a
  go registers (Div a b) = go registers a >>= \a -> go registers b <#> div a
  go registers (Mod a b) = go registers a >>= \a -> go registers b <#> mod a
  go registers (Eql a b) = go registers a >>= \a -> go registers b <#> eq a >>> if _ then 1 else 0
  go registers (Let reg v body) = do
    v <- go registers v
    go (set reg v registers) body
  go _ GetInp = do
    inputs <- State.get
    case inputs of
      List.Nil -> State.lift $ Left NoInput
      (v : rest) -> do
        State.put rest
        pure v

instance showTree :: Show Tree where
  show GetInp = "inp"
  show (Let reg expr body) = "let " <> show reg <> " = " <> show expr <> " in\n" <> show body
  show (Expr expr) = show expr
  show (Add a b) = show a <> " + " <> show b
  show (Mul a b) = show a <> " * " <> show b
  show (Div a b) = show a <> " / " <> show b
  show (Mod a b) = show a <> " % " <> show b
  show (Eql a b) = show a <> " == " <> show b

buildTree :: List Instr -> Register -> Tree
buildTree instrs last = go instrs
  where
  go :: List Instr -> Tree
  go List.Nil = Expr (Register last)
  go (instr : rest) = case instr of
    InpInstr var -> Let var GetInp $ go rest
    AddInstr dst src -> Let dst (Add (Expr (Register dst)) (Expr src)) $ go rest
    MulInstr dst src -> Let dst (Mul (Expr (Register dst)) (Expr src)) $ go rest
    DivInstr dst src -> Let dst (Div (Expr (Register dst)) (Expr src)) $ go rest
    ModInstr dst src -> Let dst (Mod (Expr (Register dst)) (Expr src)) $ go rest
    EqlInstr dst src -> Let dst (Eql (Expr (Register dst)) (Expr src)) $ go rest

simplify :: Tree -> Tree
simplify tree = case tree of
  (Let reg val body) -> case usages reg body of
    1 | not (containsInp val) -> simplify $ inline reg (simplify val) body
    _ -> Let reg (simplify val) (simplify body)
    where
    usages :: Register -> Tree -> Int
    usages reg = case _ of
      Expr (Register r) | r == reg -> 1
      Expr _ -> 0
      Let r val body | r /= reg -> usages reg val + usages reg body
      Let _ val _ -> usages reg val
      Add a b -> usages reg a + usages reg b
      Mul a b -> usages reg a + usages reg b
      Div a b -> usages reg a + usages reg b
      Mod a b -> usages reg a + usages reg b
      Eql a b -> usages reg a + usages reg b
      GetInp -> 0

    containsInp :: Tree -> Boolean
    containsInp = case _ of
      GetInp -> true
      Expr _ -> false
      Let _ v body -> containsInp v || containsInp body
      Add a b -> containsInp a || containsInp b
      Mul a b -> containsInp a || containsInp b
      Div a b -> containsInp a || containsInp b
      Mod a b -> containsInp a || containsInp b
      Eql a b -> containsInp a || containsInp b

    inline :: Register -> Tree -> Tree -> Tree
    inline reg val = case _ of
      Expr (Register r) | r == reg -> val
      Expr v -> Expr v
      Let r v body | r /= reg -> Let r (inline reg val v) (inline reg val body)
      Let r v body -> Let r (inline reg val v) body
      Add a b -> Add (inline reg val a) (inline reg val b)
      Mul a b -> Mul (inline reg val a) (inline reg val b)
      Div a b -> Div (inline reg val a) (inline reg val b)
      Mod a b -> Mod (inline reg val a) (inline reg val b)
      Eql a b -> Eql (inline reg val a) (inline reg val b)
      GetInp -> GetInp
  (Add a b) -> Add (simplify a) (simplify b)
  (Mul a b) -> Mul (simplify a) (simplify b)
  (Div a b) -> Div (simplify a) (simplify b)
  (Mod a b) -> Mod (simplify a) (simplify b)
  (Eql a b) -> Eql (simplify a) (simplify b)
  (Expr expr) -> Expr expr
  GetInp -> GetInp

digits :: Int -> List Int
digits = List.reverse <<< go
  where
  go n | n < 10 = (n `mod` 10) : List.Nil
  go n = (n `mod` 10) : go (n `div` 10)

fillWithPrependedZeroes :: Int -> List Int -> List Int
fillWithPrependedZeroes targetLength l = do
  let ll = List.length l
  if targetLength <= ll then l
  else (const 0 <$> List.range 1 (targetLength - ll)) <> l

solvePartOne instrs = do
  let tree = simplify $ buildTree instrs Z
  let inputs = fillWithPrependedZeroes 14 (digits 0)
  Right $ show tree <> "\n" <> show (eval inputs tree)

solvePartTwo instrs = do
  bimap show show
    $ eval (fillWithPrependedZeroes 14 (digits 0))
    $ simplify (buildTree instrs Z)

day = makeDay parse solvePartOne solvePartTwo
