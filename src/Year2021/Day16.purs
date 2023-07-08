module Year2021.Day16 (partOne, partTwo) where

import MeLude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (get, gets, put, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.List as List
import Data.String as String
import JS.BigInt (BigInt, shl, or, toInt)

type Bit = Boolean
newtype Bits = Bits (List Bit)

derive instance newtypeBits :: Newtype Bits _

showBit :: Bit -> String
showBit true = "1"
showBit false = "0"

instance showBits :: Show Bits where
  show (Bits bits) = String.joinWith "" $ Array.fromFoldable $ map showBit bits

instance semigroupBits :: Semigroup Bits where
  append (Bits a) (Bits b) = Bits $ append a b

instance monoidBits :: Monoid Bits where
  mempty = Bits mempty

hexToBits :: Char -> Either String Bits
hexToBits '0' = Right $ Bits $ List.fromFoldable [ false, false, false, false ]
hexToBits '1' = Right $ Bits $ List.fromFoldable [ false, false, false, true ]
hexToBits '2' = Right $ Bits $ List.fromFoldable [ false, false, true, false ]
hexToBits '3' = Right $ Bits $ List.fromFoldable [ false, false, true, true ]
hexToBits '4' = Right $ Bits $ List.fromFoldable [ false, true, false, false ]
hexToBits '5' = Right $ Bits $ List.fromFoldable [ false, true, false, true ]
hexToBits '6' = Right $ Bits $ List.fromFoldable [ false, true, true, false ]
hexToBits '7' = Right $ Bits $ List.fromFoldable [ false, true, true, true ]
hexToBits '8' = Right $ Bits $ List.fromFoldable [ true, false, false, false ]
hexToBits '9' = Right $ Bits $ List.fromFoldable [ true, false, false, true ]
hexToBits 'A' = Right $ Bits $ List.fromFoldable [ true, false, true, false ]
hexToBits 'B' = Right $ Bits $ List.fromFoldable [ true, false, true, true ]
hexToBits 'C' = Right $ Bits $ List.fromFoldable [ true, true, false, false ]
hexToBits 'D' = Right $ Bits $ List.fromFoldable [ true, true, false, true ]
hexToBits 'E' = Right $ Bits $ List.fromFoldable [ true, true, true, false ]
hexToBits 'F' = Right $ Bits $ List.fromFoldable [ true, true, true, true ]
hexToBits c = Left $ "'" <> show c <> "' is not a hexadecimal digit"

hexStringToBits :: String -> Either String Bits
hexStringToBits = toCharArray >>> traverse hexToBits >>> map fold

parse :: String -> Either String Bits
parse = String.trim >>> hexStringToBits

data OperatorType = Sum | Product | Minimum | Maximum | Greater | Less | Equal

data Packet = Literal Int BigInt | Operator OperatorType Int (Array Packet)

takeN :: Int -> State Bits Bits
takeN n = do
  bits <- get <#> unwrap <#> List.take n <#> Bits
  modify_ $ Bits <<< (List.drop n <<< unwrap)
  pure bits

takeOne :: State Bits (Maybe Bit)
takeOne = takeN 1 <#> unwrap >>> case _ of
  (b : _) -> Just b
  List.Nil -> Nothing

parseLiteralBits :: ExceptT String (State Bits) Bits
parseLiteralBits = do
  ty <- lift takeOne
  case ty of
    Just false -> lift $ takeN 4
    Just true -> do
      b <- lift $ takeN 4
      parseLiteralBits <#> (b <> _)
    Nothing -> throwError "expected literal type bit"

bitsToInt :: Bits -> BigInt
bitsToInt b = foldl f zero $ unwrap b
  where
  f :: BigInt -> Bit -> BigInt
  f n false = n `shl` one
  f n true = (n `shl` one) `or` one

parseLiteral :: Int -> ExceptT String (State Bits) Packet
parseLiteral version = Literal version <<< bitsToInt <$> parseLiteralBits

parseOperatorA :: OperatorType -> Int -> ExceptT String (State Bits) Packet
parseOperatorA opType version = do
  len <- lift $ takeN 11
  subPackets <- sequence $ Array.replicate (fromMaybe 0 $ toInt $ bitsToInt len) parsePacket
  pure $ Operator opType version subPackets

parseManyPackets :: ExceptT String (State Bits) (List Packet)
parseManyPackets = do
  bits <- gets unwrap
  case bits of
    List.Nil -> pure List.Nil
    _ -> do
      packet <- parsePacket
      map (packet : _) parseManyPackets

parseOperatorB :: OperatorType -> Int -> ExceptT String (State Bits) Packet
parseOperatorB opType version = do
  len <- lift $ map (fromMaybe 0 <<< toInt <<< bitsToInt) $ takeN 15
  bits <- get
  put (Bits $ List.take len $ unwrap bits)
  subPackets <- parseManyPackets
  modify_ \rest -> Bits $ unwrap rest <> List.drop len (unwrap bits)
  pure $ Operator opType version $ Array.fromFoldable subPackets

parseOperator :: OperatorType -> Int -> ExceptT String (State Bits) Packet
parseOperator opType version =
  lift takeOne >>= case _ of
    Just true -> parseOperatorA opType version
    Just false -> parseOperatorB opType version
    Nothing -> throwError "Expected length type ID"

parseOperatorType :: ExceptT String (State Bits) (Maybe OperatorType)
parseOperatorType = do
  ty <- lift $ map bitsToInt $ takeN 3
  bits <- get
  case toInt ty of
    Nothing -> throwError $ "operator type is too long: " <> show bits
    Just ty -> case ty of
      0 -> pure $ Just Sum
      1 -> pure $ Just Product
      2 -> pure $ Just Minimum
      3 -> pure $ Just Maximum
      4 -> pure Nothing
      5 -> pure $ Just Greater
      6 -> pure $ Just Less
      7 -> pure $ Just Equal
      _ -> throwError $ "failed to parse operator type: " <> show bits

parsePacket :: ExceptT String (State Bits) Packet
parsePacket = do
  version <- lift $ map bitsToInt $ takeN 3
  version <- case toInt version of
    Nothing -> throwError $ "packet version is too big: " <> show version
    Just version -> pure version
  parseOperatorType >>= case _ of
    Nothing -> parseLiteral version
    Just opType -> parseOperator opType version

evalPacket :: Packet -> BigInt
evalPacket (Literal _ value) = value
evalPacket (Operator opType _ subs) = map evalPacket subs # case opType of
  Sum -> sum
  Product -> product
  Minimum -> fromMaybe zero <<< minimum
  Maximum -> fromMaybe zero <<< maximum
  Greater -> binary greaterThan
  Less -> binary lessThan
  Equal -> binary eq
  where
  binary :: forall a. Semiring a => (a -> a -> Boolean) -> Array a -> a
  binary f [ a, b ] = if a `f` b then one else zero
  binary _ _ = zero

packetVersionSum :: Packet -> Int
packetVersionSum (Literal version _) = version
packetVersionSum (Operator _ version subPackets) = version + (sum $ map packetVersionSum $ subPackets)

solvePartOne :: Bits -> String |? Int
solvePartOne bits = packetVersionSum <$> evalState (runExceptT parsePacket) bits

solvePartTwo :: Bits -> String |? BigInt
solvePartTwo bits = evalPacket <$> evalState (runExceptT parsePacket) bits

partOne :: String -> String |? String
partOne input = parse input >>= solvePartOne <#> show

partTwo :: String -> String |? String
partTwo input = parse input >>= solvePartTwo <#> show

