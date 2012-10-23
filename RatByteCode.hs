
{-# LANGUAGE FlexibleContexts #-}

module RatByteCode where

import Data.Word
import Data.Bits
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

-- | List of possible instructions in the to language.
data Instruction = Done
				 | Ldci Int
				 | Ldcb Bool
				 | Ldcf Float
				 | Ld Int
				 | Ldf Int
				 | Ldrf Int
				 | Plus
				 | Minus
				 | Times
				 | Div
				 | And
				 | Or
				 | Not
				 | Less
				 | Greater
				 | Equal
				 | PlusFloat
				 | MinusFloat
				 | TimesFloat
				 | DivFloat
				 | LessFloat
				 | GreaterFloat
				 | EqualFloat
				 | Jofr Int
				 | Gotor Int
				 | Call Int
				 | Rtn
				 | Start Int
				 | TailCall Int
				 | PrintInt
				 | PrintBool
				 | PrintFloat
				 | GetInt
				 | GetBool
				 | GetFloat
				 | NoOp
				 | Crtp Int
				 | Proj Int
				 | Empty
				 | IntToFloat
				 | FloatToInt
				 | Try Int Int
				 | EndTry
				 | Raise Int
	deriving (Eq, Show)

-- | Gets the opcode for a specific instruction.
opcode :: Instruction -> Word32
opcode Done = 0
opcode (Ldci _) = 1
opcode (Ldcb _) = 2
opcode (Ld _) = 3
opcode (Ldf _) = 4
opcode (Ldrf _) = 5
opcode Plus = 6
opcode Minus = 7
opcode Times = 8
opcode Div = 9
opcode And = 10
opcode Or = 11
opcode Not = 12
opcode Less = 13
opcode Greater = 14
opcode Equal = 15
opcode (Jofr _) = 16
opcode (Gotor _) = 17
opcode (Call _) = 18
opcode Rtn = 19
opcode (Start _) = 20
opcode (TailCall _) = 21
opcode PrintInt = 22
opcode PrintBool = 23
opcode GetInt = 24
opcode GetBool = 25
opcode (Ldcf _) = 26
opcode PlusFloat = 27
opcode MinusFloat = 28
opcode TimesFloat = 29
opcode DivFloat = 30
opcode LessFloat = 31
opcode GreaterFloat = 32
opcode EqualFloat = 33
opcode GetFloat = 34
opcode PrintFloat = 35
opcode (Crtp _) = 36
opcode (Proj _) = 37
opcode Empty = 38
opcode IntToFloat = 39
opcode FloatToInt = 40
opcode (Try _ _) = 41
opcode EndTry = 42
opcode (Raise _) = 43

-- | Gets the size (in word 32) of an instruction.
size :: Instruction -> Int
size Done = 1
size (Ldci _) = 2
size (Ldcb _) = 2
size (Ldcf _) = 2
size (Ld _) = 2
size (Ldf _) = 2
size (Ldrf _) = 2
size Plus = 1
size Minus = 1
size Times = 1
size Div = 1
size And = 1
size Or = 1
size Not = 1
size Less = 1
size Greater = 1
size Equal = 1
size (Jofr _) = 2
size (Gotor _) = 2
size (Call _) = 2
size Rtn = 1
size (Start _) = 2
size (TailCall _) = 2
size PrintInt = 1
size PrintBool = 1
size GetInt = 1
size GetBool = 1
size NoOp = 0
size PlusFloat = 1
size MinusFloat = 1
size TimesFloat = 1
size DivFloat = 1
size LessFloat = 1
size GreaterFloat = 1
size EqualFloat = 1
size PrintFloat = 1
size GetFloat = 1
size (Crtp _) = 2
size (Proj _) = 2
size Empty = 1
size IntToFloat = 1
size FloatToInt = 1
size (Try _ _) = 3
size EndTry = 1
size (Raise _) = 2

-- | Converts an instruction a list of word 32.
toWords :: Instruction -> [Word32]
toWords NoOp = []
toWords x@(Ldcf f) = [opcode x, (floatToWord) f]
toWords x = opcode x : map fromIntegral (case x of
	(Ldci i) -> [i]
	(Ldcb b) -> [if b then 1 else 0]
	(Ld i) -> [i]
	(Jofr k) -> [k]
	(Gotor k) -> [k]
	(Call n) -> [n]
	(Start n) -> [n]
	(Ldf s) -> [s]
	(Ldrf s) -> [s]
	(TailCall n) -> [n]
	(Crtp n) -> [n]
	(Proj n) -> [n]
	(Try e k) -> [e, k]
	(Raise e) -> [e]
	_ -> [])

-- | Converts a Float into a Word32.
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

{-# INLINE cast #-}
-- | Casts.
cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-- | Converts from a Word8 to a list of bytes.
octets :: Word32 -> [Word8]
octets w = map fromIntegral [w, w `shiftR` 8, w `shiftR` 16, w `shiftR` 24]

-- | Converts a list of instructions to a list of bytes.
toByteCode :: [Instruction] -> [Word8]
toByteCode is = do
	i <- is
	w <- toWords i
	octets w
