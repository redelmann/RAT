
module Main where

import Rat
import RatByteCode
import RatParser
import Data.Maybe (fromJust)
import Data.Word
import System.Environment (getArgs)
import qualified Data.ByteString as B
import System.FilePath

-- | Compiles an Expression to a series of Instruction.
-- Converts every expression into instructions, then add
-- Start and Done instructions at the beginning and end,
-- make tail call optimisation and computes the jumps in terms of
-- word 32.
compile :: Expression -> [Instruction]
compile p = computeJumps $ makeTailCall $ [Start $ computeNeededStackSize p] ++ convert p [] ++ [Done]

-- | Converts an expression into a series of instructions.
-- The second argument is a series of instructions to append
-- at the end of each branch of the expression.
convert :: Expression -> [Instruction] -> [Instruction]
convert (Number n) ts = [Ldci n] ++ ts
convert (Boolean b) ts = [Ldcb b] ++ ts
convert (Binary c a b (Just t)) ts = as ++ bs ++ [op] ++ ts 
	where
		op = case (c, t) of
			("+", IntType) -> Plus
			("-", IntType) -> Minus
			("*", IntType) -> Times
			("/", IntType) -> Div
			("=", IntType) -> Equal
			("<", IntType) -> Less
			(">", IntType) -> Greater
			("+", FloatType) -> PlusFloat
			("-", FloatType) -> MinusFloat
			("*", FloatType) -> TimesFloat
			("/", FloatType) -> DivFloat
			("=", FloatType) -> EqualFloat
			("<", FloatType) -> LessFloat
			(">", FloatType) -> GreaterFloat
			("&", BoolType) -> And
			("|", BoolType) -> Or
		as = convert a []
		bs = convert b []
convert (Unary c a) ts = (convert a []) ++ [Not] ++ ts
convert (IfThenElse i y n) ts = is ++ [Jofr (2 + length ys)] ++ ys ++ [Gotor (1 + length ns)] ++ ns
	where
		is = convert i []
		ys = convert y ts
		ns = convert n ts
convert (Identifier x) ts = [Ld $ fromJust $ snd x] ++ ts
convert (Application f p) ts = convert f [] ++ convert p [] ++ [Call 1] ++ ts
convert (Fun _ e _ _) ts = [Ldf (computeNeededStackSize e)] ++ [Gotor (1 + length es)] ++ es ++ ts
	where
		es = convert e [Rtn]
convert (RecFun _ _ e _ _) ts = [Ldrf (computeNeededStackSize e)] ++ [Gotor (1 + length es)] ++ es ++ ts
	where
		es = convert e [Rtn]
convert (Print e (Just IntType)) ts = es ++ [PrintInt] ++ ts
	where
		es = convert e []
convert (Print e (Just BoolType)) ts = es ++ [PrintBool] ++ ts
	where
		es = convert e []
convert (Print e (Just FloatType)) ts = es ++ [PrintFloat] ++ ts
	where
		es = convert e []
convert (Get (Just IntType)) ts = [GetInt] ++ ts
convert (Get (Just BoolType)) ts = [GetBool] ++ ts
convert (Get (Just FloatType)) ts = [GetFloat] ++ ts
convert (FloatNumber f) ts = [Ldcf f] ++ ts
convert (Tuple es) ts = ess ++ [Crtp (length es)] ++ ts
	where
		ess = concat $ map (\e -> convert e []) es
convert (Projection e i) ts = es ++ [Proj i] ++ ts 
	where
		es = convert e []
convert Nil ts = [Crtp 0] ++ ts
convert (Cons h t) ts = hs ++ tas ++ [Crtp 2] ++ ts
	where
		hs = convert h []
		tas = convert t []
convert (IsNil e) ts = es ++ [Empty] ++ ts
	where
		es = convert e []
convert (Head e) ts = es ++ [Proj 0] ++ ts
	where
		es = convert e []
convert (Tail e) ts = es ++ [Proj 1] ++ ts
	where
		es = convert e []
convert (Cast e (Just IntType) FloatType) ts = es ++ [IntToFloat] ++ ts
	where
		es = convert e []	 
convert (Cast e (Just FloatType) IntType) ts = es ++ [FloatToInt] ++ ts
	where
		es = convert e []
convert (TryCatch (_, Just ex) tr ca) ts = [Try ex (1 + length trs + 2)] ++ trs ++ [EndTry, Gotor (1 + length cas)] ++ cas ++ ts
	where
		trs = convert tr []
		cas = convert ca []
convert (Throw (_, Just ex)) _ = [Raise ex]

-- | Changes the jumps in the instructions from
-- jumps in term of expression to jumps in term of
-- word 32.
computeJumps :: [Instruction] -> [Instruction]
computeJumps [] = []
computeJumps (x:xs) = case x of
	(Jofr i) -> (Jofr (sum $ map size $ take i (x:xs))) : computeJumps xs
	(Gotor i) -> (Gotor (sum $ map size $ take i (x:xs))) : computeJumps xs
	(Try e i) -> (Try e (sum $ map size $ take i (x:xs))) : computeJumps xs
	x -> x : computeJumps xs

-- | Make the tail call optimisation on a serie of instructions.
makeTailCall :: [Instruction] -> [Instruction]
makeTailCall (Call n : Rtn : ys) = (TailCall n) : NoOp : makeTailCall ys  -- NoOp is used so that their is the same number of
makeTailCall (y: ys) = y : makeTailCall ys                                -- instructions in case of jumps.
makeTailCall [] = []

-- | Utility function, to get the series of instructions from a source file.
getInstructions :: String -> IO [Instruction]
getInstructions f = do
	r <- getProgram f
	case r of
		Right e -> return $ compile e
		Left e -> return []

-- | Main method of the program.
-- Compiles source files given as program arguments.
main :: IO ()
main = do
	args <- getArgs
	sequence $ fmap (compileFile) args
	return ()

-- | Compile a source file given its name.
-- The output file has the same name, but
-- .steak as extension.
compileFile :: String -> IO ()
compileFile f = do
		r <- getProgram f
		let new = (replaceExtension f ".ratc")
		case r of
			Right e -> do
				B.writeFile new (B.pack $ toByteCode $ compile e)
				putStrLn $ "File " ++ f ++ " compiled to " ++ new
			Left e -> putStrLn $ "Impossible to compile " ++ f ++ " because : " ++ e ++ "."
