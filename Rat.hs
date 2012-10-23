
module Rat where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- | Defines possible expressions in the from language.
data Expression = Identifier (String, Maybe Int)
				| Number Int
				| FloatNumber Float
				| Boolean Bool
				| Fun String Expression Type (Maybe Type)
				| RecFun String String Expression Type (Maybe Type)
				| Application Expression Expression
				| IfThenElse Expression Expression Expression
				| Binary String Expression Expression (Maybe Type)
				| Unary String Expression
				| Print Expression (Maybe Type)
				| Get (Maybe Type)
				| Tuple [Expression]
				| Projection Expression Int
				| Cons Expression Expression
				| Nil
				| IsNil Expression
				| Head Expression
				| Tail Expression
				| Cast Expression (Maybe Type) Type
				| TryCatch (String, Maybe Int) Expression Expression
				| Throw (String, Maybe Int)
		deriving (Show)

-- | Defines possible types of the from language.
data Type = IntType
		  | BoolType
		  | FloatType
		  | FunType Type Type
		  | TupleType [Type]
		  | ListType Type
		deriving (Eq, Show)

-- | Computes the index of exceptions.
computeIndexException :: Expression -> Expression
computeIndexException = f m
	where
		m :: Map.Map String Int
		m = Map.fromList [("divisionByZero", 1), ("noRead", 2), ("endOfFile", 3)]

		g :: Int -> Maybe Int -> Maybe Int
		g i Nothing = Just i
		g _ (Just i) = Just i

		f :: Map.Map String Int -> Expression -> Expression
		f m (TryCatch (s, _) a b) = let m' = Map.alter (g $ 1 + Map.size m) s m in TryCatch (s, Map.lookup s m') (f m' a) (f m' b)
		f m (Throw (s, _)) = let m' = Map.alter (g $ 1 + Map.size m) s m in Throw (s, Map.lookup s m')
		f _ (Identifier x) = Identifier x
		f _ (Number n) = Number n
		f _ (FloatNumber f) = FloatNumber f
		f _ (Boolean b) = Boolean b
		f m (Fun v e t r) = Fun v (f m e) t r
		f m (RecFun n v e t r) = RecFun n v (f m e) t r
		f m (Application e p) = Application (f m e) (f m p)
		f m (IfThenElse i t e) = IfThenElse (f m i) (f m t) (f m e)
		f m (Binary h a b mt) = Binary h (f m a) (f m b) mt
		f m (Unary h a) = Unary h (f m a)
		f m (Print e t) = Print (f m e) t
		f m (Get t) = Get t
		f m (Tuple es) = Tuple (map (f m) es)
		f m (Projection e i) = Projection (f m e) i
		f _ Nil = Nil
		f m (Cons h t) = Cons (f m h) (f m t)
		f m (IsNil e) = IsNil (f m e)
		f m (Head e) = Head (f m e)
		f m (Tail e) = Tail (f m e)
		f m (Cast e mft tt) = Cast (f m e) mft tt

-- | Compute the De Bruijn indexes.
calculateStaticDistances :: Expression -> Expression
calculateStaticDistances = f 0 Map.empty
	where
		g :: [String] -> Int -> Map.Map String Int -> Map.Map String Int
		g [] _ m = m
		g (x:xs) i m = g xs (i+1) (Map.insert x i m)

		f :: Int -> Map.Map String Int -> Expression -> Expression
		f c m (Identifier (s, _)) = Identifier (s, fmap (\x -> c-x-1) $ Map.lookup s m)
		f _ _ (Number n) = Number n
		f _ _ (FloatNumber f) = FloatNumber f
		f _ _ (Boolean b) = Boolean b
		f c m (Fun v e t r) = Fun v (f c' m' e) t r
			where
				c' = c + 1
				m' = g [v] c m
		f c m (RecFun n v e t r) = RecFun n v (f c' m' e) t r
			where
				c' = c + 2
				m' = g [n,v] c m
		f c m (Application e p) = Application (f c m e) (f c m p)
		f c m (IfThenElse i t e) = IfThenElse (f c m i) (f c m t) (f c m e)
		f c m (Binary h a b mt) = Binary h (f c m a) (f c m b) mt
		f c m (Unary h a) = Unary h (f c m a)
		f c m (Print e t) = Print (f c m e) t
		f c m (Get t) = Get t
		f c m (Tuple es) = Tuple (map (f c m) es)
		f c m (Projection e i) = Projection (f c m e) i
		f _ _ Nil = Nil
		f c m (Cons h t) = Cons (f c m h) (f c m t)
		f c m (IsNil e) = IsNil (f c m e)
		f c m (Head e) = Head (f c m e)
		f c m (Tail e) = Tail (f c m e)
		f c m (Cast e mft tt) = Cast (f c m e) mft tt
		f c m (TryCatch i a b) = TryCatch i (f c m a) (f c m b)
		f _ _ (Throw x) = Throw x

-- | Verifies that the expression does not contain free variables.
checkNoFreeVariable :: Expression -> Bool
checkNoFreeVariable (Number _) = True
checkNoFreeVariable (Boolean _) = True
checkNoFreeVariable (FloatNumber _) = True
checkNoFreeVariable (Identifier (_, m)) = m /= Nothing
checkNoFreeVariable (Fun _ e _ _) = checkNoFreeVariable e
checkNoFreeVariable (RecFun _ _ e _ _) = checkNoFreeVariable e
checkNoFreeVariable (Application e p) = and $ map checkNoFreeVariable [e, p]
checkNoFreeVariable (IfThenElse i t e) = and $ map checkNoFreeVariable [i, t, e]
checkNoFreeVariable (Binary _ a b _) = checkNoFreeVariable a && checkNoFreeVariable b
checkNoFreeVariable (Unary _ a) = checkNoFreeVariable a
checkNoFreeVariable (Print e _) = checkNoFreeVariable e
checkNoFreeVariable (Get _) = True
checkNoFreeVariable (Tuple es) = and $ map checkNoFreeVariable es
checkNoFreeVariable (Projection e _) = checkNoFreeVariable e
checkNoFreeVariable (Cons h t) = and $ map checkNoFreeVariable [h, t]
checkNoFreeVariable Nil = True
checkNoFreeVariable (IsNil e) = checkNoFreeVariable e
checkNoFreeVariable (Head e) = checkNoFreeVariable e
checkNoFreeVariable (Tail e) = checkNoFreeVariable e
checkNoFreeVariable (Cast e _ _) = checkNoFreeVariable e
checkNoFreeVariable (TryCatch _ a b) = and $ map checkNoFreeVariable [a, b]
checkNoFreeVariable (Throw _) = True

-- | Type check the program, and infer types for
-- expressions like Nil, Get, Print, and Binary operations.
inferAndVerify :: Expression -> Either String Expression
inferAndVerify = fmap snd . f Map.empty Nothing
	where
		guard c s = if c then Right () else Left s 
		orElse :: Maybe a -> String -> Either String a
		orElse (Just x) _ = Right x
		orElse Nothing s = Left s

		f :: Map.Map String Type -> Maybe Type -> Expression -> Either String (Type, Expression)
		f m _ (Identifier (s, i)) = do
			a <- Map.lookup s m `orElse` ("No type for variable " ++ s)
			return $ (a, Identifier (s, i))
		f _ _ (Number n) = return (IntType, Number n)
		f _ _ (FloatNumber f) = return (FloatType, FloatNumber f)
		f _ _ (Boolean b) = return (BoolType, Boolean b)
		f m (Just (FunType _ ktt)) (Fun v e t Nothing) = do
			let mt = Just ktt
			let m' = Map.insert v t m
			(rt, e') <- f m' mt e
			guard (mt == Nothing || Just rt == mt) $ "Return type of function mismatch, expected : " ++ show (fromJust mt) ++ " got " ++ show rt
			return $ (FunType t rt, Fun v e' t (Just rt))
		f m _ (Fun v e t mt) = do
			let m' = Map.insert v t m
			(rt, e') <- f m' mt e
			guard (mt == Nothing || Just rt == mt) $ "Return type of function mismatch, expected : " ++ show (fromJust mt) ++ " got " ++ show rt
			return $ (FunType t rt, Fun v e' t (Just rt))
		f m (Just (FunType _ ktt)) (RecFun n v e t Nothing) = do
			let mt = Just ktt
			let m' = Map.alter (const $ fmap (FunType t) mt) n $ Map.insert v t m
			(rt, e') <- f m' mt e
			guard (mt == Nothing || Just rt == mt) $ "Return type of recursive function mismatch, expected : " ++ show (fromJust mt) ++ " got " ++ show rt
			return $ (FunType t rt, RecFun n v e' t (Just rt))
		f m _ (RecFun n v e t mt) = do
			let m' = Map.alter (const $ fmap (FunType t) mt) n $ Map.insert v t m
			(rt, e') <- f m' mt e
			guard (mt == Nothing || Just rt == mt) $ "Return type of recursive function mismatch, expected : " ++ show (fromJust mt) ++ " got " ++ show rt
			return $ (FunType t rt, RecFun n v e' t (Just rt))
		f m k (Application e p) = do
			(tf, e') <- f m Nothing e
			case tf of
				(FunType ft tt) -> do
					(tp, p') <- f m (Just ft) p
					guard (tp == ft) $ "Argument (" ++ show tp ++ ") don't match type of function (" ++ show ft ++ ")"
					return (tt, Application e' p')
				_ -> Left "Application of non-function"
		f m k (IfThenElse i t e) = do
			(ti, i') <- f m (Just BoolType) i
			guard (ti == BoolType) $ "Condition of if is not of type bool"
			(tt, t') <- f m k t
			(te, e') <- f m k e
			guard (tt == te) $ "If part and then part do not have the same type"
			return (tt, IfThenElse i' t' e')
		f m mt (Binary c a b _) = do
			let te = case c of
				"+" -> [IntType, FloatType]
				"-" -> [IntType, FloatType]
				"*" -> [IntType, FloatType]
				"/" -> [IntType, FloatType]
				"&" -> [BoolType]
				"|" -> [BoolType]
				"=" -> [IntType, FloatType]
				">" -> [IntType, FloatType]
				"<" -> [IntType, FloatType]
			(ta, a') <- f m mt a
			(tb, b') <- f m mt b
			guard (ta `elem` te && tb `elem` te && ta == tb) $ "Binary operation " ++ show c ++ " mistyped"
			let tr = if c `elem` ["+", "-", "*", "/"] then ta else BoolType
			return (tr, Binary c a' b' (Just ta))
		f m _ (Unary c a) = do
			let (te, tr) = case c of
				"\\" -> (BoolType, BoolType)
			(ta, a') <- f m (Just te) a
			guard (ta == te) $ "Unary operation " ++ show c ++ " mistyped"
			return (tr, Unary c a')
		f m k (Print e _) = do
			(t, e') <- f m k e
			guard (elem t [BoolType, IntType, FloatType]) "Can only print bool, float or int"
			return (t, Print e' (Just t))
		f m k (Get _) = do
			t <- k `orElse` "Can not infer type of ? command"
			guard (elem t [BoolType, IntType, FloatType]) "Can only enter bool, float or int"
			return (t, Get (Just t))
		f m k (Tuple es) = do
			let ks = case k of
				(Just (TupleType ts)) -> map Just ts
				_ -> repeat Nothing
			(ts, es') <- fmap unzip $ sequence $ map (\(k, e) -> f m k e) $ zip ks es
			guard (length es == length es') "Wrong number of elements in tuple"
			return (TupleType ts, Tuple es')
		f m k (Projection e i) = do
			(t, e') <- f m Nothing e
			case t of
				TupleType ts -> do
					guard (length ts > i) $ "Projection of element " ++ show (i + 1) ++ " on a tuple of size " ++ show (length ts)
					return (ts !! i, Projection e' i)
				_ -> Left "Projection of a non-tuple"
		f m k Nil = case k of
				Just (ListType t) -> return (ListType t, Nil)
				_ -> Left "Nil can not be typed correctly"
		f m k (Cons h t) = do
			let kh = case k of
				Just (ListType t) -> Just t
				_ -> Nothing
			(th, h') <- f m kh h
			(tt, t') <- f m (Just (ListType th)) t
			guard (tt == (ListType th)) "Head and tail of the list of different type"
			return (tt, Cons h' t')
		f m _ (IsNil e) = do
			(t, e') <- f m Nothing e
			case t of
				(ListType _) -> return (BoolType, IsNil e')
				_ -> Left "Tried to check if a non list is empty"
		f m k (Head e) = do
			let tk = fmap ListType k
			(tl, e') <- f m tk e
			case tl of
				(ListType t) -> return (t, Head e')
				_ -> Left "Head applied on a non list"
		f m k (Tail e) = do
			(tl, e') <- f m k e
			case tl of
				(ListType t) -> return (tl, Tail e')
				_ -> Left "Tail applied on a non list"
		f _ _ (Cast (Number n) _ FloatType) = return (FloatType, FloatNumber $ fromInteger (toInteger n)) 
		f _ _ (Cast (FloatNumber n) _ IntType) = return (IntType, Number $ floor n)
		f m k (Cast e mft tt) = do
			(t, e') <- f m mft e
			if tt == t then  -- No need for casting
				return (t, e')
			else
				case (t, tt) of
					(IntType, FloatType) -> return (FloatType, Cast e' (Just IntType) FloatType)
					(FloatType, IntType) -> return (IntType, Cast e' (Just FloatType) IntType)
					(TupleType fts, TupleType tts) -> do
						let es = case e' of
							(Tuple elems) -> elems
							_ -> map (\i -> Projection e' i) $ takeWhile (<length fts) [0..]
						guard (length fts == length tts) "Can not cast from tuples with different sizes"
						(ts', es') <- fmap unzip $ sequence $ map (\(ft, tt, e) -> f m (Just tt) (Cast e (Just ft) tt)) $ zip3 fts tts es
						guard (ts' == tts) "Cast of tuple unsuccessful." -- Should not happend, should fail beforehand.
						return (TupleType ts', Tuple es')
					_ -> Left $ "Can not cast from " ++ show t ++ " to " ++ show tt
		f m k (TryCatch i a b) = do
			(ta, a') <- f m k a
			(tb, b') <- f m k b
			guard (ta == tb) "Try and catch expressions must have the same type"
			return (ta, TryCatch i a' b')
		f _ (Just t) (Throw x) = return (t, Throw x)
		f _ Nothing (Throw _) = Left "Can not infer type of throw"

-- | Computes the needed stack size of an expression.
computeNeededStackSize :: Expression -> Int
computeNeededStackSize (Number _) = 1
computeNeededStackSize (FloatNumber _) = 1
computeNeededStackSize (Boolean _) = 1
computeNeededStackSize (Identifier _) = 1
computeNeededStackSize (Binary _ a b _) = maximum [computeNeededStackSize a, 1 + computeNeededStackSize b]
computeNeededStackSize (Unary _ a) = computeNeededStackSize a
computeNeededStackSize (Fun _ _ _ _) = 1
computeNeededStackSize (RecFun _ _ _ _ _) = 1
computeNeededStackSize (Application f e) =  maximum [computeNeededStackSize f, 1 + computeNeededStackSize e]
computeNeededStackSize (IfThenElse i t e) = maximum [computeNeededStackSize i, computeNeededStackSize t, computeNeededStackSize e]
computeNeededStackSize (Get _) = 1
computeNeededStackSize (Print e _) = computeNeededStackSize e
computeNeededStackSize (Tuple es) = maximum $ 1 : (zipWith (+) [0..] $ map computeNeededStackSize es)
computeNeededStackSize (Projection e _) = computeNeededStackSize e
computeNeededStackSize (Cons h t) = maximum [computeNeededStackSize h, 1 + computeNeededStackSize t]
computeNeededStackSize Nil = 1
computeNeededStackSize (IsNil e) = computeNeededStackSize e
computeNeededStackSize (Head e) = computeNeededStackSize e
computeNeededStackSize (Tail e) = computeNeededStackSize e
computeNeededStackSize (Cast e _ _) = computeNeededStackSize e
computeNeededStackSize (TryCatch _ t c) = maximum $ map computeNeededStackSize [t, c]
computeNeededStackSize (Throw _) = 0
