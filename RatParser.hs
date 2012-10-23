
module RatParser where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Char hiding (spaces)
import Text.Parsec.Expr
import Rat
import Control.Monad (guard)

-- | Keywords that identifiers can not take.
keywords :: [String]
keywords = ["if", "fun", "then", "def", "rec", "end", "let", "int", "bool", "float", "head", "tail", "empty", "try", "catch", "throw"]

-- | Parser for an entire program.
program :: Parser Expression
program = do
	skipMany space
	e <- expression
	skipMany space
	eof
	return e

-- | Parser for at least 1 space.
spaces :: Parser ()
spaces = skipMany1 space

-- | Parser expressions.
expression :: Parser Expression
expression = try operation <|> nonoperation

-- | Parser for non-operations.
nonoperation :: Parser Expression
nonoperation = try headList <|>
			   try tailList <|>
			   try empty <|>
			   try list <|>
			   try getval <|> 
			   try tuple <|>
			   try defrecBinding <|> 
			   try defBinding <|> 
			   try letBinding <|> 
			   try float <|> 
			   try number <|> 
			   try boolean <|> 
			   try ifThenElse <|>
			   try tryCatch <|>
			   try throw <|>
			   try fun <|> 
			   try recfun <|> 
			   try identifier <|>
			   inParens expression

-- | Table of operation.
optable = [
			[Postfix appl],
			[Postfix projection],
			[Prefix cast],
			[Prefix printex],
			[Prefix (unary "\\")],
			[Infix (binary "*") AssocLeft, Infix (binary "/") AssocLeft],
			[Infix (binary "+") AssocLeft, Infix (binary "-") AssocLeft],
			[Infix (binary "<") AssocLeft, Infix (binary ">") AssocLeft, Infix (binary "=") AssocLeft],
			[Infix (binary "&") AssocLeft, Infix (binary "|") AssocLeft],
			[Infix cons AssocRight]
		]

-- | Parser for the cast operation.
cast :: Parser (Expression -> Expression)
cast = try $ do
	char '('
	t <- types
	char ')'
	return (\e -> Cast e Nothing t)

-- | Parser for the application operation.
appl :: Parser (Expression -> Expression)
appl = try $ do
	vs <- fmap concat $ try $ many1 elems
	return $ (\f -> foldl (\x y -> Application x y) f vs)
	where
		elems = do
			char '('
			skipMany space
			es <- try $ sepBy1 expression $ try (skipMany space >> char ',' >> skipMany space)
			skipMany space
			char ')'
			return es

-- | Parser for the projection operation.
projection :: Parser (Expression -> Expression)
projection = try $ do
	string "._"
	i <- oneOf $ concat $ map show [1..9]
	is <- many digit
	return $ (\e -> Projection e ((read (i:is)) - 1))

-- | Parser for the list construction operation.
cons :: Parser (Expression -> Expression -> Expression)
cons = try $ do
	many space
	string "::"
	many space
	return (\h t -> Cons h t)

-- | Parser of lists in the form [a, b, c].
list :: Parser Expression
list = do
	char '['
	many space
	es <- sepBy expression $ try (many space >> char ',' >> many space)
	many space
	char ']'
	let e = foldr (\h t -> Cons h t) Nil es
	return e

-- | Parser for operations.
operation :: Parser Expression
operation = buildExpressionParser optable (try nonoperation <|> inParens operation)

-- | Returns the same parser in parentheses.
inParens :: Parser a -> Parser a
inParens p = try $ do
	char '('
	optional spaces
	r <- p
	optional spaces
	char ')'
	return r

-- | Binary operation.
binary :: String -> Parser (Expression -> Expression -> Expression)
binary c = try $ do
	optional spaces
	string c
	optional spaces
	return $ (\a b -> Binary c a b Nothing)

-- | Unary operation.
unary :: String -> Parser (Expression -> Expression)
unary c = try $ do
	optional spaces
	string c
	optional spaces
	return $ Unary c

-- | Parser for print operation.
printex :: Parser (Expression -> Expression)
printex = try $ do
	optional spaces
	char '!'
	optional spaces
	return $ \x -> Print x Nothing

-- | Parser for a Get expression.
getval :: Parser Expression
getval = char '?' >> return (Get Nothing)

-- | Parser for identifier strings.
identifierString :: Parser String
identifierString = do
	h <- letter
	i <- many alphaNum
	guard (not $ h:i `elem` keywords)
	return $ h:i

-- | Parser for identifiers.
identifier :: Parser Expression
identifier = do
	s <- identifierString
	return $ Identifier (s, Nothing)

-- | Parser for constant number expressions.
number :: Parser Expression
number = do
	f <- option id (char '-' >> return negate)
	xs <- many1 digit
	return $ Number $ f $ read xs

-- | Parser for constant float expressions.
float :: Parser Expression
float = do
	f <- option id (char '-' >> return negate)
	xs <- many1 digit
	char '.'
	ys <- many digit
	return $ FloatNumber $ f $ read $ xs ++ "." ++ ys ++ "0"

-- | Parser for constant boolean expressions.
boolean :: Parser Expression
boolean = do
	xs <- string "true" <|> string "false"
	return $ Boolean $ xs == "true"

-- | Parser for tuple expressions.
tuple :: Parser Expression
tuple = do
	char '{'
	many space
	es <- sepBy expression $ try (many space >> char ',' >> many space)
	many space
	char '}'
	return $ Tuple es

-- | Parser for Head expressions.
headList :: Parser Expression
headList = do
	string "head"
	char '('
	many space
	e <- expression
	many space
	char ')'
	return $ Head e

-- | Parser for Tail expressions.
tailList :: Parser Expression
tailList = do
	string "tail"
	char '('
	many space
	e <- expression
	many space
	char ')'
	return $ Tail e

-- | Parser for IsNil expressions.
empty :: Parser Expression
empty = do
	string "empty"
	char '('
	many space
	e <- expression
	many space
	char ')'
	return $ (IsNil e)

-- | Parser for IfThenElse expressions.
ifThenElse :: Parser Expression
ifThenElse = do
	string "if"
	skipMany space
	i <- inParens expression
	skipMany space
	char '{'
	skipMany space
	t <- expression
	skipMany space
	char '}'
	skipMany space
	string "else"
	skipMany space
	char '{'
	skipMany space
	e <- expression
	skipMany space
	char '}'
	return $ IfThenElse i t e

-- | Parser for TryCatch expressions.
tryCatch :: Parser Expression
tryCatch = do
	string "try"
	skipMany space
	char '{'
	skipMany space
	t <- expression
	skipMany space
	char '}'
	skipMany space
	string "catch"
	skipMany space
	char '('
	skipMany space
	e <- many1 letter
	skipMany space
	char ')'
	skipMany space
	char '{'
	skipMany space
	c <- expression
	skipMany space
	char '}'
	return $ TryCatch (e, Nothing) t c

-- | Parser for Throw expressions.
throw :: Parser Expression
throw = do
	string "throw"
	char '('
	skipMany space
	e <- many1 letter
	skipMany space
	char ')'
	return $ Throw (e, Nothing)

-- | Parser for anonymous function declations.
fun :: Parser Expression
fun = do
	string "fun"
	char '('
	skipMany space
	ids <- sepBy1 varTypePair $ try (skipMany space >> char ',' >> skipMany space)
	skipMany space
	char ')'
	skipMany space
	r <- optionMaybe $ try $ char ':' >> optional spaces >> types
	skipMany space
	char '{'
	skipMany space
	e <- expression
	skipMany space
	char '}'
	let (vs, ts) = unzip ids
	return $ foldr (\(v, t) x -> Fun v x t Nothing) e ids

-- | Parser for pair of identifier string and type.
varTypePair :: Parser (String, Type)
varTypePair = do
	v <- identifierString
	optional spaces
	char ':'
	optional spaces
	t <- types
	return (v, t)

-- | Parser for anonymous recursive function declations.
recfun :: Parser Expression
recfun = do
	string "fun"
	spaces
	string "rec"
	skipMany space
	n <- identifierString
	char '('
	optional spaces
	ids <- sepBy1 varTypePair $ try (skipMany space >> char ',' >> skipMany space)
	skipMany space
	char ')'
	optional spaces
	r <- optionMaybe $ try $ char ':' >> skipMany space >> types
	skipMany space
	char '{'
	skipMany space
	e <- expression
	skipMany space
	char '}'
	let (v, t) = head ids
	let (f, rt) = foldr (\(v, t) (x, mt) -> (Fun v x t mt, fmap (FunType t) mt)) (e, r) (tail ids)
	return $ RecFun n v f t rt

-- | Parser for let bindings.
letBinding :: Parser Expression
letBinding = do
	string "let"
	spaces
	x <- identifierString
	optional spaces
	char ':'
	optional spaces
	t <- types
	optional spaces
	string "="
	optional spaces
	e <- expression
	optional spaces
	r <- expression
	return $ Application (Fun x r t Nothing) e

-- | Parser for def bindings.
defBinding :: Parser Expression
defBinding = do
	string "def"
	spaces
	n <- identifierString
	char '('
	skipMany space
	ids <- sepBy1 varTypePair $ try (many space >> char ',' >> many space)
	skipMany space
	char ')'
	skipMany space
	rt <- char ':' >> skipMany space >> types
	skipMany space
	char '{'
	skipMany space
	b <- expression
	skipMany space
	char '}'
	skipMany space
	r <- expression
	let (vs, ts) = unzip ids
	let funType = foldr FunType rt ts
	let funBody = foldr (\(v, t) e -> Fun v e t Nothing) b ids
	return $ Application (Fun n r funType Nothing) funBody

-- | Parser for recursive def bindings.
defrecBinding :: Parser Expression
defrecBinding = do
	string "def"
	spaces
	string "rec"
	spaces
	n <- identifierString
	char '('
	skipMany space
	ids <- sepBy1 varTypePair $ try (skipMany space >> char ',' >> skipMany space)
	skipMany space
	char ')'
	skipMany space
	rt <- char ':' >> skipMany space >> types
	skipMany space
	char '{'
	optional spaces
	b <- expression
	skipMany space
	char '}'
	skipMany space
	r <- expression
	let (vs, ts) = unzip ids
	let funType = foldr FunType rt (tail ts)
	let (v, t) = head ids
	let funBody = foldr (\(v, t) e -> Fun v e t Nothing) b (tail ids)
	return $ Application (Fun n r (FunType t funType) Nothing) (RecFun n v funBody t (Just funType))

-- | Parser of types.
types :: Parser Type
types = try funType <|> nonFunTypes

-- | Parser of non-function types.
nonFunTypes :: Parser Type
nonFunTypes = tupleType <|> listType <|> intType <|> boolType <|> floatType

-- | Parser of the int type.
intType :: Parser Type
intType = do
	string "int"
	return IntType

-- | Parser of the bool type.
boolType :: Parser Type
boolType = do
	string "bool"
	return BoolType

-- | Parser of the float type.
floatType :: Parser Type
floatType = do
	string "float"
	return FloatType

-- | Parser of the [ ] type.
listType :: Parser Type
listType = do
	char '['
	many space
	t <- types
	many space
	char ']'
	return $ ListType t

-- | Parser of the { } type.
tupleType :: Parser Type
tupleType = do
	char '{'
	many space
	ts <- sepBy types $ try (many space >> char ',' >> many space)
	many space
	char '}'
	return $ TupleType ts

-- | Table of type operators.
typetable = [
		[Infix arrow AssocRight]
	]

-- | Parser of function types.
funType :: Parser Type
funType = buildExpressionParser typetable (try nonFunTypes <|> inParens types)

-- | Parser of the arrow type operation.
arrow :: Parser (Type -> Type -> Type)
arrow = try $ do
	optional spaces
	string "->"
	optional spaces
	return FunType

-- | Tests the Parser on a given string.
-- Used for debugging purposes.
test p s = case parse p "" s of 
	Right e -> Right e
	Left e -> Left $ show e

-- | Gets a well typed program from a string,
-- Used for debugging purposes.
getExpression :: String -> Either String Expression
getExpression s = case parse program "" s of
		(Left e) -> Left $ "Syntax error : " ++ show e
		(Right e) -> case fmap (computeIndexException . calculateStaticDistances) (inferAndVerify e) of
			(Right e) -> if checkNoFreeVariable e then Right e else Left "Unbound variable"
			(Left e) -> Left e

-- | Gets a well-typed program from a file.
-- If a expression is returned, it is certain that it does not
-- contain free variables, that the De Bruijn indexes are in place,
-- the exceptions are correctly numbered and that expressions are well-typed.
getProgram :: String -> IO (Either String Expression)
getProgram f = do
	r <- parseFromFile program f
	return $ case r of
		(Left e) -> Left $ "Syntax error : " ++ show e
		(Right e) -> case fmap (computeIndexException . calculateStaticDistances) (inferAndVerify e) of
			(Right e) -> if checkNoFreeVariable e then Right e else Left "Unbound variable"
			(Left e) -> Left e


