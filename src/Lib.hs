{-|
Module      : Lib
Description : This module contains data types, parsers, and interpreters for the
              application.
Copyright   : (c) Edgar Giovanni Lepe, 2018
License     : BSD3
Maintainer  : lepe.edgar10@gmail.com


-}
module Lib
    ( -- * Data Types
      Parser
    , UserState
    , Val(..)
    , Expr(..)
    , Stmt(..)
      -- * Parsers
    , parseValue
    , parseExpression
    , parseStatement
      -- * Interpreters
    , interpret
    , interpretExpression
    , interpretStatement
    ) where

import           Control.Monad.State  (State, get, modify, runState)
import           Data.List            (intercalate)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Text.Parsec          (Parsec, alphaNum, between, char, letter,
                                       oneOf, parse, try, (<|>), (<?>))
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as T

type Interpreter = State UserState

-- | A key value map of variable names to sets of values.
type UserState = Map.Map String (Set.Set Val)

-- | A parser with stream type of String, user state type of UserState,
-- underlying monad of type Identity, and return type of a.
type Parser = Parsec String ()

-- | Valid values for sets.
data Val = Bool Bool
         | Integer Integer
         | Double Double
         | Char Char
         | String String
         | Tuple (Val, Val)
         | Set (Set.Set Val)
         deriving (Eq, Ord)

instance Show Val where
  show (Bool b)      = show b
  show (Integer i)   = show i
  show (Double d)    = show d
  show (Char c)      = show c
  show (String s)    = s
  show (Tuple (x,y)) = show (x, y)
  show (Set s)       = prettyPrintSet s

-- | Valid expressions.
data Expr = Literal (Set.Set Val)
          | Identifier String
          | Union Expr Expr
          | Difference Expr Expr
          | Intersection Expr Expr
          | CartesianProduct Expr Expr
          deriving (Show, Eq)

-- | Valid statements.
data Stmt = Print Expr -- ^ Print statement
          | Assignment String Expr -- ^ Assignment statement
          | FuncCall String [Expr] -- ^ Function call
          | MutateSet String Val Expr
          deriving (Show)

-- | The Language Definition
--
--   - No comments are allowed.
--   - Identifiers start with a letter and can be followed by numbers, letters,
--     and underscores.
--   - The following operators are allowed:
--       - = (Assignment)
--       - |a| (Cardinality)
--       - P(a) (Power Set)
--       - ∪ (Union)
--       - ∩ (Intersection)
--       - \\ (Difference)
--       - - (Difference)
--       - × (Power Set)
langDef :: T.LanguageDef ()
langDef = emptyDef
          { T.commentStart = ""
          , T.commentEnd = ""
          , T.commentLine = ""
          , T.nestedComments = False
          , T.identStart = letter
          , T.identLetter = alphaNum <|> char '_'
          , T.opStart = oneOf "=|P∪∩\\-×"
          , T.opLetter = letter
          , T.reservedNames =
            [ "Union"
            , "Intersection"
            , "Difference"
            , "Subset"
            , "CartesianProduct"
            , "P"
            , "Add"
            , "Remove"
            , "True"
            , "False"
            ]
          , T.reservedOpNames =
            [ "="
            , "|"
            , "∪"
            , "∩"
            , "\\"
            , "-"
            , "×"
            , "⊂"
            , "=="
            ]
          }

-- | Creates a lexer using language definition
lexer :: T.TokenParser ()
lexer = T.makeTokenParser langDef

interpret :: String -> UserState -> (String, UserState)
interpret input userState = do
  case parse parseStatement "" input of
    (Right res) -> runState (interpretStatement res) userState
    (Left err)  -> (show err, userState)

prettyPrintSet :: Show a => Set.Set a -> String
prettyPrintSet s =
  let asList = Set.toList s
  in "{" ++ (intercalate ", " $ map show asList) ++ "}"

prettyPrintStringSet :: Set.Set String -> String
prettyPrintStringSet s =
  let asList = Set.toList s
  in "{" ++ (intercalate ", " asList) ++ "}"

interpretStatement :: Stmt -> Interpreter String
interpretStatement (Print expr) = do
  v <- interpretExpression expr
  return $ prettyPrintSet v
interpretStatement (Assignment var expr) = do
  s <- interpretExpression expr
  st <- get
  if Map.member var st
    then modify (\st' -> Map.insert var s $ Map.delete var st')
    else modify (\st' -> Map.insert var s st')
  return $ var ++ " = " ++ prettyPrintSet s
interpretStatement (FuncCall funcName args) =
  case funcName of
    "Union" -> interpretExpression (Union (args !! 0) (args !! 1)) >>=
      return . prettyPrintSet
    "Intersection" -> interpretExpression (Intersection (args !! 0)
                                                       (args !! 1)) >>=
      return . prettyPrintSet
    "Difference" -> interpretExpression (Difference (args !! 0) (args !! 1)) >>=
      return . prettyPrintSet
    "P" -> do
      s <- interpretExpression (args !! 0)
      let pSet = Set.map prettyPrintSet (Set.powerSet s)
      return $ prettyPrintStringSet pSet
    "Subset" -> do
      s1 <- interpretExpression (args !! 0)
      s2 <- interpretExpression (args !! 1)
      return $ show (s1 `Set.isSubsetOf` s2)
    "CartesianProduct" -> do
      s1 <- interpretExpression (args !! 0)
      s2 <- interpretExpression (args !! 1)
      return . prettyPrintSet $ Set.cartesianProduct s1 s2
    "Size" -> interpretExpression (args !! 0) >>= return . show . Set.size
interpretStatement (MutateSet op val expr) = do
  case op of
    "Add" -> case expr of
               (Identifier i) -> do
                 st <- get
                 case Map.lookup i st of
                   (Just oldSet) -> do
                     let newSet = Set.insert val oldSet
                     modify (\x -> Map.insert i newSet $ Map.delete i x)
                     return $ prettyPrintSet newSet
                   Nothing -> fail $ "Unknown identifier " ++ i
               _ -> do
                 s <- interpretExpression expr
                 return $ prettyPrintSet (Set.insert val s)
    "Remove" -> case expr of
                  (Identifier i) -> do
                    st <- get
                    case Map.lookup i st of
                      (Just oldSet) -> do
                        let newSet = Set.delete val oldSet
                        modify (\x -> Map.insert i newSet $ Map.delete i x)
                        return $ prettyPrintSet newSet
                      Nothing -> fail $ "Unknown identifier " ++ i
                  _ -> do
                    s <- interpretExpression expr
                    return $ prettyPrintSet (Set.delete val s)

interpretExpression :: Expr
                    -> Interpreter (Set.Set Val)
interpretExpression (Literal s) = do
  return s
interpretExpression (Identifier i) = do
  st <- get
  case Map.lookup i st of
    Just v -> return v
    Nothing -> fail $ "Unknown identifier " ++ i
interpretExpression (Union e1 e2) = do
  s1 <- interpretExpression e1
  s2 <- interpretExpression e2
  return $ Set.union s1 s2
interpretExpression (Intersection e1 e2) = do
  s1 <- interpretExpression e1
  s2 <- interpretExpression e2
  return $ Set.intersection s1 s2
interpretExpression (Difference e1 e2) = do
  s1 <- interpretExpression e1
  s2 <- interpretExpression e2
  return $ Set.difference s1 s2
interpretExpression (CartesianProduct e1 e2) = do
  s1 <- interpretExpression e1
  s2 <- interpretExpression e2
  return $ Set.map Tuple (Set.cartesianProduct s1 s2)

--------------------------------------------------------------------------------
-- Statement Parsing
--------------------------------------------------------------------------------

parseStatement :: Parser Stmt
parseStatement = (T.whiteSpace lexer >> try parseFunctionCall)
             <|> try parseMutateSet
             <|> try parseAssignment
             <|> parsePrint
             <?> "Statement"

parsePrint :: Parser Stmt
parsePrint = parseExpression >>= return . Print

parseMutateSet :: Parser Stmt
parseMutateSet = do
  op <- try (T.reserved lexer "Add" >> return "Add") <|>
        (T.reserved lexer "Remove" >> return "Remove")
  T.symbol lexer "("
  val <- parseValue
  T.comma lexer
  expr <- parseExpression
  T.symbol lexer ")"
  case op of
    "Add" -> return $ MutateSet "Add" val expr
    "Remove" -> return $ MutateSet "Remove" val expr

parseFunctionCall :: Parser Stmt
parseFunctionCall = try parseSizeFunc <|> parseFunctionCall'

parseSizeFunc :: Parser Stmt
parseSizeFunc = do
  arg <- between (T.symbol lexer "|") (T.symbol lexer "|") parseExpression
  return $ FuncCall "Size" [arg]

parseFunctionCall' :: Parser Stmt
parseFunctionCall' = do
  funcName <- parseFuncName
  args <-  T.parens lexer $ T.commaSep1 lexer parseExpression
  return $ FuncCall funcName args
  where parseFuncName = try (T.reserved lexer "Union" >> return "Union")
                    <|> try (T.reserved lexer "Intersection" >>
                             return "Intersection")
                    <|> try (T.reserved lexer "Difference" >>
                             return "Difference")
                    <|> try (T.reserved lexer "P" >> return"P")
                    <|> try (T.reserved lexer "Subset" >> return "Subset")
                    <|> try (T.reserved lexer "CartesianProduct" >>
                             return "CartesianProduct")

parseAssignment :: Parser Stmt
parseAssignment = do
  (Identifier ident) <- parseIdentifier
  T.whiteSpace lexer
  T.reservedOp lexer "="
  T.whiteSpace lexer
  v <- parseExpression
  return $ Assignment ident v

--------------------------------------------------------------------------------
-- Expression Parsing
--------------------------------------------------------------------------------

parseExpression :: Parser Expr
parseExpression = buildExpressionParser table parseTerm <?> "Expression"

table = [ [ Infix (T.reservedOp lexer "∪" >> return Union) AssocLeft
          , Infix (T.reservedOp lexer "∩" >> return Intersection) AssocLeft
          , Infix ((try (T.reservedOp lexer "\\") <|> T.reservedOp lexer "-") >>
                   return Difference) AssocLeft
          , Infix (T.reservedOp lexer "×" >> return CartesianProduct) AssocLeft
          ]
        ]

parseTerm :: Parser Expr
parseTerm = try parseLiteral <|> parseIdentifier

parseLiteral :: Parser Expr
parseLiteral = do
  (Set s) <- parseSet
  return $ Literal s

parseIdentifier :: Parser Expr
parseIdentifier = T.identifier lexer >>= return . Identifier

--------------------------------------------------------------------------------
-- VAL Parsing
--------------------------------------------------------------------------------

parseValue :: Parser Val
parseValue = try parseFloat
        <|> try parseInteger
        <|> try parseBool
        <|> try parseChar
        <|> try parseString
        <|> try parseTuple
        <|> try parseSet
        <?> "Value"

-- | Parses a set of Vals
parseSet :: Parser Val
parseSet = do
  s <- T.braces lexer $ T.commaSep lexer $ parseValue
  return $ Set $ Set.fromList s

-- | Parses a tuple of Vals
parseTuple :: Parser Val
parseTuple = do
  (x:y:_) <- T.parens lexer $ T.commaSep1 lexer $ parseValue
  return $ Tuple (x, y)

parseBool :: Parser Val
parseBool = do
  result <- try (T.symbol lexer "True") <|> T.symbol lexer "False"
  return $ Bool $ (read result :: Bool)

parseString :: Parser Val
parseString = T.stringLiteral lexer >>= return . String

parseChar :: Parser Val
parseChar = T.charLiteral lexer >>= return . Char

parseNumber :: Parser Val
parseNumber = try parseFloat <|> parseInteger

parseInteger :: Parser Val
parseInteger = T.integer lexer >>= return . Integer

parseFloat :: Parser Val
parseFloat = T.float lexer >>= return . Double
