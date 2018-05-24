{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
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
      -- ** Value parser
    , parseVal
      -- ** Expression Parser
    , parseExpr
    , simplifyExpression
    , prettyPrintSet
    , mainParser
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Identity  (Identity)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import qualified Text.Parsec.Token      as T
import qualified Data.List

-- | A key value map of variable names to sets of values.
type UserState = Map.Map String (Set.Set Val)

-- | A parser with stream type of String, user state type of UserState,
-- underlying monad of type Identity, and return type of a.
type Parser a = ParsecT String UserState Identity a

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
  show (Tuple (x,y)) = show (show x, show y)
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
data Stmt = Print (Set.Set Val) -- ^ Print statement
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
langDef :: T.GenLanguageDef String UserState Identity
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
lexer :: T.GenTokenParser String UserState Identity
lexer = T.makeTokenParser langDef

mainParser :: Parser (String, UserState)
mainParser = do
  res <- T.whiteSpace lexer >> parseStatement >>= interpretStatement
  st <- getState
  return $ (res, st)

prettyPrintSet :: Show a => Set.Set a -> String
prettyPrintSet s =
  let asList = Set.toList s
  in "{" ++ (Data.List.intercalate ", " $ map show asList) ++ "}"

prettyPrintStringSet :: Set.Set String -> String
prettyPrintStringSet s =
  let asList = Set.toList s
  in "{" ++ (Data.List.intercalate "," asList) ++ "}"


interpretStatement :: Stmt -> Parser String
interpretStatement (Print v) = return $ prettyPrintSet v
interpretStatement (Assignment var expr) = do
  s <- simplifyExpression expr
  st <- getState
  if Map.member var st
    then modifyState (\st' -> Map.insert var s $ Map.delete var st')
    else modifyState (\st' -> Map.insert var s st')
  return $ var ++ " = " ++ prettyPrintSet s
interpretStatement (FuncCall funcName args) =
  case funcName of
    "Union" -> simplifyExpression (Union (args !! 0) (args !! 1)) >>=
      return . prettyPrintSet
    "Intersection" -> simplifyExpression (Intersection (args !! 0)
                                                       (args !! 1)) >>=
      return . prettyPrintSet
    "Difference" -> simplifyExpression (Difference (args !! 0) (args !! 1)) >>=
      return . prettyPrintSet
    "P" -> do
      s <- simplifyExpression (args !! 0)
      let pSet = Set.map prettyPrintSet (Set.powerSet s)
      return $ prettyPrintStringSet pSet
    "Subset" -> do
      s1 <- simplifyExpression (args !! 0)
      s2 <- simplifyExpression (args !! 1)
      return $ show (s1 `Set.isSubsetOf` s2)
    "CartesianProduct" -> do
      s1 <- simplifyExpression (args !! 0)
      s2 <- simplifyExpression (args !! 1)
      return . prettyPrintSet $ Set.cartesianProduct s1 s2
    "Size" -> simplifyExpression (args !! 0) >>= return . show . Set.size
interpretStatement (MutateSet op val expr) = do
  case op of
    "Add" -> case expr of
               (Identifier i) -> do
                 st <- getState
                 case Map.lookup i st of
                   (Just oldSet) -> do
                     let newSet = Set.insert val oldSet
                     modifyState (\x -> Map.insert i newSet $ Map.delete i x)
                     return $ prettyPrintSet newSet
                   Nothing -> fail $ "Unknown identifier " ++ i
               _ -> do
                 s <- simplifyExpression expr
                 return $ prettyPrintSet (Set.insert val s)
    "Remove" -> case expr of
                  (Identifier i) -> do
                    st <- getState
                    case Map.lookup i st of
                      (Just oldSet) -> do
                        let newSet = Set.delete val oldSet
                        modifyState (\x -> Map.insert i newSet $ Map.delete i x)
                        return $ prettyPrintSet newSet
                      Nothing -> fail $ "Unknown identifier " ++ i
                  _ -> do
                    s <- simplifyExpression expr
                    return $ prettyPrintSet (Set.delete val s)


--------------------------------------------------------------------------------
-- Statement Parsing
--------------------------------------------------------------------------------

parseStatement :: Parser Stmt
parseStatement = try parseFunctionCall
             <|> try parseMutateSet
             <|> try parseAssignment
             <|> parsePrint

parsePrint :: Parser Stmt
parsePrint = parseExpr >>= simplifyExpression >>= return . Print

parseMutateSet :: Parser Stmt
parseMutateSet = do
  op <- try (T.reserved lexer "Add" >> return "Add") <|>
        (T.reserved lexer "Remove" >> return "Remove")
  T.symbol lexer "("
  val <- parseVal
  T.comma lexer
  expr <- parseExpr
  T.symbol lexer ")"
  case op of
    "Add" -> return $ MutateSet "Add" val expr
    "Remove" -> return $ MutateSet "Remove" val expr

parseFunctionCall :: Parser Stmt
parseFunctionCall = try parseSizeFunc <|> parseFunctionCall'

parseSizeFunc :: Parser Stmt
parseSizeFunc = do
  arg <- between (T.symbol lexer "|") (T.symbol lexer "|") parseExpr
  return $ FuncCall "Size" [arg]

parseFunctionCall' :: Parser Stmt
parseFunctionCall' = do
  funcName <- parseFuncName
  args <-  T.parens lexer $ T.commaSep1 lexer parseExpr
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
  v <- parseExpr
  return $ Assignment ident v

simplifyExpression :: Expr -> Parser (Set.Set Val)
simplifyExpression (Literal s) = return s
simplifyExpression (Identifier i) = do
  st <- getState
  case Map.lookup i st of
    Just v -> return v
    Nothing -> fail $ "Unknown identifier " ++ i
simplifyExpression (Union e1 e2) = do
  s1 <- simplifyExpression e1
  s2 <- simplifyExpression e2
  return $ Set.union s1 s2
simplifyExpression (Intersection e1 e2) = do
  s1 <- simplifyExpression e1
  s2 <- simplifyExpression e2
  return $ Set.intersection s1 s2
simplifyExpression (Difference e1 e2) = do
  s1 <- simplifyExpression e1
  s2 <- simplifyExpression e2
  return $ Set.difference s1 s2
simplifyExpression (CartesianProduct e1 e2) = do
  s1 <- simplifyExpression e1
  s2 <- simplifyExpression e2
  let asList = Set.toList $ Set.cartesianProduct s1 s2
  return $ Set.fromList $ map Tuple asList

--------------------------------------------------------------------------------
-- Expression Parsing
--------------------------------------------------------------------------------

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseTerm

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

parseVal :: Parser Val
parseVal = try parseFloat
        <|> try parseInteger
        <|> try parseBool
        <|> try parseChar
        <|> try parseString
        <|> try parseNTuple
        <|> try parseSet

parseSet :: Parser Val
parseSet = do
  s <- T.braces lexer $ T.commaSep lexer $ parseVal
  return $ Set $ Set.fromList s

parseNTuple :: Parser Val
parseNTuple = do
  (x:y:_) <- T.parens lexer $ T.commaSep1 lexer $ parseVal
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
