{-# LANGUAGE BangPatterns #-}
{-|
Module      : Lib
Description : This module contains the library functions for the application
Copyright   : (c) Edgar Giovanni Lepe, 2018
License     : BSD3
Portability :


-}
module Lib
    ( parseTerm
    , parseStatement
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Identity  (Identity)
import           Data.Map               as Map
import           Data.Semigroup         (Semigroup (stimes, (<>)))
import qualified Data.Set               as Set
import           Data.Set.Internal      (Set (Tip), Set (Bin), merge)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import qualified Text.Parsec.Token      as T

-- | The Language Definition
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
langDef :: T.GenLanguageDef String UserState IO
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
lexer :: T.GenTokenParser String UserState IO
lexer = T.makeTokenParser langDef

data Expression = Constant (Set.Set Integer)
                | Identifier String
                | Union Expression Expression
                | Intersection Expression Expression
                | Difference Expression Expression
                | CartesianProduct Expression Expression
                | PowerSet Expression
                | Equal Expression Expression
                | Subset Expression Expression
                deriving (Show)

data Statement = Assignment String Expression
               | FunctionCall String [Expression]
               | Print Expression
               deriving (Show)

type UserState = Map.Map String (Set.Set Integer)

type Parser a = ParsecT String UserState IO a

parseExpression :: Parser Expression
parseExpression = try parseOperator <|> try parseTerm <?> "Invalid Expression"

parseStatement :: Parser Statement
parseStatement = try parseFunction
             <|> try parseFunction
             <|> try parsePrint
             <?> "Invalid Statement"

parseOperator :: Parser Expression
parseOperator = buildExpressionParser table parseTerm

table :: OperatorTable String UserState IO Expression
table =
  [ [ Infix (T.reservedOp lexer "∪" >> return Union) AssocLeft
    , Infix (T.reservedOp lexer "∩" >> return Intersection) AssocLeft
    , Infix (T.reservedOp lexer "\\" >> return Difference) AssocLeft
    , Infix (T.reservedOp lexer "-" >> return Difference) AssocLeft
    , Infix (T.reservedOp lexer "==" >> return Equal) AssocLeft
    , Infix (T.reservedOp lexer "⊂" >> return Subset) AssocLeft
    ]
  ]

parseTerm :: Parser Expression
parseTerm = parseIdentifier
        <|> parseConstant

parseConstant :: Parser Expression
parseConstant = do
  asList <- T.braces lexer $ T.commaSep lexer $ T.integer lexer
  return $ Constant $ Set.fromList asList

parseIdentifier :: Parser Expression
parseIdentifier = do
  identifier <- T.identifier lexer
  return $ Identifier identifier

parseAssignment :: Parser Statement
parseAssignment = do
  identifier <- T.identifier lexer
  T.whiteSpace lexer
  T.reservedOp lexer "="
  T.whiteSpace lexer
  expression <- parseOperator
  return $ Assignment identifier expression

parseFunction :: Parser Statement
parseFunction = do
  funcName <- matchFuncName
  args <- T.parens lexer $ T.commaSep1 lexer $ parseTerm
  return $ FunctionCall funcName args
  where matchFuncName = (try (T.reserved lexer "Union") >> return "Union")
          <|> (try (T.reserved lexer "Intersection") >> return "Intersection")
          <|> (try (T.reserved lexer "Difference") >> return "Difference")
          <|> (try (T.reserved lexer "P") >> return "P")
          <|> (try (T.reserved lexer "CartesianProduct") >>
               return "CartesianProduct")
          <|> (try (T.reserved lexer "Add") >> return "Add")
          <|> (try (T.reserved lexer "Remove") >> return "Remove")

interpretExpression :: Expression -> Parser (Set Integer)
interpretExpression (Constant c) = return c
interpretExpression (Identifier i) = do
  st <- getState
  case Map.lookup i st of
    Nothing -> fail $ "Unknown identifier: " ++ i
    Just v  -> return v
interpretExpression (Union expr1 expr2) = do
  set1 <- interpretExpression expr1
  set2 <- interpretExpression expr2
  return $ Set.union set1 set2
interpretExpression (Intersection expr1 expr2) = do
  set1 <- interpretExpression expr1
  set2 <- interpretExpression expr2
  return $ Set.intersection set1 set2
interpretExpression (Difference expr1 expr2) = do
  set1 <- interpretExpression expr1
  set2 <- interpretExpression expr2
  return $ Set.difference set1 set2

interpretCartesianProduct :: Expression -> Parser (Set (Integer, Integer))
interpretCartesianProduct (CartesianProduct expr1 expr2) = do
  set1 <- interpretExpression expr1
  set2 <- interpretExpression expr2
  return $ Set.cartesianProduct set1 set2

interpretPowerSet :: Expression -> Parser (Set (Set Integer))
interpretPowerSet (PowerSet s) = interpretExpression s >>=
  return . Set.powerSet

interpretBoolExpression :: Expression -> Parser Bool
interpretBoolExpression (Equal expr1 expr2) = do
  set1 <- interpretExpression expr1
  set2 <- interpretExpression expr2
  return $ set1 == set2
interpretBoolExpression (Subset expr1 expr2) = do
  set1 <- interpretExpression expr1
  set2 <- interpretExpression expr2
  return $ Set.isSubsetOf set1 set2
