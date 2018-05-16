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

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr
import qualified Data.Set as Set
import Data.Functor.Identity (Identity)
import Data.Map as Map

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

data Expression = Constant (Set.Set Integer)
                | Identifier String
                | Union Expression Expression
                | Intersection Expression Expression
                | Difference Expression Expression
                | CartesianProduct Expression Expression
                | PowerSet Expression
                | Equal Expression Expression
                | Subset Expression Expression
                | Function String [Expression]
                deriving (Show)

data Statement = Assignment String Expression
               deriving (Show)

type UserState = Map.Map String Expression

parseExpression :: Parsec String () Expression
parseExpression = buildExpressionParser table parseTerm

table :: OperatorTable String () Identity Expression
table =
  [ [ Infix (T.reservedOp lexer "∪" >> return Union) AssocLeft
    , Infix (T.reservedOp lexer "∩" >> return Intersection) AssocLeft
    , Infix (T.reservedOp lexer "\\" >> return Difference) AssocLeft
    , Infix (T.reservedOp lexer "-" >> return Difference) AssocLeft
    , Infix (T.reservedOp lexer "×" >> return CartesianProduct) AssocLeft
    , Infix (T.reservedOp lexer "⊂" >> return Subset) AssocNone
    , Infix (T.reservedOp lexer "==" >> return Equal) AssocNone ]
  ]

parseTerm :: Parsec String () Expression
parseTerm = try parseFunction
        <|> parseIdentifier
        <|> parseConstant

parseStatement :: Parsec String () Statement
parseStatement = parseAssignment

parseConstant :: Parsec String () Expression
parseConstant = do
  asList <- T.braces lexer $ T.commaSep lexer $ T.integer lexer
  return $ Constant $ Set.fromList asList

parseIdentifier :: Parsec String () Expression
parseIdentifier = do
  identifier <- T.identifier lexer
  return $ Identifier identifier

parseAssignment :: Parsec String () Statement
parseAssignment = do
  identifier <- T.identifier lexer
  T.whiteSpace lexer
  T.reservedOp lexer "="
  T.whiteSpace lexer
  expression <- parseExpression
  return $ Assignment identifier expression

parseFunction :: Parsec String () Expression
parseFunction = do
  funcName <- matchFuncName
  args <- T.parens lexer $ T.commaSep1 lexer $ parseTerm
  return $ Function funcName args
  where matchFuncName = (try (T.reserved lexer "Union") >> return "Union")
          <|> (try (T.reserved lexer "Intersection") >> return "Intersection")
          <|> (try (T.reserved lexer "Difference") >> return "Difference")
          <|> (try (T.reserved lexer "P") >> return "P")
          <|> (try (T.reserved lexer "CartesianProduct") >>
               return "CartesianProduct")
          <|> (try (T.reserved lexer "Add") >> return "Add")
          <|> (try (T.reserved lexer "Remove") >> return "Remove")
