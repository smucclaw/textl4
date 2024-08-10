{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lam4.Parser.Type (
  -- * Parser related
  Parser(..),
  ParserState(..),
  AesonParser,
  ParserError,

  -- * RefPath, Env
  RefPath,
  Env
) where

import           Base
import qualified Base.Aeson             as A
import           Lam4.Expr.Name         (Unique)

import           Control.Monad.Base
import           Control.Monad.Except   ()
import           Control.Monad.Identity ()
import           Control.Monad.Reader   ()


{- | JSONPath for Refs

Invariant: The path always starts with the global root element of the json-serialized concrete syntax we're parsing

__Examples:__

@
"#/elements@2"
"#/elements@3/params@0/param"
"#/elements@0"
@
-}
type RefPath = Text

-- | Environment for Parser: map from RefPaths to Uniques/Ints
type Env = Map RefPath Unique

{----------------------------
    ParserState, Parser
-----------------------------}

-- | does NOT include the input object / value
data ParserState = MkParserState
  { refPathEnv :: !Env
  , maxUnique  :: !Unique -- ^ for making fresh int vars
  }
  deriving stock (Show, Generic)

type AesonParser = A.Parser

-- | Concrete Syntax Parser monad, for parsing the concrete syntax json serialized from the Langium parser
type Parser :: Type -> Type
newtype Parser a
  = MkParser (ParserState -> AesonParser (a, ParserState))
  deriving
    (Functor, Applicative, Monad, MonadState ParserState)
    via (StateT ParserState AesonParser)

instance MonadBase AesonParser Parser where
  liftBase :: AesonParser a -> Parser a
  liftBase aesonParser = MkParser $ \s -> fmap (, s) aesonParser


  -- = MkParser (ParserState -> A.Parser ( Either ParserError a, ParserState) )
  -- deriving
  --   (Functor, Applicative, Monad, MonadState ParserState, MonadError ParserError)
  --   via ExceptT ParserError (StateT ParserState A.Parser)
{-

ExceptT ParserError (StateT ParserState A.Parser) a
= StateT ParserState A.Parser (Either ParserError a)
= ParserState -> A.Parser ( (Either ParserError a), ParserState) )
-}

-- Prob won't really need much ParseError support
-- since this is parsing something that's valid by lights of Langium parser and validator
type ParserError = String
