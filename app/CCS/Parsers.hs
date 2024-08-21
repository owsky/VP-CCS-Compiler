module CCS.Parsers where

import CCS.Grammars (Action (..), Label (..), Process (..))
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, choice, eof, parse, some, (<?>))
import Text.Megaparsec.Char (letterChar)
import Utils (Parser, binary, dot, lexeme, parens, symbol)

-- | Parser for labels
pLabel :: Parser Label
pLabel = choice [Input <$> lexeme (some letterChar), Output <$> lexeme (some letterChar)]

-- | Parser for actions
pAction :: Parser Action
pAction = choice [ActionName <$> lexeme pLabel <?> "Action", Tau <$ lexeme (symbol "tau")]

-- | Parser for process names
pProcessName :: Parser Process
pProcessName = ProcessName <$> lexeme (some letterChar)

-- | Parser for the "action prefix" process operator
pActionPrefix :: Parser Process
pActionPrefix = do
  action <- pAction
  _ <- dot
  ActionPrefix action <$> pTerm

-- | Operator tables to be fed to makeExprParser for expression parsing
operatorTable :: [[Operator Parser Process]]
operatorTable =
  [ [binary "+" Sum],
    [binary "|" Parallel]
  ]

-- | Parser for terms to be fed to makeExprParser for expression parsing
pTerm :: Parser Process
pTerm =
  choice [pProcessName, parens pProcess]

-- | Parser for processes created through makeExprParser
pProcess :: Parser Process
pProcess = makeExprParser pTerm operatorTable

-- | Helper function that tries to parse a string into a process
parseProcess :: String -> Either (ParseErrorBundle String Void) Process
parseProcess = parse (pProcess <* eof) "stdin"