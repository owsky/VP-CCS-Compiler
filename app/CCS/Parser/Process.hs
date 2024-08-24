module CCS.Parser.Process where

-- import CCS.Grammars (Process (..))
-- import CCS.ParsersAction (pAction, pActionRestriction, pRelabellingFunction)
-- import Control.Monad.Combinators.Expr (Operator, makeExprParser)
-- import Data.Void (Void)
-- import Text.Megaparsec (ParseErrorBundle, choice, eof, optional, parse, some, try)
-- import Text.Megaparsec.Char (letterChar)
-- import Utils (Parser, dot, lexeme, roundParens, symbol)

-- pRelabel :: Process -> Parser Process
-- pRelabel p = do
--   relabel <- optional $ try pRelabellingFunction
--   case relabel of
--     Just f -> return (Relabelling p f)
--     Nothing -> return p

-- pRestriction :: Process -> Parser Process
-- pRestriction p = do
--   restrict <- optional $ try pActionRestriction
--   case restrict of
--     Just r -> return (Restriction p r)
--     Nothing -> return p

-- pRelabelRestriction :: Process -> Parser Process
-- pRelabelRestriction p = do
--   p2 <- pRelabel p
--   pRestriction p2

-- -- | Parser for process names
-- pProcessName :: Parser Process
-- pProcessName = do
--   name <- lexeme $ choice [some letterChar, symbol "0"]
--   return $ ProcessName name

-- -- | Parser for the "action prefix" process operator
-- pActionPrefix :: Parser Process
-- pActionPrefix = try $ do
--   action <- pAction
--   _ <- dot
--   ActionPrefix action <$> pProcess

-- pParens :: Parser Process
-- pParens = do
--   process <- roundParens pProcess
--   pRelabelRestriction process

-- pProcess :: Parser Process
-- pProcess = do

-- -- | Helper function that tries to parse a string into a process
-- parseProcess :: String -> Either (ParseErrorBundle String Void) Process
-- parseProcess = parse (pProcess <* eof) "stdin"