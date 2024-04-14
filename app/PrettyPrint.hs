module PrettyPrint where

import Text.SourceCode ( textPosition, underlineTextSection )
import Text.AnsiEscape ( bold, red )
import Text.Parser (SyntaxError(SyntaxError))
import WACC.Syntax.Error (WaccSyntaxErrorType)
import WACC.Semantics.Utils (SemanticError (SemanticError))
import WACC.Semantics.Error (WaccSemanticsErrorType(RedefinedIdentifier))

type SourceCode = String
type NoStyle = Bool

class PrettyError error where
    errorTitle :: error -> String

    errorRange :: error -> (Int, Maybe Int)

    errorDescription :: SourceCode -> error -> String

    prettyPrintError :: PrettyError error =>
        NoStyle -> SourceCode -> error -> [String]
    prettyPrintError noStyle code e =
        [stylishTitle ++ " " ++ stylishPosition, description] ++ codeSection
        where
            (~) c = if noStyle then id else c

            (row, col) = textPosition code (fst $ errorRange e)
            (from, to') = errorRange e
            to = case to' of Just t -> t; Nothing -> from + 1

            stylishTitle = red ~ ("[" ++ errorTitle e ++ "] ")
            stylishPosition = bold ~ (show (row + 1) ++ ":" ++ show (col + 1))
            description = errorDescription code e
            codeSection = ("|   " ++) <$>
                underlineTextSection from to (2, '^', (red ~)) code

instance PrettyError (SyntaxError WaccSyntaxErrorType) where
    errorTitle :: SyntaxError WaccSyntaxErrorType -> String
    errorTitle = const "Syntax Error"

    errorRange :: SyntaxError WaccSyntaxErrorType -> (Int, Maybe Int)
    errorRange (SyntaxError pos _) = (pos, Nothing)

    errorDescription :: SourceCode -> SyntaxError WaccSyntaxErrorType -> String
    errorDescription _ (SyntaxError _ e) = show e

instance PrettyError SemanticError where
    errorTitle :: SemanticError -> String
    errorTitle = const "Semantic Error"

    errorRange :: SemanticError -> (Int, Maybe Int)
    errorRange (SemanticError (from, to) _) = (from, Just to)

    errorDescription :: SourceCode -> SemanticError -> String
    errorDescription code (SemanticError _ e) = case e of
        RedefinedIdentifier name (textPosition code -> (pos, _)) ->
            "Variable \"" ++ name ++ "\" is declared again.\n" ++
            "Previously defined at line " ++ show (pos + 1) ++ "."
        it -> show it
