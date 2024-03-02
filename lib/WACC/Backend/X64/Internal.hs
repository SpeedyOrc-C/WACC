module WACC.Backend.X64.Internal where

data Function
    = PrintString
    | PrintChar
    | PrintInt
    | PrintBool
    | PrintLineBreak
    | PrintPointer
    | ErrorOutOfMemory
    | ErrorNull
    | ErrorDivideZero
    | ErrorOverflow
    | ErrorBadChar
    | ErrorOutOfBounds
    | SeekArrayElement1
    | SeekArrayElement4
    | SeekArrayElement8
    | ReadInt
    | ReadChar
    deriving (Show, Eq, Ord)
