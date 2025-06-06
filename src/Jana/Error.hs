module Jana.Error where

import Data.List (intercalate, sort)
import Text.Printf (printf)
import Text.PrettyPrint
import Text.Parsec.Pos

import Jana.Ast
import Jana.Format


joinlines :: [[Char]] -> [Char]
joinlines = intercalate "\n"

indent :: String -> String
indent = joinlines . map ("    " ++) . lines


data Message = Message String
             | InArgument String String
             | InExpression Expr
             | InStatement Stmt String
             | InProcedure String

instance Enum Message where
    fromEnum (Message       _) = 0
    fromEnum (InArgument  _ _) = 1
    fromEnum (InExpression  _) = 2
    fromEnum (InStatement _ _) = 3
    fromEnum (InProcedure   _) = 3
    toEnum _ = error "toEnum is undefined for Message"

instance Eq Message where
    m1 == m2 = fromEnum m1 == fromEnum m2

instance Ord Message where
    compare msg1 msg2 = compare (fromEnum msg1) (fromEnum msg2)

instance Show Message where
  show (Message s) = s
  show (InArgument funid argid) =
    printf "In an argument of `%s', namely `%s'"
           funid argid
  show (InExpression expr) = render $
    text "In expression:" $+$ nest 4 (formatExpr expr)
  show (InStatement stmt store) = render $
    text "In statement:" $+$
      nest 4 (formatStmtAbbrv stmt) $+$
      if store /= ""
        then nest 2 (text "where" <+> vcat (map text $ lines store))
        else empty
  show (InProcedure proc) =
    printf "In procedure `%s'" proc


data JanaError = JanaError SourcePos [Message]

-- instance Except JanaError where
--   noMsg  = newErrorUnknown (newPos "" 0 0)
--   strMsg = newErrorMessage (newPos "" 0 0) . Message

errorPos :: JanaError -> SourcePos
errorPos (JanaError pos _)
  = pos

errorMessages :: JanaError -> [Message]
errorMessages (JanaError _ msgs)
  = sort $ reverse msgs

errorIsUnknown :: JanaError -> Bool
errorIsUnknown (JanaError _ msgs)
  = null msgs

newErrorUnknown :: SourcePos -> JanaError
newErrorUnknown pos
  = JanaError pos []

newErrorMessage :: SourcePos -> Message -> JanaError
newErrorMessage pos msg
  = JanaError pos [msg]

newFileError :: String -> Message -> JanaError
newFileError filename
  = newErrorMessage (newPos filename 0 0)


addErrorMessage :: Message -> JanaError -> JanaError
addErrorMessage msg (JanaError pos msgs)
  = JanaError pos (msg : msgs)

setErrorPos :: JanaError -> SourcePos -> JanaError
setErrorPos (JanaError _ msgs) pos
  = JanaError pos msgs

setErrorMessage :: Message -> JanaError -> JanaError
setErrorMessage msg (JanaError pos msgs)
    = JanaError pos (msg : filter (msg /=) msgs)

addOnceErrorMessage :: Message -> JanaError -> JanaError
addOnceErrorMessage msg err@(JanaError pos msgs)
    | msg `elem` msgs = err
    | otherwise = JanaError pos (msg : filter (msg /=) msgs)

instance Show JanaError where
  show err = printf "%s:\n%s" introLine (showMsgs msgs)
    where
      pos = errorPos err
      introLine =
        case (sourceName pos, sourceLine pos, sourceColumn pos) of
          ("", 0, 0)        -> "Error"
          (file, 0, 0)      -> "File \"" ++ file ++ "\""
          ("-", line, col) ->
            printf "Error in line %d, column %d" line col
          (file, line, col) ->
            printf "File \"%s\" in line %d, column %d" file line col
      msgs = errorMessages err
      showMsgs []   = indent "Unknown error occured"
      showMsgs ms = joinlines $ map (indent . show) ms
