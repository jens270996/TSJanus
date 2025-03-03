module Jana.SymmetryChecker where

import Jana.Ast
import Jana.Invert
import Data.List (uncons)
import Jana.Format (formatStmt, formatStmts)
checkSymmetryId :: Program -> String -> Bool
checkSymmetryId (Program _ procs) funId =
    case filter (\procedure -> (procedureId . procname) procedure == funId ) procs of
                                [procedure] -> checkSymmetryStmts procs (body procedure)
                                _ -> error "invalid procedure id"


checkSymmetryStmts ::  [Proc] -> [Stmt] -> Bool
checkSymmetryStmts procs stmts =
    case stmts of
        [] -> True
        [s] -> checkSymmetryStmt procs s
        stmts -> case uncons stmts
                 of Just (s, [s2]) -> s `equiv` invertStmt Locally s2
                    Just (s,stmts') -> checkSymmetryStmts procs (init stmts') && invertStmt Locally (last stmts') `equiv` s
                    Nothing -> error "should never reach here as we already checked that (length stmts) >= 2"

checkSymmetryStmt :: [Proc] -> Stmt-> Bool
checkSymmetryStmt procs s =
    case s of
        (Local l1 stmts l2 _) -> checkSymmetryStmts procs stmts
        (Call funId _ _) -> case filter (\procedure -> procname procedure == funId ) procs of
                                [procedure] -> checkSymmetryStmts procs (body procedure)
                                _ -> error "invalid procedure id"
        (Uncall funId _ _) -> case filter (\procedure -> procname procedure == funId ) procs of
                                [procedure] -> checkSymmetryStmts procs (body procedure)
                                _ -> error "invalid procedure id"
        s -> s `equiv` invertStmt Locally s