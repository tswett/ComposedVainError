import Data.Char (isLetter, isNumber, isSpace)
import Data.List (nubBy)

data LispValue = LispAtom String | LispList [LispValue] | LispLambda Environment [String] LispValue
type Environment = [(String, LispValue)]

type Error = String

instance Show LispValue where
    show (LispAtom str) = str
    show (LispList l) = "(" ++ unwords (map show l) ++ ")"
    show (LispLambda env params body) = "<lambda (" ++ unwords params ++ ") " ++ show body ++ ">"

isAtomChar :: Char -> Bool
isAtomChar x = isLetter x || elem x "-"

parseExpr :: String -> Maybe (LispValue, String)
parseExpr "" = Nothing
parseExpr str | isAtomChar (head str) =
    Just (LispAtom (takeWhile isAtomChar str), dropWhile isAtomChar str)
parseExpr ('(':str) = do
    (list, str) <- parseList . dropWhile isSpace $ str
    Just (LispList list, str)
parseExpr _ = Nothing

parseList :: String -> Maybe ([LispValue], String)
parseList (')':str) = Just ([], str)
parseList str = do
    (headExpr, remainder) <- parseExpr str
    (tailExpr, remainder2) <- parseList . dropWhile isSpace $ remainder
    Just (headExpr : tailExpr, remainder2)

instance Read LispValue where
    readsPrec _ str = case parseExpr str of
        Just expr -> [expr]
        Nothing -> []

convertParamList :: [LispValue] -> Maybe [String]
convertParamList [] = Just []
convertParamList (LispAtom firstParamStr : params) = do
    remainingParamStrs <- convertParamList params
    Just (firstParamStr : remainingParamStrs)
convertParamList _ = Nothing

convertBindingList :: Environment -> [LispValue] -> Either Error [(String, LispValue)]
convertBindingList _ [] = Right []
convertBindingList env (LispList [LispAtom name, value] : remainingBindingExprs) = do
    remainingBindings <- convertBindingList env remainingBindingExprs
    evaluatedValue <- evalIn env value
    Right ((name, evaluatedValue) : remainingBindings)
convertBindingList _ _ = Left "ill-formed binding list"

evalIn :: Environment -> LispValue -> Either Error LispValue
evalIn env (LispList [LispAtom "lambda", LispList params, body]) = case convertParamList params of
    Just paramStrs -> Right (LispLambda env paramStrs body)
    Nothing -> Left "invalid parameter list in lambda expression"
evalIn env (LispList (LispAtom "lambda" : _)) = Left "ill-formed lambda expression"
evalIn env (LispList (LispAtom "let" : remainder)) = case convertBindingList env (init remainder) of
    Right bindings -> evalIn (mergeEnvironments env bindings) (last remainder)
    Left err -> Left err
evalIn env (LispList (funcExpr : paramExprs)) = do
    func <- evalIn env funcExpr
    params <- sequence (map (evalIn env) paramExprs)
    apply func params
evalIn env (LispList []) = Left "can't evaluate an empty list"
evalIn env (LispLambda _ _ _) = Left "can't evaluate a lambda"
evalIn env (LispAtom name) = case lookup name env of
    Just value -> Right value
    Nothing -> Left ("couldn't find variable " ++ name)

eval :: LispValue -> Either Error LispValue
eval = evalIn []

readEval :: String -> Either Error LispValue
readEval str = case reads str of
    [(value, "")] -> eval value
    [] -> Left "no valid parse"
    _ -> Left "unexpected incomplete parse"

mergeEnvironments :: Environment -> Environment -> Environment
mergeEnvironments old new = nubBy (\(name1, _) (name2, _) -> name1 == name2) (new ++ old)

apply :: LispValue -> [LispValue] -> Either Error LispValue
apply (LispLambda env params body) args = if length params /= length args
    then Left ("wrong number of arguments: expected " ++ show (length params) ++ ", got " ++ show (length args))
    else evalIn (mergeEnvironments env (zip params args)) body
apply value _ = Left ("not a function: " ++ show value)