import Text.Read
import System.IO
import System.IO.Error
import Control.Exception

type Stack = [Float]

main = catch (rpn []) handler
    where
        handler :: IOError -> IO ()
        handler e = putStrLn $ "Runtime error: " ++ show e

onError :: IOError -> IO ()
onError e
    | isUserError e = putStrLn $ "Runtime error: " ++ show e
    | otherwise = ioError e

rpn :: Stack -> IO ()
rpn stack = do
    line <- getLine
    let tokens = words line
    stack' <- performOperation tokens stack
    mapM_ (\v -> do putStr $ show v ++ " ") stack'
    putStrLn $ if null stack' then "<empty>" else ""
    rpn stack'

performOperation :: [String] -> Stack -> IO Stack
performOperation [] stack = return stack
performOperation (t:tokensRest) stack = do
    stack' <- case (readMaybe t :: Maybe Float) of
            Just n -> return (n:stack)
            Nothing -> case t of
                "+" -> binaryOp (+) stack
                "-" -> binaryOp (-) stack
                "*" -> binaryOp (*) stack
                "/" -> binaryOp (/) stack
                "%" -> binaryOp (\a b -> fromIntegral $ round a  `mod` round b) stack
                "&" -> flipFlop stack
                "_" -> unaryOp (\a -> fromIntegral $ floor a) stack
                "^" -> unaryOp (\a -> fromIntegral $ floor $ a + 1) stack
                "!" -> return $ drop 1 stack
                _ -> ioError $ userError $ "Unknown operator `" ++ t ++ "`"
    performOperation tokensRest stack'

takeOperansFromStack :: Int -> Stack -> (Maybe [Float], Stack)
takeOperansFromStack n stack
    | len >= n = let ops = take n stack
                     stack' = drop n stack
                 in (Just ops, stack')
    | otherwise = (Nothing, stack)
        where len = length stack

unaryOp :: (Float -> Float) -> Stack -> IO Stack
unaryOp op stack =
    let (operand, stack') = takeOperansFromStack 1 stack
    in case operand of
        Nothing -> ioError $ userError "not enough arguments for binary operation"
        Just [a] -> return $ op a : stack'

binaryOp :: (Float -> Float -> Float) -> Stack -> IO Stack
binaryOp op stack =
    let (operands, stack') = takeOperansFromStack 2 stack
    in case operands of
        Nothing -> ioError $ userError "not enough arguments for binary operation"
        Just [a, b] -> return $ (a `op` b) : stack'

flipFlop stack =
    let (operands, stack') = takeOperansFromStack 2 stack
    in case operands of
        Nothing -> ioError $ userError "not enough arguments for flip operation"
        Just [a, b] -> return $ b : a : stack'
