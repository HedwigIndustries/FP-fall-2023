module Main (main) where

import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
    where 
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "hi> "
            case minput of
                Nothing   -> return ()
                Just ":q" -> return ()
                Just input  -> do 
                    case parse input of
                        Left  err  -> outputStrLn $ show err
                        Right expr -> do
                            hiExpr <- eval expr
                            case hiExpr of 
                                Left err'   -> outputStrLn $ show err'
                                Right expr' -> outputStrLn $ show $ prettyValue expr'
                    loop
                    