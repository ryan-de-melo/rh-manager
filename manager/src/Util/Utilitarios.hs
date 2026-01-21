module Util.Utilitarios where

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

pause :: IO ()
pause = do
  putStrLn "\nPressione ENTER para voltar..."
  _ <- getLine
  return ()
