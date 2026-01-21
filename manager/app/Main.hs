module Main where

import Util.FeriasService

main :: IO ()
main = do
  putStrLn "Iniciando sistema de gerenciamento de férias..."
  iniciarSistemaFerias
