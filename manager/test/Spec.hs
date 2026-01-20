module Main where

import System.IO (hSetEncoding, stdout, stdin, stderr, utf8)

import Presence
import Func
import AfastamentoTest
import JornadaLicencaSpec
import FeriasTests

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin  utf8
  hSetEncoding stderr utf8

  putStrLn " INICIANDO TESTES DO SISTEMA RH "

  runPresenceTests
  runFuncionarioTests
  runAfastamentoTests
  runLicenseTests
  runFeriasTests

  putStrLn " TESTES FINALIZADOS "
