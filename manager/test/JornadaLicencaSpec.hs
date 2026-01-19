module JornadaLicencaSpec (runLicenseTests) where

import Data.Time (fromGregorian, Day)
import Model.TiposDados
import Controller.JornadaLicenca

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

inicio1, fimValido, fimInvalido, fimInvertido :: Day
inicio1      = fromGregorian 2024 6 1
fimValido    = fromGregorian 2024 6 4   -- 3 dias
fimInvalido  = fromGregorian 2024 6 10
fimInvertido = fromGregorian 2024 5 30


licencaCasamentoValida :: Licenca
licencaCasamentoValida = Licenca
  { tipoLicensa = Casamento
  , dataInicio  = inicio1
  , dataFim     = fimValido
  , descricao   = "Casamento dentro do prazo"
  }

licencaCasamentoInvalida :: Licenca
licencaCasamentoInvalida = Licenca
  { tipoLicensa = Casamento
  , dataInicio  = inicio1
  , dataFim     = fimInvalido
  , descricao   = "Casamento fora do prazo"
  }

licencaDatasInvertidas :: Licenca
licencaDatasInvertidas = Licenca
  { tipoLicensa = Luto
  , dataInicio  = inicio1
  , dataFim     = fimInvertido
  , descricao   = "Datas invertidas"
  }


licencaValidaTeste :: IO ()
licencaValidaTeste = do
  assert (verificarSeLicencaValida licencaCasamentoValida)
    "Licença de casamento válida foi aceita."

licencaInvalidaPorDiasTeste :: IO ()
licencaInvalidaPorDiasTeste = do
  assert (not (verificarSeLicencaValida licencaCasamentoInvalida))
    "Licença de casamento fora do prazo foi rejeitada."

licencaDatasInvertidasTeste :: IO ()
licencaDatasInvertidasTeste = do
  assert (not (verificarSeLicencaValida licencaDatasInvertidas))
    "Licença com datas invertidas foi rejeitada."






runLicenseTests :: IO ()
runLicenseTests = do
  putStrLn "\n--- Testes de Licença ---"

  licencaValidaTeste
  licencaInvalidaPorDiasTeste
  licencaDatasInvertidasTeste