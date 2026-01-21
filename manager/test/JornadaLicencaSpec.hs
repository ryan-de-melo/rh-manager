module JornadaLicencaSpec (runLicenseTests) where

import Data.Time (fromGregorian, Day)
import Model.TiposDados
import Controller.JornadaLicenca

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

--
-- SETUP PARA OS TESTES DE LICENÇA
--

inicio1, fimValido, fimInvalido, fimInvertido :: Day
inicio1      = fromGregorian 2024 6 1
fimValido    = fromGregorian 2024 6 4
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

--
-- SETUP PARA OS TESTES DE CICLO DE FOLGA
--

ultimaFolgaData, dataFolgaValida, dataFolgaInvalida :: Day
ultimaFolgaData = fromGregorian 2024 6 1
dataFolgaValida = fromGregorian 2024 6 5   
dataFolgaInvalida = fromGregorian 2024 6 10

cicloFolgaValido :: CicloFolga
cicloFolgaValido = Folga
  { ultimaFolga = ultimaFolgaData
  , dataFolga = dataFolgaValida
  }

cicloFolgaInvalido :: CicloFolga
cicloFolgaInvalido = Folga
  { ultimaFolga = ultimaFolgaData
  , dataFolga = dataFolgaInvalida
  }

cicloFolgaMesmoDia :: CicloFolga
cicloFolgaMesmoDia = Folga
  { ultimaFolga = ultimaFolgaData
  , dataFolga = ultimaFolgaData
  }

cicloFolgaDatasInvertidas :: CicloFolga
cicloFolgaDatasInvertidas = Folga
  { ultimaFolga = fromGregorian 2024 6 10
  , dataFolga = fromGregorian 2024 6 1
  }


verificaLegalidadeCicloValidoTeste :: IO ()
verificaLegalidadeCicloValidoTeste = do
  assert (verificaLegalidadeDeCicloFolga cicloFolgaValido)
    "Ciclo de folga válido (4 dias) foi aceito."

verificaLegalidadeCicloInvalidoTeste :: IO ()
verificaLegalidadeCicloInvalidoTeste = do
  assert (not (verificaLegalidadeDeCicloFolga cicloFolgaInvalido))
    "Ciclo de folga inválido (9 dias >= 7) foi rejeitado."

verificaLegalidadeCicloMesmoDiaTeste :: IO ()
verificaLegalidadeCicloMesmoDiaTeste = do
  assert (verificaLegalidadeDeCicloFolga cicloFolgaMesmoDia)
    "Ciclo de folga com mesmo dia (0 dias) foi aceito."

verificaLegalidadeCicloDatasInvertidosTeste :: IO ()
verificaLegalidadeCicloDatasInvertidosTeste = do
  assert (not (verificaLegalidadeDeCicloFolga cicloFolgaDatasInvertidas))
    "Ciclo de folga com datas invertidas foi rejeitado."


buscaDiaDeFolgaComDiasFaltantesTeste :: IO ()
buscaDiaDeFolgaComDiasFaltantesTeste = do
  let resultado = buscaDiaDeFolga cicloFolgaValido
  assert (resultado == Just dataFolgaValida)
    "Busca de dia de folga com dias faltantes retornou a data corretamente."




runLicenseTests :: IO ()
runLicenseTests = do
  putStrLn "\n--- Testes de Licença ---"

  licencaValidaTeste
  licencaInvalidaPorDiasTeste
  licencaDatasInvertidasTeste

  putStrLn "\n--- Testes de Ciclo de Folga ---"

  verificaLegalidadeCicloValidoTeste
  verificaLegalidadeCicloInvalidoTeste
  verificaLegalidadeCicloMesmoDiaTeste
  verificaLegalidadeCicloDatasInvertidosTeste