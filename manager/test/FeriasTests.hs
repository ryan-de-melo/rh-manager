module FeriasTests (runFeriasTests) where

import Controller.GerenciarFerias
import Model.TiposDados
import Data.Time
import qualified Data.Map as Map

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

cpfTeste :: CPF
cpfTeste = "12345678900"

gerenciadorVazio :: GerenciadorFerias
gerenciadorVazio = GerenciadorFerias Map.empty

dataAdmissaoTeste :: Day
dataAdmissaoTeste = fromGregorian 2023 1 1

inicioFeriasTeste :: Day
inicioFeriasTeste = fromGregorian 2024 1 5

fimFeriasTeste :: Day
fimFeriasTeste = fromGregorian 2024 1 19

hojeTeste :: Day
hojeTeste = fromGregorian 2024 1 5

d :: Integer -> Int -> Int -> Day
d = fromGregorian

aplicarFaltas :: RegistroFeriasFuncionario -> [Day] -> Either String RegistroFeriasFuncionario
aplicarFaltas reg [] = Right reg
aplicarFaltas reg (d:ds) =
  case registrarFalta d reg of
    Left err   -> Left err
    Right reg' -> aplicarFaltas reg' ds

feriasValidas :: Ferias
feriasValidas =
  Ferias
    { inicioFerias = inicioFeriasTeste
    , fimFerias = fimFeriasTeste
    , diasUtilizados = 14
    , statusFerias = Planejada
    }

admitirFuncionarioTeste :: IO ()
admitirFuncionarioTeste =
  case admitirFuncionario cpfTeste dataAdmissaoTeste gerenciadorVazio of
    Left _ ->
      assert False "Erro ao admitir funcionário."
    Right _ ->
      assert True "Funcionário admitido corretamente."

registrarFeriasValidasTeste :: IO ()
registrarFeriasValidasTeste = do
  let Right ger1 =
        admitirFuncionario cpfTeste dataAdmissaoTeste gerenciadorVazio

  case registrarFerias cpfTeste feriasValidas hojeTeste ger1 of
    Left err ->
      assert False ("Erro ao registrar férias válidas: " ++ err)

    Right ger2 ->
      let Just reg =
            Map.lookup cpfTeste (registros ger2)

          cicloAtual =
            head (ciclos reg)

      in assert
          (length (feriasDoCiclo cicloAtual) == 1)
          "Férias válidas registradas corretamente."

feriasAntesPeriodoAquisitivoTeste :: IO ()
feriasAntesPeriodoAquisitivoTeste = do
  let Right ger1 =
        admitirFuncionario cpfTeste dataAdmissaoTeste gerenciadorVazio

      feriasInvalidas =
        feriasValidas
          { inicioFerias = fromGregorian 2023 6 1
          , fimFerias    = fromGregorian 2023 6 14
          }

  case registrarFerias cpfTeste feriasInvalidas hojeTeste ger1 of
    Left _ ->
      assert True "Bloqueou férias antes do período aquisitivo."
    Right _ ->
      assert False "Erro: permitiu férias antes do período aquisitivo."

temDireitoAFeriasTeste :: IO ()
temDireitoAFeriasTeste = do
  let Right ger1 =
        admitirFuncionario cpfTeste dataAdmissaoTeste gerenciadorVazio

      Just reg =
        Map.lookup cpfTeste (registros ger1)

      cicloAtual =
        head (ciclos reg)

      fimAquisitivoTeste =
        fimAquisitivo (periodoAquisitivo cicloAtual)

  assert
    (temDireitoAFerias fimAquisitivoTeste cicloAtual)
    "Reconheceu corretamente o direito às férias após o período aquisitivo."

feriasAntesDoAquisitivoTeste :: IO ()
feriasAntesDoAquisitivoTeste = do
  let admissao = d 2023 1 1
      dataRef  = d 2023 10 1
      ferias   = Ferias (d 2023 10 1) (d 2023 10 15) 15 Planejada
      Right ger1 =
        admitirFuncionario "111" admissao (GerenciadorFerias Map.empty)

  case registrarFerias "111" ferias dataRef ger1 of
    Left _  -> assert True "Bloqueou férias antes do período aquisitivo."
    Right _ -> assert False "Erro: permitiu férias sem direito adquirido."

fracionamentoSemPeriodoMinimoTeste :: IO ()
fracionamentoSemPeriodoMinimoTeste = do
  let admissao = d 2022 1 1
      dataRef  = d 2023 2 1

      f1 = Ferias (d 2023 2 1) (d 2023 2 14) 14 Planejada
      f2 = Ferias (d 2023 3 1) (d 2023 3 7)  7  Planejada
      f3 = Ferias (d 2023 4 1) (d 2023 4 7)  3  Planejada

      Right ger1 =
        admitirFuncionario "222" admissao (GerenciadorFerias Map.empty)
      Right ger2 = registrarFerias "222" f1 dataRef ger1
      Right ger3 = registrarFerias "222" f2 dataRef ger2

  case registrarFerias "222" f3 dataRef ger3 of
    Left _  -> assert True "Bloqueou fracionamento sem período mínimo de 5 dias."
    Right _ -> assert False "Erro: permitiu fracionamento ilegal."


sobreposicaoFeriasTeste :: IO ()
sobreposicaoFeriasTeste = do
  let admissao = d 2022 1 1
      dataRef  = d 2023 5 1

      f1 = Ferias (d 2023 6 1)  (d 2023 6 20) 20 Planejada
      f2 = Ferias (d 2023 6 15) (d 2023 6 25) 11 Planejada

      Right ger1 =
        admitirFuncionario "333" admissao (GerenciadorFerias Map.empty)
      Right ger2 = registrarFerias "333" f1 dataRef ger1

  case registrarFerias "333" f2 dataRef ger2 of
    Left _  -> assert True "Detectou sobreposição de períodos de férias."
    Right _ -> assert False "Erro: permitiu férias sobrepostas."

feriasVencidasTeste :: IO ()
feriasVencidasTeste = do
  let admissao = d 2020 1 1
      dataRef  = d 2023 3 1
      ciclo    = head (gerarCiclos admissao)

  assert (feriasVencidas dataRef ciclo)
    "Identificou corretamente férias vencidas."

faltasReduzemSaldoTeste :: IO ()
faltasReduzemSaldoTeste = do
  let admissao  = d 2023 1 1
      inicio    = d 2023 6 1
      diasFalta = take 10 (iterate (addDays 1) inicio)

      Right ger1 =
        admitirFuncionario "444" admissao (GerenciadorFerias Map.empty)

      Right reg1 =
        aplicarFaltas (registros ger1 Map.! "444") diasFalta

      ciclo = head (ciclos reg1)

  assert (saldoDias ciclo == 24)
    "Faltas reduziram corretamente o saldo de férias."

estouroDeSaldoTeste :: IO ()
estouroDeSaldoTeste = do
  let admissao = d 2022 1 1
      dataRef  = d 2023 2 1
      ferias   = Ferias (d 2023 3 1) (d 2023 4 10) 40 Planejada

      Right ger1 =
        admitirFuncionario "555" admissao (GerenciadorFerias Map.empty)

  case registrarFerias "555" ferias dataRef ger1 of
    Left _  -> assert True "Bloqueou férias acima do saldo disponível."
    Right _ -> assert False "Erro: permitiu estourar o saldo de férias."


runFeriasTests :: IO ()
runFeriasTests = do
  putStrLn "\n--- Testes de Férias ---"

  admitirFuncionarioTeste
  registrarFeriasValidasTeste
  feriasAntesPeriodoAquisitivoTeste
  temDireitoAFeriasTeste
  feriasAntesDoAquisitivoTeste
  fracionamentoSemPeriodoMinimoTeste
  sobreposicaoFeriasTeste
  feriasVencidasTeste
  faltasReduzemSaldoTeste
  estouroDeSaldoTeste


