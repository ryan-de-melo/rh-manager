module Presence (runPresenceTests) where

import Controller.ControladorPresenca
import Data.Time (fromGregorian, TimeOfDay(..), Day)
import qualified Data.Map as Map
import Model.TiposDados

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

cpfTeste, cpfOutro :: CPF
cpfTeste = "12345678900"
cpfOutro = "99988877766"

dia1, dia2, dia3 :: Day
dia1 = fromGregorian 2025 1 10
dia2 = fromGregorian 2025 1 11
dia3 = fromGregorian 2025 1 12

entrada, saida :: TimeOfDay
entrada = TimeOfDay 8 0 0
saida   = TimeOfDay 17 0 0

sistemaVazio :: SistemaDePresenca
sistemaVazio = SistemaDePresenca Map.empty

presencaValida :: Presenca
presencaValida =
  Presenca 1 Presencial entrada saida True ""

presencaAusenteJustificada :: Presenca
presencaAusenteJustificada =
  Presenca 1 Presencial entrada entrada False "Atestado médico"

presencaAusenteSemJustificativa :: Presenca
presencaAusenteSemJustificativa =
  Presenca 1 Presencial entrada entrada False ""

presencaCompareceuComJustificativa :: Presenca
presencaCompareceuComJustificativa =
  Presenca 1 Presencial entrada saida True "Texto inválido"

presencaHorarioInvalido :: Presenca
presencaHorarioInvalido =
  Presenca 1 Presencial saida entrada True ""

registrarPresencaValidaTeste :: IO ()
registrarPresencaValidaTeste = do
  case registrarPresenca cpfTeste dia1 presencaValida sistemaVazio of
    Left _ -> assert False "Falhou ao registrar presença válida."
    Right sistema -> assert (verificarPresenca cpfTeste dia1 sistema)
        "Presença registrada corretamente."

registrarPresencaDuplicadaTeste :: IO ()
registrarPresencaDuplicadaTeste = do
  let Right sistema1 = registrarPresenca cpfTeste dia1 presencaValida sistemaVazio
  case registrarPresenca cpfTeste dia1 presencaValida sistema1 of
    Left _ -> assert True "Bloqueou registro duplicado corretamente."
    Right _ -> assert False "Erro: permitiu duplicar presença."

presencaAusenteSemJustificativaTeste :: IO ()
presencaAusenteSemJustificativaTeste = do
  case validarPresenca presencaAusenteSemJustificativa of
    Left _ -> assert True "Validação funcionou corretamente."
    Right _ -> assert False "Erro: aceitou ausência sem justificativa."

presencaCompareceuComJustificativaTeste :: IO ()
presencaCompareceuComJustificativaTeste = do
  case validarPresenca presencaCompareceuComJustificativa of
    Left _ -> assert True "Validação bloqueou corretamente."
    Right _ -> assert False "Erro: aceitou justificativa indevida."

presencaHorarioInvalidoTeste :: IO ()
presencaHorarioInvalidoTeste = do
  case validarPresenca presencaHorarioInvalido of
    Left _ -> assert True "Validação bloqueou horário inválido."
    Right _ -> assert False "Erro: aceitou horário inválido."

buscarPresencaExistenteTeste :: IO ()
buscarPresencaExistenteTeste = do
  let Right sistema = registrarPresenca cpfTeste dia1 presencaValida sistemaVazio
  case buscarPresenca cpfTeste dia1 sistema of
    Just _ -> assert True "Presença encontrada corretamente."
    Nothing -> assert False "Erro: não encontrou presença existente."

buscarPresencaInexistenteTeste :: IO ()
buscarPresencaInexistenteTeste = do
  case buscarPresenca cpfTeste dia1 sistemaVazio of
    Nothing -> assert True "Retornou Nothing corretamente."
    Just _ -> assert False "Erro: encontrou presença inexistente."

verificarPresencaTeste :: IO ()
verificarPresencaTeste = do
  let Right sistema = registrarPresenca cpfTeste dia1 presencaValida sistemaVazio
  assert (verificarPresenca cpfTeste dia1 sistema)
    "verificarPresenca retornou True corretamente."

compareceuNoDiaTeste :: IO ()
compareceuNoDiaTeste = do
  let Right sistema = registrarPresenca cpfTeste dia1 presencaValida sistemaVazio
  assert (compareceuNoDia cpfTeste dia1 sistema)
    "compareceuNoDia identificou comparecimento corretamente."

compareceuNoDiaAusenteTeste :: IO ()
compareceuNoDiaAusenteTeste = do
  let Right sistema = registrarPresenca cpfTeste dia1 presencaAusenteJustificada sistemaVazio
  assert (not (compareceuNoDia cpfTeste dia1 sistema))
    "compareceuNoDia retornou False para ausência."

horasTrabalhadasIntervaloTeste :: IO ()
horasTrabalhadasIntervaloTeste = do
  let Right s1 = registrarPresenca cpfTeste dia1 presencaValida sistemaVazio
  let Right s2 = registrarPresenca cpfTeste dia2 presencaValida s1
  let horas = horasTrabalhadas cpfTeste dia1 dia2 s2
  assert (horas == 18.0)
    "Horas trabalhadas calculadas corretamente."

horasTrabalhadasIgnoraAusenteTeste :: IO ()
horasTrabalhadasIgnoraAusenteTeste = do
  let Right s1 = registrarPresenca cpfTeste dia1 presencaValida sistemaVazio
  let Right s2 = registrarPresenca cpfTeste dia2 presencaAusenteJustificada s1
  let horas = horasTrabalhadas cpfTeste dia1 dia2 s2
  assert (horas == 9.0)
    "Ausência não somou horas."

horasTrabalhadasSemRegistrosTeste :: IO ()
horasTrabalhadasSemRegistrosTeste = do
  let horas = horasTrabalhadas cpfTeste dia1 dia3 sistemaVazio
  assert (horas == 0)
    "Sem registros retornou 0 horas."

runPresenceTests :: IO ()
runPresenceTests = do
  putStrLn "\n--- Testes de Presence ---"

  registrarPresencaValidaTeste
  registrarPresencaDuplicadaTeste
  presencaAusenteSemJustificativaTeste
  presencaCompareceuComJustificativaTeste
  presencaHorarioInvalidoTeste
  buscarPresencaExistenteTeste
  buscarPresencaInexistenteTeste
  verificarPresencaTeste
  compareceuNoDiaTeste
  compareceuNoDiaAusenteTeste
  horasTrabalhadasIntervaloTeste
  horasTrabalhadasIgnoraAusenteTeste
  horasTrabalhadasSemRegistrosTeste
