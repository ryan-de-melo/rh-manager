module Controller.ControladorPresenca where

import Model.TiposDados
import Data.Time(Day, TimeOfDay, timeOfDayToTime)
import qualified Data.Map as Map

registrarPresenca :: CPF -> Day -> Presenca -> SistemaDePresenca -> Either String SistemaDePresenca
registrarPresenca cpf dia presenca sistema
  | verificarPresenca cpf dia sistema = Left "Já existe presença registrada para este funcionário neste dia."
  | otherwise = validarPresenca presenca >> Right (inserirPresenca cpf dia presenca sistema)

inserirPresenca :: CPF -> Day -> Presenca -> SistemaDePresenca -> SistemaDePresenca
inserirPresenca cpf dia presenca sistema =
  sistema { presencasRegistradas = Map.alter atualiza cpf (presencasRegistradas sistema) }
  where
    atualiza Nothing = Just (Map.singleton dia presenca)
    atualiza (Just mapaDias) = Just (Map.insert dia presenca mapaDias)

validarPresenca :: Presenca -> Either String ()
validarPresenca p
  | not (compareceu p) && null (justificativa p) = Left "Funcionário ausente deve possuir justificativa."
  | compareceu p && not (null (justificativa p)) = Left "Funcionário que compareceu não deve possuir justificativa."
  | not (compareceu p) && checkIn p /= checkOut p = Left "Funcionário ausente não pode ter horário de entrada ou saída."
  | compareceu p && checkOut p <= checkIn p = Left "Horário de saída deve ser posterior ao horário de entrada."
  | otherwise = Right ()

buscarPresenca :: CPF -> Day -> SistemaDePresenca -> Maybe Presenca
buscarPresenca cpf dia sistema =
  case Map.lookup cpf (presencasRegistradas sistema) of
    Nothing -> Nothing
    Just mapaDias -> Map.lookup dia mapaDias

verificarPresenca :: CPF -> Day -> SistemaDePresenca -> Bool
verificarPresenca cpf dia sistema =
  case buscarPresenca cpf dia sistema of
    Nothing -> False
    Just _  -> True

compareceuNoDia :: CPF -> Day -> SistemaDePresenca -> Bool
compareceuNoDia cpf dia sistema =
  maybe False compareceu (buscarPresenca cpf dia sistema)

horasTrabalhadas :: CPF -> Day -> Day -> SistemaDePresenca -> Double
horasTrabalhadas cpf inicio fim sistema =
  case Map.lookup cpf (presencasRegistradas sistema) of
    Nothing -> 0
    Just mapaDias -> sum [ horasDoDia p | (dia, p) <- Map.toList mapaDias, dia >= inicio, dia <= fim, compareceu p]

horasDoDia :: Presenca -> Double
horasDoDia p =
  realToFrac (timeOfDayToTime (checkOut p) - timeOfDayToTime (checkIn p)) / 3600

exibirRegistroPresencas :: CPF -> SistemaDePresenca -> IO ()
exibirRegistroPresencas cpf sistema =
  case Map.lookup cpf (presencasRegistradas sistema) of
    Nothing -> putStrLn "Nenhum registro de presença encontrado para este CPF."
    Just mapaDias -> mapM_ exibirPresenca (Map.toList mapaDias)

exibirPresenca :: (Day, Presenca) -> IO ()
exibirPresenca (dia, p)
  | compareceu p = putStrLn $ show dia ++ " - compareceu"
  | otherwise = putStrLn $ show dia ++ " - ausente - justificativa: " ++ justificativa p
