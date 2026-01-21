module Util.FeriasService where

import Controller.GerenciarFerias
import Model.TiposDados
import Util.Utilitarios
import Data.Time
import qualified Data.Map as Map
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

-------------------------------------------------
-- SISTEMA
-------------------------------------------------

iniciarSistemaFerias :: IO ()
iniciarSistemaFerias = loop (GerenciadorFerias Map.empty)

loop :: GerenciadorFerias -> IO ()
loop ger = do
  cabecalho "MENU PRINCIPAL"
  putStrLn "1 - Admitir funcionário"
  putStrLn "2 - Registrar férias"
  putStrLn "3 - Registrar falta"
  putStrLn "4 - Consultar férias"
  putStrLn "0 - Sair"
  putStrLn ""

  opcao <- lerLinha "Selecione uma opção"

  case opcao of
    "1" -> admitirUI ger >>= finalizar
    "2" -> registrarFeriasUI ger >>= finalizar
    "3" -> registrarFaltaUI ger >>= finalizar
    "4" -> exibirFeriasUI ger >>= finalizar
    "0" -> putStrLn "\nSistema encerrado."
    _   -> msgErro "Opção inválida." >> pause >> loop ger
  where
    finalizar g = pause >> loop g

-------------------------------------------------
-- UI HELPERS
-------------------------------------------------

cabecalho :: String -> IO ()
cabecalho titulo = do
  clearScreen
  putStrLn "========================================"
  putStrLn "        SISTEMA DE FERIAS - RH"
  putStrLn "========================================"
  putStrLn (" " ++ titulo)
  putStrLn "----------------------------------------"
  putStrLn ""

msgErro, msgSucesso, msgInfo :: String -> IO ()
msgErro s    = putStrLn ("\n[ERRO] " ++ s)
msgSucesso s = putStrLn ("\n[OK] " ++ s)
msgInfo s    = putStrLn ("\n[INFO] " ++ s)

confirmar :: String -> IO Bool
confirmar msg = do
  r <- lerLinha (msg ++ " (S/N)")
  return (r == "S" || r == "s")

-------------------------------------------------
-- LEITURAS
-------------------------------------------------

lerLinha :: String -> IO String
lerLinha msg = do
  putStrLn msg
  putStr "> "
  hFlush stdout
  getLine

lerCPF :: IO CPF
lerCPF = lerLinha "CPF"

lerData :: String -> IO Day
lerData msg = do
  entrada <- lerLinha (msg ++ " (AAAA-MM-DD)")
  case readMaybe entrada of
    Just d  -> return d
    Nothing -> msgErro "Data inválida." >> lerData msg

lerInt :: String -> IO Int
lerInt msg = do
  entrada <- lerLinha msg
  case readMaybe entrada of
    Just n  -> return n
    Nothing -> msgErro "Número inválido." >> lerInt msg

lerSN :: String -> IO Bool
lerSN msg = do
  r <- lerLinha (msg ++ " (S/N)")
  case r of
    "S" -> return True
    "s" -> return True
    "N" -> return False
    "n" -> return False
    _   -> msgErro "Digite S ou N." >> lerSN msg

-------------------------------------------------
-- ADMISSÃO
-------------------------------------------------

admitirUI :: GerenciadorFerias -> IO GerenciadorFerias
admitirUI ger = do
  cabecalho "ADMISSÃO DE FUNCIONÁRIO"
  cpf <- lerCPF
  admissao <- lerData "Data de admissão"

  confirmar "Confirmar admissão?" >>= \ok ->
    if not ok
      then msgInfo "Operação cancelada." >> return ger
      else
        case admitirFuncionario cpf admissao ger of
          Left err -> msgErro err >> return ger
          Right g  -> msgSucesso "Funcionário admitido com sucesso." >> return g

-------------------------------------------------
-- REGISTRO DE FÉRIAS
-------------------------------------------------

registrarFeriasUI :: GerenciadorFerias -> IO GerenciadorFerias
registrarFeriasUI ger = do
  cabecalho "REGISTRO DE FÉRIAS"
  cpf <- lerCPF
  fracionada <- lerSN "Férias fracionadas?"

  qtd <- if fracionada then lerQuantidadePeriodos else return 1
  periodos <- mapM lerPeriodo [1 .. qtd]

  confirmar "Confirmar registro das férias?" >>= \ok ->
    if not ok
      then msgInfo "Registro cancelado." >> return ger
      else do
        hoje <- utctDay <$> getCurrentTime
        registrarSequencial cpf periodos hoje ger

lerQuantidadePeriodos :: IO Int
lerQuantidadePeriodos = do
  n <- lerInt "Quantidade de períodos (2 ou 3)"
  if n < 2 || n > 3
    then msgErro "Quantidade inválida." >> lerQuantidadePeriodos
    else return n

lerPeriodo :: Int -> IO (Day, Day)
lerPeriodo i = do
  putStrLn ("\nPeríodo " ++ show i)
  ini <- lerData "Data de início"
  fim <- lerData "Data de fim"
  return (ini, fim)

-------------------------------------------------
-- REGISTRO DE FALTA
-------------------------------------------------

registrarFaltaUI :: GerenciadorFerias -> IO GerenciadorFerias
registrarFaltaUI ger = do
  cabecalho "REGISTRO DE FALTA"
  cpf <- lerCPF
  dataFalta <- lerData "Data da falta"

  case Map.lookup cpf (registros ger) of
    Nothing -> msgErro "Funcionário não encontrado." >> return ger
    Just reg ->
      case registrarFalta dataFalta reg of
        Left err -> msgErro err >> return ger
        Right regAtual -> do
          msgSucesso "Falta registrada com sucesso."
          return ger { registros = Map.insert cpf regAtual (registros ger) }

-------------------------------------------------
-- CONSULTA DE FÉRIAS
-------------------------------------------------

exibirFeriasUI :: GerenciadorFerias -> IO GerenciadorFerias
exibirFeriasUI ger = do
  cabecalho "CONSULTA DE FÉRIAS"
  cpf <- lerCPF
  hoje <- utctDay <$> getCurrentTime

  case Map.lookup cpf (registros ger) of
    Nothing -> msgErro "Funcionário não encontrado."
    Just reg -> do
      let ciclosAtualizados =
            map (atualizarCiclo hoje)
              (takeWhile
                (\c -> inicioAquisitivo (periodoAquisitivo c) <= hoje)
                (ciclos reg))

          ciclosExibidos = selecionarCiclosExibicao ciclosAtualizados

      if null ciclosExibidos
        then msgInfo "Nenhum ciclo relevante encontrado."
        else mapM_ exibirFeriasPersonalizado ciclosExibidos

  return ger

-------------------------------------------------
-- EXIBIÇÃO
-------------------------------------------------

exibirFeriasPersonalizado :: CicloFerias -> IO ()
exibirFeriasPersonalizado c = do
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn ("Ciclo aquisitivo : " ++ show iniAq ++ " / " ++ show fimAq)
  putStrLn ("Ciclo concessivo : " ++ show iniCon ++ " / " ++ show fimCon)
  putStrLn ("Status do ciclo  : " ++ show (statusCiclo c))
  putStrLn ("Saldo disponível : " ++ show (saldoDias c) ++ " dias")
  putStrLn ""
  putStrLn "Férias registradas:"

  if null (feriasDoCiclo c)
    then putStrLn "  Nenhuma férias registrada neste ciclo."
    else mapM_ exibirFeriasDetalhada (feriasDoCiclo c)
  where
    pa = periodoAquisitivo c
    pc = periodoConcessivo c
    iniAq = inicioAquisitivo pa
    fimAq = fimAquisitivo pa
    iniCon = inicioConcessivo pc
    fimCon = fimConcessivo pc

exibirFeriasDetalhada :: Ferias -> IO ()
exibirFeriasDetalhada f =
  putStrLn $
    "  Período: " ++
    show (inicioFerias f) ++ " até " ++
    show (fimFerias f) ++
    " (" ++ show (diasUtilizados f) ++ " dias) - " ++
    show (statusFerias f)

-------------------------------------------------
-- FILTRO CORRETO DE CICLOS
-------------------------------------------------

selecionarCiclosExibicao :: [CicloFerias] -> [CicloFerias]
selecionarCiclosExibicao ciclos =
  let (comFerias, semFerias) =
        span (not . null . feriasDoCiclo) ciclos
  in comFerias ++ take 1 semFerias

-------------------------------------------------
-- APOIO
-------------------------------------------------

criarFerias :: Day -> Day -> Ferias
criarFerias ini fim =
  Ferias ini fim (fromIntegral (diffDays fim ini) + 1) Planejada

registrarSequencial :: CPF -> [(Day, Day)] -> Day -> GerenciadorFerias -> IO GerenciadorFerias
registrarSequencial _ [] _ ger =
  msgSucesso "Férias registradas com sucesso." >> return ger

registrarSequencial cpf ((ini, fim):xs) hoje ger = do
  let ferias = criarFerias ini fim
  case registrarFerias cpf ferias hoje ger of
    Left err -> msgErro err >> return ger
    Right g  -> registrarSequencial cpf xs hoje g
