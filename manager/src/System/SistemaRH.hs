module System.SistemaRH where

import Controller.GerenciaFuncionarios
import Controller.GerenciaDepartamentos
import Controller.GerenciaCargos
import Controller.Afastamento
import Controller.GerenciaFerias
import Controller.ControladorPresenca
import Controller.JornadaLicenca
import Controller.GerenciaLicencaEJornadas
import Model.TiposDados
import Util.Utilitarios
import Data.Time
import qualified Data.Map as Map
import Data.List(find)
import Model.TiposDados (Afastamento(idAfastamento))

-------------------------------------------------
-- SISTEMA PRINCIPAL
-------------------------------------------------

iniciarSistemaRH :: IO ()
iniciarSistemaRH =
  loop (SistemaBancoDadosRH [] [] [] [] sistemaJornadaVazio (GerenciadorFerias Map.empty) (SistemaDePresenca Map.empty))

loop :: SistemaBancoDadosRH -> IO ()
loop sistema = do
  cabecalho "MENU - SISTEMA DE BANCO DE DADOS RH"

  putStrLn "1 - Gerenciar funcionários"
  putStrLn "2 - Gerenciar departamentos"
  putStrLn "3 - Gerenciar cargos"
  putStrLn "4 - Licenças e Jornadas"
  putStrLn "5 - Férias"
  putStrLn "6 - Presença"
  putStrLn "0 - Sair\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> menuFuncionariosLoop sistema >>= loop
    "2" -> menuDepartamentosLoop sistema >>= loop
    "3" -> menuCargosLoop sistema >>= loop
    "4" -> menuJornadaLicencaLoop sistema >>= loop
    "5" -> menuFeriasLoop sistema >>= loop
    "6" -> menuPresencaLoop sistema >>= loop
    "0" -> putStrLn "\nSistema encerrado."
    _   -> msgErro "Opção inválida." >> pause >> loop sistema

-------------------------------------------------
-- MENU FUNCIONÁRIOS
-------------------------------------------------

menuFuncionariosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuFuncionariosLoop sistema = do
  cabecalho "MENU - FUNCIONÁRIOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "6 - Registrar afastamento"
  putStrLn "7 - Encerrar afastamento"
  putStrLn "8 - Listar funcionários afastados"
  putStrLn "9 - Desligar funcionário"
  putStrLn "10 - Listar funcionários desligados"
  putStrLn "0 - Voltar\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1"  -> cadastrarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "2"  -> alterarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "3"  -> buscarFuncionarioUI sistema >> pause >> menuFuncionariosLoop sistema
    "4"  -> excluirFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "5"  -> listarFuncionariosUI sistema >> pause >> menuFuncionariosLoop sistema
    "6"  -> registrarAfastamentoUI sistema >>= pausaEVolta menuFuncionariosLoop
    "7"  -> encerrarAfastamentoUI sistema >>= pausaEVolta menuFuncionariosLoop
    "8"  -> listarAfastadosUI sistema >> pause >> menuFuncionariosLoop sistema
    "9"  -> desligarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "10" -> listarDesligadosUI sistema >> pause >> menuFuncionariosLoop sistema
    "0"  -> return sistema
    _    -> msgErro "Opção inválida." >> pause >> menuFuncionariosLoop sistema

-------------------------------------------------
-- MENU DEPARTAMENTOS
-------------------------------------------------

menuDepartamentosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuDepartamentosLoop sistema = do
  cabecalho "MENU - DEPARTAMENTOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "0 - Voltar\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> cadastrarDepartamentoUI sistema >>= pausaEVolta menuDepartamentosLoop
    "2" -> alterarDepartamentoUI sistema >>= pausaEVolta menuDepartamentosLoop
    "3" -> buscarDepartamentoUI sistema >> pause >> menuDepartamentosLoop sistema
    "4" -> excluirDepartamentoUI sistema >>= pausaEVolta menuDepartamentosLoop
    "5" -> listarDepartamentosUI sistema >> pause >> menuDepartamentosLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuDepartamentosLoop sistema

-------------------------------------------------
-- MENU CARGOS
-------------------------------------------------

menuCargosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuCargosLoop sistema = do
  cabecalho "MENU - CARGOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "0 - Voltar\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> cadastrarCargoUI sistema >>= pausaEVolta menuCargosLoop
    "2" -> alterarCargoUI sistema >>= pausaEVolta menuCargosLoop
    "3" -> buscarCargoUI sistema >> pause >> menuCargosLoop sistema
    "4" -> excluirCargoUI sistema >>= pausaEVolta menuCargosLoop
    "5" -> listarCargosUI sistema >> pause >> menuCargosLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuCargosLoop sistema

-------------------------------------------------
-- MENU LICENÇAS E JORNADAS
-------------------------------------------------

menuJornadaLicencaLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuJornadaLicencaLoop sistema = do
  cabecalho "MENU - LICENÇAS E JORNADAS"

  putStrLn "1 - Registrar licença"
  putStrLn "2 - Remover licença"
  putStrLn "3 - Listar licenças"
  putStrLn "4 - Criar escala semanal"
  putStrLn "5 - Listar jornadas"
  putStrLn "6 - Consultar folga do funcionário"
  putStrLn "0 - Voltar\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> registrarLicencaUI sistema >>= pausaEVolta menuJornadaLicencaLoop
    "2" -> removerLicencaUI sistema >>= pausaEVolta menuJornadaLicencaLoop
    "3" -> listarLicencasUI sistema >> pause >> menuJornadaLicencaLoop sistema
    "4" -> criarEscalaUI sistema >>= pausaEVolta menuJornadaLicencaLoop
    "5" -> listarJornadasUI sistema >> pause >> menuJornadaLicencaLoop sistema
    "6" -> consultarFolgaUI sistema >> pause >> menuJornadaLicencaLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuJornadaLicencaLoop sistema

-------------------------------------------------
-- MENU FÉRIAS
-------------------------------------------------

menuFeriasLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuFeriasLoop sistema = do
  cabecalho "MENU - FÉRIAS"
  putStrLn "1 - Registrar férias"
  putStrLn "2 - Consultar férias"
  putStrLn "3 - Verificar direito a férias"
  putStrLn "0 - Voltar\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> registrarFeriasUI (sistemaFerias sistema) >>= \g ->
             return sistema { sistemaFerias = g } >>= pausaEVolta menuFeriasLoop
    "2" -> exibirFeriasUI (sistemaFerias sistema) >>= \g ->
             return sistema { sistemaFerias = g } >>= pausaEVolta menuFeriasLoop
    "3" -> verificarDireitoUI (sistemaFerias sistema) >> pause >> menuFeriasLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuFeriasLoop sistema

menuPresencaLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuPresencaLoop sistema = do
  cabecalho "MENU - PRESENÇA"
  putStrLn "1 - Registrar presença"
  putStrLn "2 - Consultar presença por dia"
  putStrLn "3 - Exibir histórico de presenças"
  putStrLn "4 - Calcular horas trabalhadas"
  putStrLn "5 - Calcular faltas injustificadas"
  putStrLn "0 - Voltar\n"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> registrarPresencaUI sistema >>= \s ->
             pausaEVolta menuPresencaLoop s
    "2" -> consultarPresencaUI sistema >> pause >> menuPresencaLoop sistema
    "3" -> exibirHistoricoUI sistema >> pause >> menuPresencaLoop sistema
    "4" -> calcularHorasUI sistema >> pause >> menuPresencaLoop sistema
    "5" -> calcularFaltasUI sistema >> pause >> menuPresencaLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuPresencaLoop sistema


-------------------------------------------------
-- FUNCIONÁRIOS - UI
-------------------------------------------------

cadastrarFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
cadastrarFuncionarioUI sistema = do
  cabecalho "CADASTRAR FUNCIONÁRIO"
  funcionario <- lerFuncionario

  let resultadoFuncionario =
        adicionarFuncionarioValidado
          funcionario
          (cargos sistema)
          (departamento sistema)
          (funcionarios sistema)

  case resultadoFuncionario of
    Left err -> msgErro err >> return sistema
    Right novaLista -> do
      let cpf = idFunc funcionario
          admissao = dataAdmissaoFunc funcionario
          gerAtual = sistemaFerias sistema
          presAtual = sistemaPresenca sistema
          -- registra presença inicial vazia
          presAtualizado = presAtual
            { presencasRegistradas = Map.insert cpf Map.empty (presencasRegistradas presAtual) }
          gerAtualizadoE = admitirFuncionario cpf admissao gerAtual

      case gerAtualizadoE of
        Left errFerias ->
          msgErro ("Erro ao registrar férias: " ++ errFerias) >>
          return sistema { funcionarios = novaLista
                         , sistemaPresenca = presAtualizado } -- presença já registrada
        Right gerAtualizado -> do
          msgSucesso "Funcionário cadastrado e registrado nos sistemas de férias e presença."
          return sistema { funcionarios = novaLista
                         , sistemaFerias = gerAtualizado
                         , sistemaPresenca = presAtualizado }




alterarFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
alterarFuncionarioUI sistema = do
  cabecalho "ALTERAR FUNCIONÁRIO"
  funcionario <- lerFuncionario

  case modificarFuncionario funcionario (funcionarios sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Funcionário alterado."
        >> return sistema { funcionarios = novaLista }

buscarFuncionarioUI :: SistemaBancoDadosRH -> IO ()
buscarFuncionarioUI sistema = do
  cabecalho "BUSCAR FUNCIONÁRIO"
  cpf <- lerCPF

  case buscarFuncionario cpf (funcionarios sistema) of
    Nothing -> msgInfo "Funcionário não encontrado."
    Just f  -> exibirFuncionario f

excluirFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
excluirFuncionarioUI sistema = do
  cabecalho "EXCLUIR FUNCIONÁRIO"
  cpf <- lerCPF

  case excluirFuncionario cpf (funcionarios sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Funcionário removido."
        >> return sistema { funcionarios = novaLista }

listarFuncionariosUI :: SistemaBancoDadosRH -> IO ()
listarFuncionariosUI sistema =
  if null (funcionarios sistema)
    then msgInfo "Nenhum funcionário cadastrado."
    else mapM_ exibirFuncionario (funcionarios sistema)


-------------------------------------------------
-- LICENÇAS - UI
-------------------------------------------------

registrarLicencaUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
registrarLicencaUI sistema = do
  cabecalho "REGISTRAR LICENÇA"

  cpf <- lerCPF
  tipo <- lerTipoLicenca
  ini  <- lerData "Data início"
  fim  <- lerData "Data fim"
  desc <- lerLinha "Descrição"

  let lic = Licenca cpf tipo ini fim desc
      sj  = sistemaJornada sistema

  case adicionaLicenca undefined lic sj of
    Left err -> msgErro err >> return sistema
    Right sj' ->
      msgSucesso "Licença registrada."
        >> return sistema { sistemaJornada = sj' }

removerLicencaUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
removerLicencaUI sistema = do
  cabecalho "REMOVER LICENÇA"

  cpf <- lerCPF
  ini <- lerData "Data início da licença"

  let sj = sistemaJornada sistema
      licEncontrada =
        find (\l -> funcionarioLicenca l == cpf && dataInicio l == ini)
             (licencas sj)

  case licEncontrada of
    Nothing -> msgErro "Licença não encontrada." >> return sistema
    Just lic ->
      case removerLicenca lic sj of
        Left err -> msgErro err >> return sistema
        Right sj' ->
          msgSucesso "Licença removida."
            >> return sistema { sistemaJornada = sj' }

listarLicencasUI :: SistemaBancoDadosRH -> IO ()
listarLicencasUI sistema =
  let ls = licencas (sistemaJornada sistema)
  in if null ls
        then msgInfo "Nenhuma licença cadastrada."
        else mapM_ print ls

-------------------------------------------------
-- JORNADAS - UI
-------------------------------------------------

criarEscalaUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
criarEscalaUI sistema = do
  cabecalho "CRIAR ESCALA SEMANAL"

  cpf <- lerCPF
  qtd <- lerInt "Qtd de dias trabalhados"
  dias <- sequence (replicate qtd (lerData "Dia"))
  ini  <- lerInt "Hora entrada"
  fim  <- lerInt "Hora saída"
  ult  <- lerData "Última folga"
  prox <- lerData "Próxima folga"

  case criaEscala cpf dias ini fim ult prox of
    Nothing -> msgErro "Escala inválida." >> return sistema
    Just escala ->
      case adicionaJornada escala (sistemaJornada sistema) of
        Left err -> msgErro err >> return sistema
        Right sj' ->
          msgSucesso "Escala criada."
            >> return sistema { sistemaJornada = sj' }

listarJornadasUI :: SistemaBancoDadosRH -> IO ()
listarJornadasUI sistema =
  let js = jornadasSemanais (sistemaJornada sistema)
  in if null js
        then msgInfo "Nenhuma jornada cadastrada."
        else mapM_ print js

consultarFolgaUI :: SistemaBancoDadosRH -> IO ()
consultarFolgaUI sistema = do
  cabecalho "CONSULTAR FOLGA"

  cpf <- lerCPF
  case buscaFolgaDoFuncionario cpf (sistemaJornada sistema) of
    Nothing -> msgInfo "Nenhuma folga encontrada."
    Just d  -> putStrLn ("Próxima folga: " ++ show d)


-------------------------------------------------
-- AFASTAMENTOS - UI
-------------------------------------------------

registrarAfastamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
registrarAfastamentoUI sistema = do
  cabecalho "REGISTRAR AFASTAMENTO"

  hoje <- utctDay <$> getCurrentTime
  cpf  <- lerCPF
  tipo <- lerTipoAfastamento
  ini  <- lerData "Data início"
  fim  <- lerData "Data fim"
  desc <- lerLinha "Descrição"

  doc <- obterDocumentacaoObrigatoria tipo

  let resultado =
        registrarAfastamento
          hoje
          cpf
          tipo
          ini
          fim
          desc
          doc
          (funcionarios sistema)
          (afastamentos sistema)

  case resultado of
    Left err ->
      msgErro err >> return sistema

    Right (funcs, afs) -> do
      case afs of
        (a:_) ->
          putStrLn ("ID do afastamento: " ++ show (idAfastamento a))

      msgSucesso "Afastamento registrado com sucesso."

      return sistema
        { funcionarios = funcs
          , afastamentos = afs
        }

obterDocumentacaoObrigatoria :: TipoAfastamento -> IO (Maybe Documentacao)
obterDocumentacaoObrigatoria tipo
  | tipoExigeDocumentacao tipo = do
      doc <- lerDocumentacaoOpcional
      case doc of
        Nothing ->
          msgErro "Este tipo de afastamento exige documentação."
            >> obterDocumentacaoObrigatoria tipo
        Just _ ->
          return doc
  | otherwise =
      lerDocumentacaoOpcional


encerrarAfastamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
encerrarAfastamentoUI sistema = do
  cabecalho "ENCERRAR AFASTAMENTO"

  hoje <- utctDay <$> getCurrentTime
  idAf <- lerInt "ID do afastamento"

  case encerrarAfastamento hoje idAf (funcionarios sistema) (afastamentos sistema) of
    Left err -> msgErro err >> return sistema
    Right (funcs, afs) ->
      msgSucesso "Afastamento encerrado."
        >> return sistema { funcionarios = funcs, afastamentos = afs }

listarAfastadosUI :: SistemaBancoDadosRH -> IO ()
listarAfastadosUI sistema =
  let afastados = filter (\f -> statusFunc f == Afastado) (funcionarios sistema)
  in if null afastados
        then msgInfo "Nenhum funcionário afastado."
        else mapM_ exibirFuncionario afastados



-------------------------------------------------
-- DESLIGAMENTO - UI
-------------------------------------------------

desligarFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
desligarFuncionarioUI sistema = do
  cabecalho "DESLIGAR FUNCIONÁRIO"
  cpf <- lerCPF

  case buscarFuncionario cpf (funcionarios sistema) of
    Nothing -> msgErro "Funcionário não encontrado." >> return sistema
    Just f ->
      let f' = f { statusFunc = Desligado }
      in case modificarFuncionario f' (funcionarios sistema) of
           Left err -> msgErro err >> return sistema
           Right fs ->
             msgSucesso "Funcionário desligado."
               >> return sistema { funcionarios = fs }

listarDesligadosUI :: SistemaBancoDadosRH -> IO ()
listarDesligadosUI sistema =
  let desligados = filter (\f -> statusFunc f == Desligado) (funcionarios sistema)
  in if null desligados
        then msgInfo "Nenhum funcionário desligado."
        else mapM_ exibirFuncionario desligados

-------------------------------------------------
-- DEPARTAMENTOS - UI
-------------------------------------------------

cadastrarDepartamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
cadastrarDepartamentoUI sistema = do
  cabecalho "CADASTRAR DEPARTAMENTO"
  depto <- lerDepartamento

  case adicionarDepartamentoValidado
         depto
         (funcionarios sistema)
         (departamento sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Departamento cadastrado."
        >> return sistema { departamento = novaLista }

alterarDepartamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
alterarDepartamentoUI sistema = do
  cabecalho "ALTERAR DEPARTAMENTO"
  depto <- lerDepartamento

  case modificarDepartamento depto (departamento sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Departamento alterado."
        >> return sistema { departamento = novaLista }

buscarDepartamentoUI :: SistemaBancoDadosRH -> IO ()
buscarDepartamentoUI sistema = do
  cabecalho "BUSCAR DEPARTAMENTO"
  did <- lerInt "ID do departamento"

  case buscarDepartamento did (departamento sistema) of
    Nothing -> msgInfo "Departamento não encontrado."
    Just d  -> exibirDepartamento d

excluirDepartamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
excluirDepartamentoUI sistema = do
  cabecalho "EXCLUIR DEPARTAMENTO"
  did <- lerInt "ID do departamento"

  case excluirDepartamento did (departamento sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Departamento removido."
        >> return sistema { departamento = novaLista }

listarDepartamentosUI :: SistemaBancoDadosRH -> IO ()
listarDepartamentosUI sistema =
  if null (departamento sistema)
    then msgInfo "Nenhum departamento cadastrado."
    else mapM_ exibirDepartamento (departamento sistema)

-------------------------------------------------
-- CARGOS - UI
-------------------------------------------------

cadastrarCargoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
cadastrarCargoUI sistema = do
  cabecalho "CADASTRAR CARGO"
  cargo <- lerCargo

  case adicionarCargoValidado
         cargo
         (departamento sistema)
         (cargos sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Cargo cadastrado."
        >> return sistema { cargos = novaLista }


alterarCargoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
alterarCargoUI sistema = do
  cabecalho "ALTERAR CARGO"
  cargo <- lerCargo

  case modificarCargo cargo (cargos sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Cargo alterado."
        >> return sistema { cargos = novaLista }

buscarCargoUI :: SistemaBancoDadosRH -> IO ()
buscarCargoUI sistema = do
  cabecalho "BUSCAR CARGO"
  cid <- lerInt "ID do cargo"

  case buscarCargo cid (cargos sistema) of
    Nothing -> msgInfo "Cargo não encontrado."
    Just c  -> exibirCargo c

excluirCargoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
excluirCargoUI sistema = do
  cabecalho "EXCLUIR CARGO"
  cid <- lerInt "ID do cargo"

  case excluirCargo cid (cargos sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Cargo removido."
        >> return sistema { cargos = novaLista }

listarCargosUI :: SistemaBancoDadosRH -> IO ()
listarCargosUI sistema =
  if null (cargos sistema)
    then msgInfo "Nenhum cargo cadastrado."
    else mapM_ exibirCargo (cargos sistema)

-------------------------------------------------
-- FÉRIAS - UI
-------------------------------------------------

verificarDireitoService
  :: CPF -> Day -> GerenciadorFerias -> Either String [CicloFerias]
verificarDireitoService cpf ref ger = do
  reg <-
    maybe
      (Left "Funcionário não encontrado.")
      Right
      (Map.lookup cpf (registros ger))

  let ciclosAtualizados =
        map (atualizarCiclo ref)
          (takeWhile
            (\c -> inicioAquisitivo (periodoAquisitivo c) <= ref)
            (ciclos reg))

      ciclosComDireito =
        filter (temDireitoAFerias ref) ciclosAtualizados

  Right ciclosComDireito

registrarFeriasUI :: GerenciadorFerias -> IO GerenciadorFerias
registrarFeriasUI ger = do
  cabecalho "REGISTRO DE FÉRIAS"
  cpf <- lerCPF
  fracionada <- lerSN "Férias fracionadas?"

  qtd <- if fracionada then lerQuantidadePeriodos else return 1
  periodos <- mapM lerPeriodo [1 .. qtd]

  ref <- lerData "Data de referência para validação"

  confirmar "Confirmar registro das férias?" >>= \ok ->
    if not ok
      then msgInfo "Registro cancelado." >> putStrLn "" >> return ger
      else registrarSequencial cpf periodos ref ger

verificarDireitoUI :: GerenciadorFerias -> IO ()
verificarDireitoUI ger = do
  cabecalho "VERIFICAÇÃO DE DIREITO A FÉRIAS"
  cpf <- lerCPF
  ref <- lerData "Data de referência"

  case verificarDireitoService cpf ref ger of
    Left err -> msgErro err >> putStrLn ""
    Right [] -> msgInfo "Funcionário ainda não possui direito a férias." >> putStrLn ""
    Right cs -> do
      msgSucesso "Funcionário possui direito a férias."
      mapM_ exibirResumoCiclo cs

criarFerias :: Day -> Day -> Ferias
criarFerias ini fim =
  Ferias ini fim (fromIntegral (diffDays fim ini) + 1) Planejada

registrarSequencial
  :: CPF -> [(Day, Day)] -> Day -> GerenciadorFerias -> IO GerenciadorFerias
registrarSequencial _ [] _ ger =
  msgSucesso "Férias registradas com sucesso." >> putStrLn "" >> return ger

registrarSequencial cpf ((ini, fim):xs) ref ger = do
  let ferias = criarFerias ini fim
  case registrarFerias cpf ferias ref ger of
    Left err -> msgErro err >> putStrLn "" >> return ger
    Right g  -> registrarSequencial cpf xs ref g

atualizarFeriasComFalta ::
  CPF ->
  Day ->
  GerenciadorFerias ->
  IO GerenciadorFerias
atualizarFeriasComFalta cpf dia ger =
  case Map.lookup cpf (registros ger) of
    Nothing -> do
      msgErro "Funcionário não encontrado no sistema de férias."
      return ger
    Just reg ->
      case registrarFalta dia reg of
        Left err -> do
          msgErro ("Erro ao registrar falta no sistema de férias: " ++ err)
          return ger
        Right regAtualizado ->
          return ger { registros = Map.insert cpf regAtualizado (registros ger) }

-------------------------------------------------
-- PRESENÇA - UI
-------------------------------------------------

registrarPresencaUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
registrarPresencaUI sistema = do
  cabecalho "REGISTRO DE PRESENÇA"

  cpf <- lerCPF

  if not (any (\f -> idFunc f == cpf) (funcionarios sistema))
    then do
      msgErro "Funcionário não encontrado no sistema."
      return sistema
    else do
      dia <- lerData "Data da presença"
      deptoId <- lerInt "ID do departamento"
      compareceuFlag <- lerSN "Funcionário compareceu?"

      presenca <- if compareceuFlag
                   then do
                     modalidade <- lerModalidade
                     entrada <- lerHora "Horário de entrada"
                     saida <- lerHora "Horário de saída"
                     return Presenca
                       { idDepartamento = deptoId
                       , tipoPresenca = modalidade
                       , checkIn = entrada
                       , checkOut = saida
                       , compareceu = True
                       , justificativa = ""
                       }
                   else do
                     just <- lerLinha "Justificativa (vazio para injustificada)"
                     return Presenca
                       { idDepartamento = deptoId
                       , tipoPresenca = Presencial
                       , checkIn = midnight
                       , checkOut = midnight
                       , compareceu = False
                       , justificativa = just
                       }

      let resultadoPres = registrarPresenca cpf dia presenca (sistemaPresenca sistema)
      sistemaAtualizado <- case resultadoPres of
        Left err -> do
          msgErro err
          return sistema
        Right presAtualizado -> do
          msgSucesso "Presença registrada com sucesso."

          let gerFerias = sistemaFerias sistema
          sistemaComFalta <- if not compareceuFlag
                             then case Map.lookup cpf (registros gerFerias) of
                                    Nothing -> do
                                      msgErro "Funcionário não encontrado no sistema de férias."
                                      return sistema { sistemaPresenca = presAtualizado }
                                    Just reg -> case registrarFalta dia reg of
                                                  Left errF -> do
                                                    msgErro ("Erro ao registrar falta no sistema de férias: " ++ errF)
                                                    return sistema { sistemaPresenca = presAtualizado }
                                                  Right regAtualizado -> do
                                                    let gerAtualizado = gerFerias { registros = Map.insert cpf regAtualizado (registros gerFerias) }
                                                    return sistema { sistemaPresenca = presAtualizado
                                                                   , sistemaFerias = gerAtualizado }
                             else return sistema { sistemaPresenca = presAtualizado }

          return sistemaComFalta

      return sistemaAtualizado

consultarPresencaUI :: SistemaBancoDadosRH -> IO ()
consultarPresencaUI sistema = do
  cabecalho "CONSULTA DE PRESENÇA"
  cpf <- lerCPF
  dia <- lerData "Data a consultar"

  if not (any (\f -> idFunc f == cpf) (funcionarios sistema))
    then msgErro "Funcionário não encontrado no sistema."
    else case buscarPresenca cpf dia (sistemaPresenca sistema) of
           Nothing -> msgInfo "Nenhuma presença registrada para este dia."
           Just p  -> exibirPresenca (dia, p)

calcularHorasUI :: SistemaBancoDadosRH -> IO ()
calcularHorasUI sistema = do
  cabecalho "CÁLCULO DE HORAS TRABALHADAS"
  cpf <- lerCPF

  if not (any (\f -> idFunc f == cpf) (funcionarios sistema))
    then msgErro "Funcionário não encontrado no sistema."
    else do
      ini <- lerData "Data inicial"
      fim <- lerData "Data final"
      let total = horasTrabalhadas cpf ini fim (sistemaPresenca sistema)
      putStrLn ("\nTotal de horas trabalhadas: " ++ show total)


calcularFaltasUI :: SistemaBancoDadosRH -> IO ()
calcularFaltasUI sistema = do
  cabecalho "FALTAS INJUSTIFICADAS"
  cpf <- lerCPF

  if not (any (\f -> idFunc f == cpf) (funcionarios sistema))
    then msgErro "Funcionário não encontrado no sistema."
    else do
      ini <- lerData "Data inicial"
      fim <- lerData "Data final"
      let qtd = faltasInjustificadas cpf ini fim (sistemaPresenca sistema)
      putStrLn ("\nFaltas injustificadas no período: " ++ show qtd)

-------------------------------------------------
-- FORMATAÇÃO / EXIBIÇÃO
-------------------------------------------------

formatarPresenca :: (Day, Presenca) -> String
formatarPresenca (dia, p)
  | compareceu p =
      show dia
        ++ " | COMPARECEU"
        ++ " | " ++ show (tipoPresenca p)
        ++ " | " ++ show (checkIn p)
        ++ " - " ++ show (checkOut p)
        ++ " | Depto: " ++ show (idDepartamento p)
  | otherwise =
      show dia
        ++ " |  AUSENTE  "
        ++ statusJustificativa
  where
    statusJustificativa =
      if null (justificativa p)
        then " | INJUSTIFICADA"
        else " | JUSTIFICADA: " ++ justificativa p

exibirPresenca :: (Day, Presenca) -> IO ()
exibirPresenca = putStrLn . formatarPresenca

exibirRegistroPresencas :: CPF -> SistemaDePresenca -> IO ()
exibirRegistroPresencas cpf sistema =
  case Map.lookup cpf (presencasRegistradas sistema) of
    Nothing ->
      putStrLn "Nenhum registro de presença encontrado para este CPF."
    Just mapaDias -> do
      putStrLn "\nDATA       |   STATUS   | DETALHES"
      putStrLn "-----------------------------------------------"
      mapM_ exibirPresenca (Map.toAscList mapaDias)

exibirHistoricoUI :: SistemaBancoDadosRH -> IO ()
exibirHistoricoUI sistema = do
  cabecalho "HISTÓRICO DE PRESENÇAS"
  cpf <- lerCPF

  if not (any (\f -> idFunc f == cpf) (funcionarios sistema))
    then msgErro "Funcionário não encontrado no sistema."
    else exibirRegistroPresencas cpf (sistemaPresenca sistema)

exibirFeriasDetalhada :: Ferias -> IO ()
exibirFeriasDetalhada f =
  putStrLn $
    "  Período: " ++
    show (inicioFerias f) ++ " até " ++
    show (fimFerias f) ++
    " (" ++ show (diasUtilizados f) ++ " dias) - " ++
    show (statusFerias f)

exibirResumoCiclo :: CicloFerias -> IO ()
exibirResumoCiclo c = do
  let p = periodoConcessivo c
  putStrLn ""
  putStrLn ("Período concessivo : "
    ++ show (inicioConcessivo p)
    ++ " / "
    ++ show (fimConcessivo p))
  putStrLn ("Saldo disponível  : " ++ show (saldoDias c) ++ " dias")

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

exibirFeriasUI :: GerenciadorFerias -> IO GerenciadorFerias
exibirFeriasUI ger = do
  cabecalho "CONSULTA DE FÉRIAS"
  cpf <- lerCPF
  ref <- lerData "Data de referência"

  case Map.lookup cpf (registros ger) of
    Nothing -> msgErro "Funcionário não encontrado." >> putStrLn ""
    Just reg -> do
      let ciclosAtualizados =
            map (atualizarCiclo ref)
              (takeWhile
                (\c -> inicioAquisitivo (periodoAquisitivo c) <= ref)
                (ciclos reg))

          ciclosExibidos =
            filter
              (\c ->
                statusCiclo c /= Esperado ||
                any (\f ->
                      statusFerias f == EmAndamento ||
                      statusFerias f == Concluida)
                    (feriasDoCiclo c))
              ciclosAtualizados

      if null ciclosExibidos
        then msgInfo "Nenhum ciclo relevante encontrado." >> putStrLn ""
        else mapM_ exibirFeriasPersonalizado ciclosExibidos

  return ger

exibirFuncionario :: Funcionario -> IO ()
exibirFuncionario f = do
  putStrLn "--------------------------------"
  putStrLn ("CPF: " ++ idFunc f)
  putStrLn ("Nome: " ++ nomeFunc f)
  putStrLn ("Status: " ++ show (statusFunc f))
  putStrLn ("Departamento ID: " ++ show (deptoFunc f))
  putStrLn ("Cargo ID: " ++ show (cargoFunc f))

exibirDepartamento :: Departamento -> IO ()
exibirDepartamento d = do
  putStrLn "--------------------------------"
  putStrLn ("ID: " ++ show (idDepto d))
  putStrLn ("Nome: " ++ nomeDepto d)
  putStrLn ("Descrição: " ++ descricaoDepto d)
  putStrLn ("Gerente: " ++ exibirGerente (idGerenteDepto d))
  putStrLn ("Qtd Funcionários: " ++ show (qtdFuncionarioDepto d))
  where
    exibirGerente Nothing  = "Não definido"
    exibirGerente (Just g) = show g


exibirCargo :: Cargo -> IO ()
exibirCargo c = do
  putStrLn "--------------------------------"
  putStrLn ("ID: " ++ show (idCargo c))
  putStrLn ("Nome: " ++ nomeCargo c)
  putStrLn ("Função: " ++ funcaoCargo c)
  putStrLn ("Carga Horária: " ++ show (cargaHoraria c))
  putStrLn ("Salário: " ++ show (salario c))
  putStrLn ("Departamento ID: " ++ show (deptoAssociado c))

-------------------------------------------------
-- LEITURAS AUXILIARES
-------------------------------------------------

lerFuncionario :: IO Funcionario
lerFuncionario = do
  cpf <- lerCPF
  nome <- lerLinha "Nome"
  nasc <- lerData "Data de nascimento"
  genero <- lerLinha "Gênero"
  email <- lerLinha "Email"
  telefone <- lerLinha "Telefone"
  endereco <- lerLinha "Endereço"
  cargo <- lerInt "ID do cargo"
  depto <- lerInt "ID do departamento"
  carga <- lerInt "Carga horária"
  admissao <- lerData "Data de admissão"

  return Funcionario
    { idFunc = cpf
    , nomeFunc = nome
    , dataNascimentoFunc = nasc
    , generoFunc = genero
    , emailFunc = email
    , telefoneFunc = telefone
    , enderecoFunc = endereco
    , statusFunc = Ativo
    , linkLinkedinFunc = ""
    , cargoFunc = cargo
    , deptoFunc = depto
    , historicoAlteracoesFunc = []
    , cargaHorariaFunc = carga
    , dataAdmissaoFunc = admissao
    }

lerDepartamento :: IO Departamento
lerDepartamento = do
  idD <- lerInt "ID do departamento"
  nome <- lerLinha "Nome"
  desc <- lerLinha "Descrição"
  temGerente <- lerSN "Deseja informar gerente?"
  gerente <-
    if temGerente
       then Just <$> lerInt "ID do gerente"
       else return Nothing
  qtd <- lerInt "Qtd de funcionários"

  return Departamento
    { idDepto = idD
    , nomeDepto = nome
    , descricaoDepto = desc
    , idGerenteDepto = gerente
    , qtdFuncionarioDepto = qtd
    , registroPresencaDepto = []
    }


lerCargo :: IO Cargo
lerCargo = do
  idC <- lerInt "ID do cargo"
  nome <- lerLinha "Nome"
  funcao <- lerLinha "Função"
  carga <- lerInt "Carga horária"
  sal <- lerDouble "Salário"
  depto <- lerInt "ID do departamento"

  return Cargo
    { idCargo = idC
    , nomeCargo = nome
    , funcaoCargo = funcao
    , cargaHoraria = carga
    , salario = sal
    , deptoAssociado = depto
    }


lerTipoAfastamento :: IO TipoAfastamento
lerTipoAfastamento = do
  putStrLn "1 - Médico"
  putStrLn "2 - Acidente de trabalho"
  putStrLn "3 - Licença maternidade"
  putStrLn "4 - Licença paternidade"
  putStrLn "5 - Ausência justificada"

  op <- lerLinha "Tipo"

  case op of
    "1" -> return AfastamentoMedico
    "2" -> return AcidenteDeTrabalho
    "3" -> return LicencaMaternidade
    "4" -> return LicencaPaternidade
    "5" -> return AusenciaJustificada
    _   -> msgErro "Opção inválida." >> lerTipoAfastamento

lerDocumentacaoOpcional :: IO (Maybe Documentacao)
lerDocumentacaoOpcional = do
  temDoc <- lerSN "Deseja informar documentação?"
  if not temDoc
     then return Nothing
     else do
       desc <- lerLinha "Descrição do documento"
       dataEnvio <- lerData "Data de envio do documento"
       return $ Just Documentacao
         { descricaoDocumento = desc
         , dataEnvioDocumento = dataEnvio
         }

lerTipoLicenca :: IO TiposDeLicenca
lerTipoLicenca = do
  putStrLn "1 - Casamento"
  putStrLn "2 - Maternidade"
  putStrLn "3 - Paternidade"
  putStrLn "4 - Atestado"
  putStrLn "5 - Luto"
  putStrLn "6 - Doação de sangue"

  op <- lerLinha "Tipo"

  case op of
    "1" -> return Casamento
    "2" -> return Maternidade
    "3" -> return Paternidade
    "4" -> return Atestado
    "5" -> return Luto
    "6" -> return DoacaoSangue
    _   -> msgErro "Opção inválida." >> lerTipoLicenca

lerModalidade :: IO Modalidade
lerModalidade = do
  putStrLn "Modalidade:"
  putStrLn "1 - Presencial"
  putStrLn "2 - Remoto"
  op <- lerLinha "Escolha"

  case op of
    "1" -> return Presencial
    "2" -> return Remoto
    _   -> msgErro "Opção inválida." >> lerModalidade

lerPeriodo :: Int -> IO (Day, Day)
lerPeriodo i = do
  putStrLn ""
  putStrLn ("Período " ++ show i)
  ini <- lerData "Data de início"
  fim <- lerData "Data de fim"
  return (ini, fim)
