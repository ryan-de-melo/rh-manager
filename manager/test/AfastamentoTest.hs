module AfastamentoTest (runAfastamentoTests) where

import Model.TiposDados hiding (inicio, fim)
import Controller.Afastamento
import Controller.GerenciaFuncionarios (buscarFuncionario)
import Data.Time (fromGregorian, Day, addDays)
import System.Exit (exitFailure)

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = do
      putStrLn $ "Testes FALHARAM: " ++ msg
      exitFailure

-- Datas de teste
hoje :: Day
hoje = fromGregorian 2026 01 10

inicio :: Day
inicio = fromGregorian 2026 01 10

fim :: Day
fim = fromGregorian 2026 01 18

fimInvalido :: Day 
fimInvalido = fromGregorian 2024 12 31

dataNasc :: Day
dataNasc = fromGregorian 1980 06 26

dataAdmissaoTeste :: Day
dataAdmissaoTeste = fromGregorian 2025 1 13

-- Funcionários
funcAtivo :: Funcionario
funcAtivo = Funcionario "11111111111" "Carlos" dataNasc "M" "@x.com" "9999" "PB" Ativo "link" 1 1 [] 40 dataAdmissaoTeste

funcAfastado :: Funcionario
funcAfastado = Funcionario "22222222222" "Ana" dataNasc "F" "@y.com" "8888" "RJ" Afastado "link" 1 1 [] 30 dataAdmissaoTeste

funcDesligado :: Funcionario
funcDesligado = Funcionario "33333333333" "Beatriz" dataNasc "F" "@z.com" "7777" "SP" Desligado "link" 1 1 [] 20 dataAdmissaoTeste

-- TESTES ------------------------------------------------------------
afastamentoValidoTeste :: IO ()
afastamentoValidoTeste = do
  let funcionarios = [funcAtivo]
  let afastamentos = []

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing funcionarios afastamentos of
    Left _ -> assert False "Falhou ao registrar afastamento válido."
    Right (funcs, afs) -> do 
      let Just f = buscarFuncionario "11111111111" funcs 
      assert (statusFunc f == Afastado) "Funcionário ficou com status Afastado."
      assert (length afs == 1) "Afastamento foi adicionado à lista."

funcionarioAfastadoTeste :: IO ()
funcionarioAfastadoTeste = do
  let funcionarios = [funcAfastado]
  let afastamentos = []

  case registrarAfastamento hoje "22222222222" LicencaMaternidade inicio fim "Mãe de gemeos" Nothing funcionarios afastamentos of
    Left _ -> assert True "Erro: afastamento só pode ser registrado para funcionário ativo."
    Right (_, afs) ->
      assert (length afs == 0) "Erro: função retornou Right, mas não deveria ter registrado afastamento."

funcionarioDesligadoTeste :: IO ()
funcionarioDesligadoTeste = do 
  let funcionarios = [funcDesligado]
  let afastamentos = []

  case registrarAfastamento hoje "33333333333" AusenciaJustificada inicio fim "Falecimento de um parente" Nothing funcionarios afastamentos of
    Left _ -> assert True "Erro: afastamento só pode ser registrado para funcionário ativo."
    Right (_, afs) ->
      assert (length afs == 0) "Erro: função retornou Right, mas não deveria ter registrado afastamento."

funcionarioInexistenteTeste :: IO ()
funcionarioInexistenteTeste = do
  let funcionarios = []
  let afastamentos = []

  case registrarAfastamento hoje "01234567891" LicencaPaternidade inicio fim "Filho nasceu" Nothing funcionarios afastamentos of
    Left _ -> assert True "Erro: afastamento de funcionário inexistente."
    Right (_, afs) ->
      assert (length afs == 0) "Erro: permitiu afastamento para funcionário inexistente."

periodoInvalidoTeste :: IO ()
periodoInvalidoTeste = do
  let funcionarios = [funcAtivo]
  let afastamentos = []

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio fimInvalido "Cirurgia" Nothing funcionarios afastamentos of
    Left _ -> assert True "Falhou ao registrar afastamento válido."
    Right (_, afs) ->
      assert (length afs == 0) "Erro: permitiu afastamento para data invalida (inicio > fim)."

registrarComInicioIgualFimTeste :: IO ()
registrarComInicioIgualFimTeste = do
  let funcionarios = [funcAtivo]
  let afastamentos = []

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio inicio "Cirurgia" Nothing funcionarios afastamentos of
    Left _ -> assert True "Falhou ao registrar afastamento válido."
    Right (funcs, afs) -> do
      let Just f = buscarFuncionario ("11111111111") funcs
      assert (statusFunc f == Afastado) "Funcionário ficou com status Afastado."
      assert (length afs == 1) "Afastamento foi adicionado à lista."

conflitoAfastamentoTeste :: IO ()
conflitoAfastamentoTeste = do
  let 
    inicio2 :: Day
    inicio2 = fromGregorian 2026 01 12

    fim2 :: Day
    fim2 = fromGregorian 2026 02 01

    afastamentoExistente = Afastamento 7 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    funcionarios = [funcAtivo]
    afastamentos = [afastamentoExistente]

  case registrarAfastamento hoje "11111111111" AusenciaJustificada inicio2 fim2 "Recesso" Nothing funcionarios afastamentos of
    Left _ -> assert True "Erro: conflito entre datas."
    Right _ -> assert False "Erro: permitiu afastamentos conflitantes."

encerrarAfastamentoValidoTeste :: IO ()
encerrarAfastamentoValidoTeste = do
  let
    afastamento = Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    funcionarios = [funcAtivo {statusFunc = Afastado}]
    afastamentos = [afastamento]

  case encerrarAfastamento hoje 1 funcionarios afastamentos of
    Left _ -> assert False "Falhou ao encerrar afastamento válido."

    Right (funcs, afs) -> do
      assert (null afs) "Afastamento foi removido da lista."

      case buscarFuncionario "11111111111" funcs of
        Nothing -> assert False "Funcionário não encontrado após encerramento."

        Just f -> assert (statusFunc f == Ativo) "Funcionário voltou ao status Ativo."

encerrarAfastamentoInexistenteTeste :: IO ()
encerrarAfastamentoInexistenteTeste = do
  let funcionarios = [funcAtivo]
      afastamentos = []

  case encerrarAfastamento hoje 99 funcionarios afastamentos of
    Left _ -> assert True "Bloqueou encerramento de afastamento inexistente."

    Right _ -> assert False "Erro: permitiu encerrar afastamento inexistente."

encerrarAfastamentoComOutroAtivoTeste :: IO ()
encerrarAfastamentoComOutroAtivoTeste = do
  let
    fim2 :: Day
    fim2 = fromGregorian 2026 02 01

    afast1 = Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    afast2 = Afastamento 2 "11111111111" LicencaPaternidade inicio fim2 "Filho nasceu" Nothing

    funcionarios = [funcAtivo { statusFunc = Afastado }]
    afastamentos = [afast1, afast2]

  case encerrarAfastamento hoje 1 funcionarios afastamentos of
    Left _ ->
      assert False "Falhou ao encerrar afastamento."

    Right (funcs, afs) -> do
      assert (length afs == 1) "Removeu apenas um afastamento."

      case buscarFuncionario "11111111111" funcs of
        Just f -> assert (statusFunc f == Afastado) "Funcionário permaneceu Afastado devido a outro afastamento ativo."

        Nothing -> assert False "Funcionário não encontrado."

afastamentosDoFuncionarioTeste :: IO ()
afastamentosDoFuncionarioTeste = do
  let
    afast1 = Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    afast2 = Afastamento 2 "11111111111" LicencaPaternidade inicio fim "Filho nasceu" Nothing
    afast3 = Afastamento 3 "22222222222" AusenciaJustificada inicio fim "Férias" Nothing

    resultado = afastamentosDoFuncionario "11111111111" [afast1, afast2, afast3]

  assert (length resultado == 2) "Listou apenas os afastamentos do funcionário."

afastamentoAtivoEmTeste :: IO ()
afastamentoAtivoEmTeste = do
  let
    afastamento = Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    afastamentos = [afastamento]

  assert (afastamentoAtivoEm hoje "11111111111" afastamentos)
    "Detectou afastamento ativo na data."

  assert (not $ afastamentoAtivoEm (fromGregorian 2026 02 10) "11111111111" afastamentos)
    "Detectou corretamente ausência de afastamento na data."

atualizarStatusFuncionariosPorDataTeste :: IO ()
atualizarStatusFuncionariosPorDataTeste = do
  let
    afastamento = Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    funcionarios = [funcAtivo, funcDesligado]
    afastamentos = [afastamento]

    [f1, f2] = atualizarStatusFuncionariosPorData hoje funcionarios afastamentos

  assert (statusFunc f1 == Afastado) "Funcionário ativo passou para Afastado."

  assert (statusFunc f2 == Desligado) "Funcionário desligado manteve status."

listarAfastamentosTeste :: IO ()
listarAfastamentosTeste = do
  let
    afastamentos =
      [ Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
      , Afastamento 2 "22222222222" AusenciaJustificada inicio fim "Férias" Nothing
      ]

  assert (length (listarAfastamentos afastamentos) == 2) "Listou todos os afastamentos."

listarAfastamentosAtivosTeste :: IO ()
listarAfastamentosAtivosTeste = do
  let
    afast1 = Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing
    afast2 = Afastamento 2 "22222222222" AusenciaJustificada (fromGregorian 2026 02 01) (fromGregorian 2026 02 10) "Férias" Nothing

    ativos = listarAfastamentosAtivos hoje [afast1, afast2]

  assert (length ativos == 1) "Listou apenas afastamentos ativos na data."

funcionarioSemAfastamentoTeste :: IO ()
funcionarioSemAfastamentoTeste = do
  let
    funcionarios = [funcAtivo]
    afastamentos = []

    [f] = atualizarStatusFuncionariosPorData hoje funcionarios afastamentos

  assert (statusFunc f == Ativo) "Funcionário sem afastamentos permanece Ativo."

registrarAfastamentoComDocumentacaoTeste :: IO ()
registrarAfastamentoComDocumentacaoTeste = do
  let
    funcionarios = [funcAtivo]
    afastamentos = []

    doc = Just (Documentacao { descricaoDocumento = "Atestado médico", dataEnvioDocumento = hoje})

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio fim "Cirurgia" doc funcionarios afastamentos of
    Left _ -> assert False "Falhou ao registrar afastamento com documentação."

    Right (_, afs) -> do
      let [a] = afs
      assert (documentacao a == doc) "Documentação foi registrada corretamente no afastamento."

documentacaoNaoAfetaConflitoTeste :: IO ()
documentacaoNaoAfetaConflitoTeste = do
  let
    afastamentoExistente =
      Afastamento 1 "11111111111" AfastamentoMedico inicio fim "Cirurgia" Nothing

    funcionarios = [funcAtivo]
    afastamentos = [afastamentoExistente]

    doc = Just (Documentacao { descricaoDocumento = "Outro atestado" , dataEnvioDocumento = hoje})

  case registrarAfastamento hoje "11111111111" AusenciaJustificada inicio fim "Outro motivo" doc funcionarios afastamentos of
    Left _ -> assert True "Conflito detectado corretamente mesmo com documentação."

    Right _ -> assert False "Erro: documentação não deveria permitir burlar conflito."

documentacaoAntesDoInicioTeste :: IO ()
documentacaoAntesDoInicioTeste = do
  let
    funcionarios = [funcAtivo]
    afastamentos = []

    doc = Just (Documentacao { descricaoDocumento = "Atestado", dataEnvioDocumento = fromGregorian 2026 01 05})

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio fim "Cirurgia" doc funcionarios afastamentos of
    Left _ -> assert True "Bloqueou documentação com data anterior ao início do afastamento."
    Right _ -> assert False "Erro: permitiu documentação anterior ao início."

documentacaoDentroDaToleranciaTeste :: IO ()
documentacaoDentroDaToleranciaTeste = do
  let
    funcionarios = [funcAtivo]
    afastamentos = []

    doc = Just (Documentacao { descricaoDocumento = "Atestado", dataEnvioDocumento = addDays 3 fim})

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio fim "Cirurgia" doc funcionarios afastamentos of
    Left _ -> assert False "Falhou ao aceitar documentação dentro da tolerância."
    Right _ -> assert True "Aceitou documentação dentro da tolerância."

documentacaoForaDaToleranciaTeste :: IO ()
documentacaoForaDaToleranciaTeste = do
  let
    funcionarios = [funcAtivo]
    afastamentos = []

    doc = Just (Documentacao{ descricaoDocumento = "Atestado", dataEnvioDocumento = addDays 10 fim})

  case registrarAfastamento hoje "11111111111" AfastamentoMedico inicio fim "Cirurgia" doc funcionarios afastamentos of
    Left _ -> assert True "Bloqueou documentação enviada após o limite de tolerância."
    Right _ -> assert False "Erro: permitiu documentação fora da tolerância."
-- RUNNER ------------------------------------------------------------

runAfastamentoTests :: IO ()
runAfastamentoTests = do
  putStrLn "\n--- Testes de Afastamento ---"

  afastamentoValidoTeste
  funcionarioAfastadoTeste
  funcionarioDesligadoTeste
  funcionarioInexistenteTeste
  periodoInvalidoTeste
  registrarComInicioIgualFimTeste
  conflitoAfastamentoTeste
  encerrarAfastamentoValidoTeste
  encerrarAfastamentoInexistenteTeste
  encerrarAfastamentoComOutroAtivoTeste
  afastamentosDoFuncionarioTeste
  afastamentoAtivoEmTeste
  atualizarStatusFuncionariosPorDataTeste
  listarAfastamentosTeste
  listarAfastamentosAtivosTeste
  funcionarioSemAfastamentoTeste
  registrarAfastamentoComDocumentacaoTeste
  documentacaoNaoAfetaConflitoTeste
  documentacaoAntesDoInicioTeste
  documentacaoDentroDaToleranciaTeste
  documentacaoForaDaToleranciaTeste