module Controller.Afastamento where

import Model.TiposDados
import Data.Time(Day, addDays)
import Data.List (find)
import Controller.GerenciaFuncionarios

-- Define a tolerância para envio de documentação após o término do afastamento (em dias).
toleranciaDocumentacao :: Integer
toleranciaDocumentacao = 5

-- Gera um novo ID para o afastamento.
novoIdAfastamento :: [Afastamento] -> Id
novoIdAfastamento [] = 1
novoIdAfastamento afastamentos = maximum (map idAfastamento afastamentos) + 1

-- Valida se o período de afastamento é válido (data de início anterior ou igual à data de fim).
validarPeriodoAfastamento :: Afastamento -> Bool
validarPeriodoAfastamento a = dataInicioAfastamento a <= dataFimAfastamento a

tipoExigeDocumentacao :: TipoAfastamento -> Bool
tipoExigeDocumentacao AfastamentoMedico   = True
tipoExigeDocumentacao AcidenteDeTrabalho  = True
tipoExigeDocumentacao AusenciaJustificada = True
tipoExigeDocumentacao _                   = False

-- Verifica se dois afastamentos conflitam (mesmo funcionário e períodos que se sobrepõem).
conflita :: Afastamento -> Afastamento -> Bool  
conflita a1 a2 =
    cpfFuncionario a1 == cpfFuncionario a2 &&
    not (dataFimAfastamento a1 < dataInicioAfastamento a2 || dataFimAfastamento a2 < dataInicioAfastamento a1)

-- Registra um novo afastamento para um funcionário.
registrarAfastamento ::
    Day -> CPF -> TipoAfastamento -> Day -> Day -> String -> Maybe Documentacao ->
    [Funcionario] -> [Afastamento] ->
    Either String ([Funcionario], [Afastamento])
registrarAfastamento hoje cpf tipo inicio fim descricao doc funcionarios afastamentos =
    case buscarFuncionario cpf funcionarios of
        Nothing -> Left "Erro: Funcionário não encontrado."
        Just func ->
            if statusFunc func /= Ativo
                then Left "Erro: Afastamento só pode ser registrado para funcionário ativo."
                else
                    let
                        idNovo = novoIdAfastamento afastamentos
                        novoAfastamento =
                            Afastamento
                                { idAfastamento = idNovo
                                , cpfFuncionario = cpf
                                , tipoAfastamento = tipo
                                , dataInicioAfastamento = inicio
                                , dataFimAfastamento = fim
                                , descricaoAfastamento = descricao
                                , documentacao = doc
                                }
                    in
                        if not (validarPeriodoAfastamento novoAfastamento)
                            then Left "Erro: Período de afastamento inválido."
                        else if not (validarDocumentacao novoAfastamento)
                            then Left "Erro: Data da documentação fora do período permitido."
                        else if any (conflita novoAfastamento) afastamentos
                            then Left "Erro: Conflito com outro afastamento existente."
                        else
                            let
                                funcionariosAtualizados =
                                    case modificarFuncionario 
                                        (atualizarStatusPorData hoje func (novoAfastamento : afastamentos)) funcionarios of
                                        Left _  -> funcionarios
                                        Right fs -> fs
                            in Right (funcionariosAtualizados, novoAfastamento : afastamentos)

-- Verifica se um funcionário está afastado na data atual.
afastamentoAtivoHoje :: Day -> CPF -> [Afastamento] -> Bool
afastamentoAtivoHoje = afastamentoAtivoEm

-- Encerra um afastamento existente.
encerrarAfastamento :: Day -> Id -> [Funcionario] -> [Afastamento] -> Either String ([Funcionario], [Afastamento])
encerrarAfastamento hoje idAf funcionarios afastamentos =
    case buscarAfastamento idAf afastamentos of
        Nothing -> Left "Erro: Afastamento não encontrado."
        Just afastamento ->
            case buscarFuncionario (cpfFuncionario afastamento) funcionarios of
                Nothing -> Left "Erro: Funcionário não encontrado."
                Just func ->
                    let
                        afastamentosAtualizados =
                            filter (\a -> idAfastamento a /= idAf) afastamentos

                        continuaAfastado =
                            afastamentoAtivoHoje hoje
                                (cpfFuncionario afastamento)
                                afastamentosAtualizados

                        novoStatus = 
                            if continuaAfastado
                                then Afastado
                                else Ativo

                        funcionariosAtualizados =
                            case modificarFuncionario
                                (func { statusFunc = novoStatus })
                                funcionarios of
                                Left _  -> funcionarios
                                Right fs -> fs
                    in
                        Right (funcionariosAtualizados, afastamentosAtualizados)                        

-- Busca um afastamento pelo seu ID.
buscarAfastamento :: Id -> [Afastamento] -> Maybe Afastamento
buscarAfastamento idAf =
  find (\a -> idAfastamento a == idAf)

-- Atualiza o status de um funcionário com base nos afastamentos ativos na data fornecida.
atualizarStatusPorData :: Day -> Funcionario -> [Afastamento] -> Funcionario
atualizarStatusPorData hoje func afastamentos =
    let
        afastadoHoje =
            any (\a -> 
                cpfFuncionario a == idFunc func && 
                dataInicioAfastamento a <= hoje &&
                hoje <= dataFimAfastamento a
            ) afastamentos
    in
        if afastadoHoje
            then func { statusFunc = Afastado }
            else func { statusFunc = Ativo }

-- Retorna todos os afastamentos de um determinado funcionário
afastamentosDoFuncionario :: CPF -> [Afastamento] -> [Afastamento]
afastamentosDoFuncionario cpf = filter (\a -> cpfFuncionario a == cpf)

-- Verifica se um funcionário está afastado em uma data específica.
afastamentoAtivoEm :: Day -> CPF -> [Afastamento] -> Bool
afastamentoAtivoEm dataRef cpf =
  any (\a ->
        cpfFuncionario a == cpf &&
        dataInicioAfastamento a <= dataRef &&
        dataRef <= dataFimAfastamento a
      )

-- Atualiza o status de todos os funcionários com base nos afastamentos ativos na data fornecida.
atualizarStatusFuncionariosPorData :: Day -> [Funcionario] -> [Afastamento] -> [Funcionario]
atualizarStatusFuncionariosPorData dataRef funcionarios afastamentos =
    map atualizar funcionarios
  where
    atualizar func
      | statusFunc func == Desligado = func
      | afastamentoAtivoEm dataRef (idFunc func) afastamentos =
          func { statusFunc = Afastado }
      | otherwise =
          func { statusFunc = Ativo }

-- Lista todos os afastamentos registrados.
listarAfastamentos :: [Afastamento] -> [Afastamento]
listarAfastamentos = id          

-- Lista todos os afastamentos ativos em uma data específica.
listarAfastamentosAtivos :: Day -> [Afastamento] -> [Afastamento]
listarAfastamentosAtivos dataRef =
    filter (\a -> dataInicioAfastamento a <= dataRef && dataRef <= dataFimAfastamento a)

-- Valida a documentação do afastamento (modelo adotado: data de envio deve estar entre o início do afastamento e até 5 dias após o fim).
validarDocumentacao :: Afastamento -> Bool
validarDocumentacao afastamento =
    case documentacao afastamento of
        Nothing -> True
        Just doc ->
            let
                inicio = dataInicioAfastamento afastamento
                fim = dataFimAfastamento afastamento
                dataDoc = dataEnvioDocumento doc
            in
                dataDoc >= inicio &&
                dataDoc <= addDays toleranciaDocumentacao fim