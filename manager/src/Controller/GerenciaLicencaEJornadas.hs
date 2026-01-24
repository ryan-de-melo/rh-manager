module Controller.GerenciaLicencaEJornadas where

import Model.TiposDados
import Controller.JornadaLicenca

-- Licenças

adicionaLicenca :: Funcionario -> Licenca -> SistemaJornadaLicenca -> SistemaJornadaLicenca
adicionaLicenca func novaLicenca sistema
    | not (verificarSeLicencaValida novaLicenca) =
        Left "Erro Licença inválida"
    | otherwise =
        Right sistema {
            licencas = novaLicenca : licencas sistema
        }

removerLicenca :: Licenca -> SistemaJornadaLicenca -> SistemaJornadaLicenca
removerLicenca licencaParaRemover sistema
    | licencaParaRemover `notElem` licencas sistema =
        Left "Erro Licença não encontrada"
    | otherwise =
        Right sistema {
            licencas = filter (/= licencaParaRemover) (licencas sistema)
        }

listarLicencas :: SistemaJornadaLicenca -> [Licenca]
listarLicencas sistema = licencas sistema

verificaFuncionarioEmLicenca :: CPF -> SistemaJornadaLicenca -> Bool
verificaFuncionarioEmLicenca cpf sistema =
    any (\licenca -> funcionarioLicenca licenca == cpf) (licencas sistema)

-- Jornadas/Escalas

adicionaJornada :: EscalaSemanal -> SistemaJornadaLicenca -> SistemaJornadaLicenca
adicionaJornada novaJornada sistema
    | not (verificaEscalaValida novaJornada) =
        Left "Erro: Escala inválida"
    | not (verificaLegalidadeCargaHorariaSemanal novaJornada) =
        Left "Erro Carga horária semanal ilegal"
    | otherwise =
        Right sistema {
            jornadasSemanais = novaJornada : jornadasSemanais sistema
        }

removerJornada :: EscalaSemanal -> SistemaJornadaLicenca -> SistemaJornadaLicenca
removerJornada jornadaParaRemover sistema
    | jornadaParaRemover `notElem` jornadasSemanais sistema =
        Left "Erro: Jornada não encontrada."
    | otherwise =
        Right sistema {
            jornadasSemanais =
                filter (/= jornadaParaRemover) (jornadasSemanais sistema)
        }

listarJornadas :: SistemaJornadaLicenca -> [EscalaSemanal]
listarJornadas sistema = jornadas sistema

buscaFolgaDoFuncionario :: CPF -> SistemaJornadaLicenca -> Maybe Day
buscaFolgaDoFuncionario cpf sistema =
    case find (\escala -> idFuncionarioSemanal escala == cpf)
              (jornadasSemanais sistema) of
        Just escala -> buscaDiaDeFolga (cicloFolga escala)
        Nothing     -> Nothing


-- Metodo auxiliar pra facilitar criar escala

criaEscala :: CPF -> [Day] -> Int -> Int -> Day -> Day -> Maybe EscalaSemanal
criaEscala cpf dias horaEntrada horaSaida ultimaFolga proximaFolga =
    let
        jornadaDiaria = JornadaDiaria {
            inicio = horaEntrada,
            fim = horaSaida
        }

        jornadasCriadas = replicate (length dias) jornadaDiaria

        ciclo = Folga {
            ultimaFolga = ultimaFolga,
            dataFolga = proximaFolga
        }

        escala = EscalaSemanal {
            idFuncionarioSemanal = cpf,
            diasTrabalho = dias,
            jornadas = jornadasCriadas,
            cicloFolga = ciclo
        }
    in
        if  verificaJornadaValida jornadaDiaria
            && verificaLegalidadeCargaHorariaDiaria jornadaDiaria
            && verificaEscalaValida escala
            && verificaLegalidadeCargaHorariaSemanal escala
            && verificaLegalidadeDeCicloFolga ciclo
        then Just escala
        else Nothing