module Controller.JornadaLicenca where

import Model.TiposDados
import Data.Time (Day, diffDays)
import Data.Bool (Bool(False, True))
import Model.TiposDados (JornadaDiaria)

-- Licença

diasDeLicenca :: Licenca -> Int
diasDeLicenca licenca 
    | tipoLicensa licenca == Casamento     = 3
    | tipoLicensa licenca == Maternidade   = 120
    | tipoLicensa licenca == Paternidade   = 5
    | tipoLicensa licenca == Atestado      = 15
    | tipoLicensa licenca == Luto          = 2
    | tipoLicensa licenca == DoacaoSangue  = 1
    | otherwise                            = 0

verificarSeLicencaValida :: Licenca -> Bool
verificarSeLicencaValida licenca
    | dataInicio licenca > dataFim licenca = False
    | diffDays (dataFim licenca) (dataInicio licenca) <= fromIntegral (diasDeLicenca licenca) = True
    | otherwise = False


-- Jornada de Trabalho

verificaCargaHorariaPorCargo :: Funcionario -> Cargo -> Bool
verificaCargaHorariaPorCargo func cargo =
    cargaHorariaFunc func == cargaHoraria cargo

buscaDiaDeFolga :: CicloFolga -> Maybe Day
buscaDiaDeFolga cicloFolga
    | verificaLegalidadeDeCicloFolga cicloFolga = Just (dataFolga cicloFolga)
    | otherwise                    = Nothing

verificaLegalidadeDeCicloFolga :: CicloFolga -> Bool
verificaLegalidadeDeCicloFolga cicloFolga =
    let diff = diffDays (dataFolga cicloFolga) (ultimaFolga cicloFolga)
        in diff >= 0 && diff < 7

atualizarCicloFolga :: Day -> CicloFolga -> Maybe CicloFolga
atualizarCicloFolga futuraFolga cicloFolga
    | verificaLegalidadeDeCicloFolga cicloFolga =
        Just cicloFolga {   ultimaFolga = dataFolga cicloFolga,
                            dataFolga = futuraFolga
                        }
    | otherwise = Nothing


calculaHorasTrabalhadasPorDia :: JornadaDiaria -> Int
calculaHorasTrabalhadas jornadaDiaria =
    fim jornadaDiaria - inicio jornadaDiaria

calculaHorasTrabalhadasPorSemana :: EscalaSemanal -> Int
calculaHorasTrabalhadasPorSemana escalaSemanal =
    sum (map calculaHorasTrabalhadasPorDia (jornadas escalaSemanal))


verificaEscalaValida :: EscalaSemanal -> Bool
verificaEscalaValida escala =
    length (diasTrabalho escala) == length (jornadas escala)
    && all verificaJornadaValida (jornadas escala)

verificaJornadaValida :: JornadaDiaria -> Bool
verificaJornadaValida jornada =
    inicio jornada >= 0 &&
    fim jornada <= 23 &&
    fim jornada > inicio jornada


verificaLegalidadeCargaHorariaDiaria :: JornadaDiaria -> Bool
verificaLegalidadeCargaHorariaDiaria jornadaDiaria =
    let carga = calculaHorasTrabalhadas jornadaDiaria
    in carga > 0 && carga <= 8

verificaLegalidadeCargaHorariaSemanal :: EscalaSemanal -> Bool
verificaLegalidadeCargaHorariaSemanal escalaSemanal
    | not (verificaEscalaValida escalaSemanal) = False
    | calculaHorasTrabalhadasPorSemana escalaSemanal <= 44 = True
    | otherwise = False