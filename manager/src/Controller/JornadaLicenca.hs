module Controller.JornadaLicenca where

import Model.TiposDados
import Data.Time (Day, diffDays)

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

