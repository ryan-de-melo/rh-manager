module Model.TiposDados where

import Data.Time(Day, TimeOfDay)
import qualified Data.Map as Map

type CPF = String
type Id = Int
type Registro = [String]

data Cargo = Cargo{
    idCargo :: Id,
    nomeCargo :: String,
    funcaoCargo :: String,
    cargaHoraria :: Int,
    salario :: Double,
    idSupervisor :: Id,
    deptoAssociado :: Id
} deriving (Show,Read, Eq)

data Departamento = Departamento{
    idDepto :: Id,
    nomeDepto :: String,
    descricaoDepto :: String,
    idGerenteDepto :: Id, -- Chave estrangeira. Referência ao Funcionario.
    qtdFuncionarioDepto :: Int,
    registroPresencaDepto :: Registro -- Esse aqui não sei bem qual tipo...
} deriving (Show,Read,Eq)
    
data Status = Ativo |  Afastado | Desligado deriving(Show,Read,Eq)

data Funcionario = Funcionario{
    idFunc :: CPF,
    nomeFunc :: String,
    dataNascimentoFunc :: Day,
    generoFunc :: String,
    emailFunc :: String,
    telefoneFunc :: String,
    enderecoFunc :: String,
    statusFunc :: Status,
    linkLinkedinFunc :: String,
    cargoFunc :: Id, -- Chave estrangeira. Referência ao Cargo.
    deptoFunc :: Id, -- Chave estrangeira. Referência ao Departamento.
    historicoAlteracoesFunc :: [String],
    cargaHorariaFunc :: Int, -- Chave estrangeira ???
    dataAdmissaoFunc :: Day

} deriving (Show, Read, Eq)

data SistemaBancoDadosRH = SistemaBancoDadosRH {
    funcionarios :: [Funcionario],
    cargos :: [Cargo],
    departamento :: [Departamento]
} deriving (Show, Read)

data SistemaJornadaLicenca = SistemaJornadaLicenca {
    licencas :: [Licenca],
    jornadasSemanais :: [EscalaSemanal]
} deriving (Show, Read, Eq)

data Modalidade = Presencial | Remoto deriving (Show, Read, Eq)

data Presenca = Presenca {
    idDepartamento :: Id,
    tipoPresenca :: Modalidade,
    checkIn :: TimeOfDay,
    checkOut :: TimeOfDay,
    compareceu :: Bool,
    justificativa :: String
} deriving (Show, Read, Eq)

data SistemaDePresenca = SistemaDePresenca {
    presencasRegistradas :: Map.Map CPF (Map.Map Day Presenca)
} deriving (Show, Read, Eq)

data TipoAfastamento = AfastamentoMedico | AcidenteDeTrabalho | LicencaMaternidade | LicencaPaternidade | AusenciaJustificada deriving (Show, Read, Eq)

data Documentacao = Documentacao {
    descricaoDocumento :: String,
    dataEnvioDocumento :: Day
} deriving (Show, Read, Eq)

data Afastamento = Afastamento {
    idAfastamento :: Id,
    cpfFuncionario :: CPF,
    tipoAfastamento :: TipoAfastamento,
    dataInicioAfastamento :: Day,
    dataFimAfastamento :: Day,
    descricaoAfastamento :: String,
    documentacao :: Maybe Documentacao
} deriving (Show, Read, Eq)


data TiposDeLicenca = Casamento | Maternidade | Paternidade | Atestado | Luto | DoacaoSangue deriving (Show, Read, Eq)

data Licenca = Licenca {
    funcionarioLicenca :: CPF,
    tipoLicensa :: TiposDeLicenca,
    dataInicio :: Day,
    dataFim :: Day,
    descricao :: String
} deriving (Show, Read, Eq)

data CicloFolga = Folga {
    ultimaFolga :: Day,
    dataFolga :: Day
} deriving (Show, Read, Eq)


data JornadaDiaria = JornadaDiaria {
    inicio :: Int, -- hora (0-23)
    fim :: Int  -- hora (0-23)
} deriving (Show, Read, Eq)

data EscalaSemanal = EscalaSemanal {
    idFuncionarioSemanal :: CPF,
    diasTrabalho :: [Day],
    jornadas :: [JornadaDiaria],
    cicloFolga :: CicloFolga
} deriving (Show, Read, Eq)

data StatusFerias = Planejada| EmAndamento| Concluida deriving (Show, Read, Eq)

data Ferias = Ferias { 
    inicioFerias :: Day, 
    fimFerias :: Day,
    diasUtilizados :: Int, 
    statusFerias :: StatusFerias
} deriving (Show, Read, Eq)

data PeriodoAquisitivo = PeriodoAquisitivo { 
    inicioAquisitivo :: Day, 
    fimAquisitivo :: Day
} deriving (Show, Read, Eq)

data PeriodoConcessivo = PeriodoConcessivo { 
    inicioConcessivo :: Day, 
    fimConcessivo :: Day
} deriving (Show, Read, Eq)

data StatusCiclo = Vigente | Concluido | Vencido | Esperado deriving (Show, Read, Eq)

data CicloFerias = CicloFerias { 
    periodoAquisitivo :: PeriodoAquisitivo, 
    periodoConcessivo :: PeriodoConcessivo, 
    saldoDias :: Int, 
    faltas :: Int,
    feriasDoCiclo :: [Ferias],
    statusCiclo :: StatusCiclo
} deriving (Show, Read, Eq)

data RegistroFeriasFuncionario = RegistroFeriasFuncionario { 
    dataAdmissao :: Day,
    ciclos :: [CicloFerias]
} deriving (Show)

data GerenciadorFerias = GerenciadorFerias { 
    registros :: Map.Map CPF RegistroFeriasFuncionario
} deriving (Show)
