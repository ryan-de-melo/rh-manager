module Controller.GerenciaFerias where

import Model.TiposDados
import Data.Time
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Bool (Bool)

gerarCiclos :: Day -> [CicloFerias]
gerarCiclos admissao =
  let criar inicioAq =
        let fimAq = addGregorianYearsClip 1 inicioAq
            inicioCo = fimAq
            fimCo = addGregorianYearsClip 1 inicioCo
        in CicloFerias { 
            periodoAquisitivo = PeriodoAquisitivo inicioAq fimAq, 
            periodoConcessivo = PeriodoConcessivo inicioCo fimCo, 
            saldoDias = 30, 
            faltas = 0, 
            feriasDoCiclo = [], 
            statusCiclo = Esperado
          }
      prox inicioAq =
        let ciclo = criar inicioAq
            proximoInicioAq =
              fimConcessivo (periodoConcessivo ciclo)
        in ciclo : prox proximoInicioAq
  in prox admissao


admitirFuncionario :: CPF -> Day -> GerenciadorFerias -> Either String GerenciadorFerias
admitirFuncionario cpf admissao ger
  | Map.member cpf (registros ger) = Left "Funcionário já cadastrado."
  | otherwise =
      let registro = 
            RegistroFeriasFuncionario { 
              dataAdmissao = admissao, 
              ciclos = gerarCiclos admissao
            }
      in Right ger { registros = Map.insert cpf registro (registros ger) }

atualizarStatusFerias :: Day -> Ferias -> Ferias
atualizarStatusFerias ref f
  | ref < inicioFerias f = f { statusFerias = Planejada }
  | ref > fimFerias f = f { statusFerias = Concluida }
  | otherwise = f { statusFerias = EmAndamento }

atualizarStatusCiclo :: Day -> CicloFerias -> CicloFerias
atualizarStatusCiclo ref ciclo
  | ref > fimConcessivo pc && saldoDias ciclo > 0 = ciclo { statusCiclo = Vencido }
  | ref > fimConcessivo pc && saldoDias ciclo == 0 = ciclo { statusCiclo = Concluido }
  | ref < inicioConcessivo pc = ciclo { statusCiclo = Esperado }
  | otherwise = ciclo { statusCiclo = Vigente }
  where
    pc = periodoConcessivo ciclo

atualizarCiclo :: Day -> CicloFerias -> CicloFerias
atualizarCiclo ref ciclo =
  atualizarStatusCiclo ref ciclo {
    feriasDoCiclo = map (atualizarStatusFerias ref) (feriasDoCiclo ciclo)
  }

calcularSaldoPorFaltas :: Int -> Int
calcularSaldoPorFaltas f
  | f <= 5   = 30
  | f <= 14  = 24
  | f <= 23  = 18
  | f <= 32  = 12
  | otherwise = 0

cicloPorDataAquisitiva :: Day -> [CicloFerias] -> Either String ([CicloFerias], CicloFerias, [CicloFerias])
cicloPorDataAquisitiva _ [] =
  Left "Nenhum ciclo aquisitivo encontrado para a data informada."
cicloPorDataAquisitiva dia (c:cs)
  | dia < inicioAquisitivo (periodoAquisitivo c) = Left "Data anterior ao início de qualquer período aquisitivo."
  | dia >= inicioAquisitivo (periodoAquisitivo c) && dia < fimAquisitivo (periodoAquisitivo c) = Right ([], c, cs)
  | otherwise = do
      (antes, ciclo, depois) <- cicloPorDataAquisitiva dia cs
      Right (c : antes, ciclo, depois)


cicloPorDataConcessiva :: Day -> [CicloFerias] -> Either String ([CicloFerias], CicloFerias, [CicloFerias])
cicloPorDataConcessiva _ [] =
  Left "Nenhum ciclo concessivo encontrado para a data informada."
cicloPorDataConcessiva dia (c:cs)
  | dia < inicioConcessivo (periodoConcessivo c) = Left "Data anterior ao início de qualquer período concessivo."
  | dia >= inicioConcessivo (periodoConcessivo c) && dia <= fimConcessivo (periodoConcessivo c) = Right ([], c, cs)
  | otherwise = do
      (antes, ciclo, depois) <- cicloPorDataConcessiva dia cs
      Right (c : antes, ciclo, depois)


aplicarFaltaNoCiclo :: Int -> CicloFerias -> CicloFerias
aplicarFaltaNoCiclo novas ciclo =
  let totalFaltas = faltas ciclo + novas
      novoSaldo   = calcularSaldoPorFaltas totalFaltas
  in ciclo { 
      faltas = totalFaltas, saldoDias = min (saldoDias ciclo) novoSaldo
    }

registrarFalta :: Day -> RegistroFeriasFuncionario -> Either String RegistroFeriasFuncionario
registrarFalta dia reg = do
  (antes, ciclo, depois) <-
    cicloPorDataAquisitiva dia (ciclos reg)
  let cicloAtualizado =
        aplicarFaltaNoCiclo 1 ciclo
  Right reg {
    ciclos = antes ++ cicloAtualizado : depois
  }

registrarFerias :: CPF -> Ferias -> Day -> GerenciadorFerias -> Either String GerenciadorFerias
registrarFerias cpf ferias dataReferencia ger = do
  reg <-
    maybe
      (Left "Funcionário não encontrado.")
      Right
      (Map.lookup cpf (registros ger))
  (antes, ciclo, depois) <-
    cicloPorDataConcessiva
      (inicioFerias ferias)
      (ciclos reg)
  cicloFinal <- processarFerias dataReferencia ferias ciclo
  let regAtualizado =
        reg { ciclos = antes ++ cicloFinal : depois }
  Right ger {
    registros =
      Map.insert cpf regAtualizado (registros ger)
  }

validarDireitoFerias :: Ferias -> CicloFerias -> Either String ()
validarDireitoFerias ferias ciclo
  | inicioFerias ferias < fimAquisitivo (periodoAquisitivo ciclo) = Left "Funcionário ainda não adquiriu direito às férias neste ciclo."
  | otherwise = Right ()

validarSaldo :: Int -> CicloFerias -> Either String ()
validarSaldo custo ciclo
  | custo > saldoDias ciclo = Left "Saldo insuficiente."
  | otherwise = Right ()

aplicarFeriasNoCiclo :: Day -> Ferias -> CicloFerias -> CicloFerias
aplicarFeriasNoCiclo dataReferencia ferias ciclo =
  let cicloAtualizado =
        atualizarCiclo dataReferencia ciclo
      custo = diasUtilizados ferias
  in cicloAtualizado { 
    saldoDias = saldoDias cicloAtualizado - custo, 
    feriasDoCiclo = ferias : feriasDoCiclo cicloAtualizado
  }

processarFerias :: Day -> Ferias -> CicloFerias -> Either String CicloFerias
processarFerias dataReferencia ferias ciclo = do
  validarDireitoFerias ferias ciclo
  let cicloAtualizado =
        atualizarCiclo dataReferencia ciclo
      feriasTotais =
        ferias : feriasDoCiclo cicloAtualizado
      custo =
        diasUtilizados ferias
  validarFracionamento feriasTotais
  validarSaldo custo cicloAtualizado
  Right cicloAtualizado {
    saldoDias = saldoDias cicloAtualizado - custo,
    feriasDoCiclo = ferias : feriasDoCiclo cicloAtualizado
  }

sobrepoe :: Ferias -> Ferias -> Bool
sobrepoe f1 f2 =
  inicioFerias f1 <= fimFerias f2 && inicioFerias f2 <= fimFerias f1

haSobreposicao :: [Ferias] -> Bool
haSobreposicao ferias =
  or [sobrepoe f1 f2 | f1 <- ferias, f2 <- ferias, f1 /= f2]

validarFracionamento :: [Ferias] -> Either String ()
validarFracionamento ferias
  | qtd > 3 = Left "Férias não podem ser fracionadas em mais de 3 períodos."
  | haSobreposicao ferias = Left "Períodos de férias não podem se sobrepor."
  | qtd == 1 && not (any ((>= 14) . diasUtilizados) ferias) = Left "Férias únicas devem ter no mínimo 14 dias."
  | qtd >= 2 && qtd <= 3 && not (any ((>= 14) . diasUtilizados) ferias) = Left "Em férias fracionadas, um dos períodos deve ter no mínimo 14 dias."
  | qtd >= 2 && any ((< 5) . diasUtilizados) ferias = Left "Em férias fracionadas, nenhum período pode ter menos de 5 dias."
  | otherwise = Right ()
  where
    qtd = length ferias

feriasVencidas :: Day -> CicloFerias -> Bool
feriasVencidas dataReferencia ciclo =
  dataReferencia > fimConcessivo (periodoConcessivo ciclo) && saldoDias ciclo > 0              

temDireitoAFerias :: Day -> CicloFerias -> Bool
temDireitoAFerias ref ciclo =
  ref >= fimAquisitivo (periodoAquisitivo ciclo)
  && ref <= fimConcessivo (periodoConcessivo ciclo)
  && saldoDias ciclo > 0
