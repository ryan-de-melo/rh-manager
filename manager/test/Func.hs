module Func (runFuncionarioTests) where

import Model.TiposDados
import System.IO (hSetEncoding,stdout,stdin, stderr,utf8)
import Controller.GerenciaFuncionarios
import Data.Time (fromGregorian,Day)

assert :: Bool -> String -> IO ()
assert condicao msg
    | condicao =  putStrLn $ "Testes PASSARAM:  " ++ msg
    | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

dataTeste :: Day
dataTeste = fromGregorian 1990 1 13
dataAdmissaoTeste = fromGregorian 2025 1 13

funcionarioTeste1 :: Funcionario
funcionarioTeste1 = Funcionario "12345678900" "Zeca" dataTeste "M" "zeca@meudominio.com" "11112222" "JP" Ativo "link1" 1 1 [] 30 dataAdmissaoTeste

funcionarioTeste2 :: Funcionario
funcionarioTeste2 = Funcionario "98765432100" "Zefa" dataTeste "F" "zefa@meudominio.com" "22223333" "SP" Desligado "link2" 1 1 [] 40 dataAdmissaoTeste

funcionarioTeste3 :: Funcionario
funcionarioTeste3 = Funcionario "12341234999" "Juca" dataTeste "M" "juca@meudominio.com" "33334444" "SP" Afastado "link3" 1 1 [] 40 dataAdmissaoTeste

funcionarioTeste4 :: Funcionario 
funcionarioTeste4 =  Funcionario "99988877700" "Cajuina" dataTeste "F" "cajuina@meudominio.com" "99998888" "RS" Ativo "link4" 3 3 [] 40 dataAdmissaoTeste
-- Inicio dos testes...

adicionarFuncionarioTeste :: IO()
adicionarFuncionarioTeste = do
    putStrLn "\n\n * Testando adicionar funcionário * "
    let resultado = adicionarFuncionario funcionarioTeste1 []
    case resultado of
        Right lista -> assert (length lista == 1) "Adicionado com sucesso! A lista agora tem 1 elemento!"
        Left _ -> assert False "Erro ao adicionar! A lista permanece vazia."

adicionarFuncionarioDuplicadoTeste :: IO()
adicionarFuncionarioDuplicadoTeste = do
    putStrLn "\n\n * Testando adicionar um funcionário já existente * "
    let listaAtual = [funcionarioTeste1]
    let resultado = adicionarFuncionario funcionarioTeste1 listaAtual
    case resultado of
        Right _ -> assert False "Erro grave. Duplicou!"
        Left msg -> assert True ("Sucesso! Não permitiu duplicar!")

adicionarCPFInvalidoTeste :: IO()
adicionarCPFInvalidoTeste = do
    putStrLn "\n\n * Testando adicionar um CPF inválido * "
    let funcIdIncorreto = Funcionario "abc" "Zeca" dataTeste "M" "zeca@meudominio.com" "11112222" "JP" Ativo "link1" 1 1 [] 30 dataAdmissaoTeste
    let resultado = adicionarFuncionario funcIdIncorreto []
    case resultado of
        Right _ -> assert False "Erro grave. Adicionou um funcionário com CPF inválido! "
        Left msg -> assert True "Sucesso! Não permitiu adicionar um CPF inválido!"

modificarFuncionarioTeste :: IO()
modificarFuncionarioTeste = do
    putStrLn "\n\n *Testando modificar um funcionário* "
    let listaFuncionario = [funcionarioTeste1]
    let funcionarioTeste1Novo = Funcionario "12345678900" "ZecaModificado" dataTeste "M" "zeca@meudominio.com" "55554444" "JP" Ativo "link1" 1 1 [] 40 dataAdmissaoTeste
    let resultado = modificarFuncionario funcionarioTeste1Novo listaFuncionario
    case resultado of
        Left msg -> assert False ("Falha na operação " ++ msg) 
        Right [fMod] -> assert ((nomeFunc fMod == "ZecaModificado") && (telefoneFunc fMod == "55554444") ) "Sucesso! Informações do funcionário foram modificadas."
        

buscarFuncionarioTeste :: IO()
buscarFuncionarioTeste = do
    putStrLn "\n\n *Testando buscar um funcionário* "
    let listaAtual = [funcionarioTeste1,funcionarioTeste2]
    
    case buscarFuncionario "12345678900" listaAtual of
        Just f -> assert (nomeFunc f == "Zeca" ) "O funcionário Zeca foi encontrado."
        Nothing -> assert False "Erro: Não foi encontrado usuário com este ID."

    case buscarFuncionario "98765432100" listaAtual of
        Just f -> assert (nomeFunc f == "Zefa") "A funcionária Zefa foi encontrada."
        Nothing -> assert False "Erro: Não foi encontrado usuário com este ID."

    case buscarFuncionario "77889911222" listaAtual of
        Nothing -> assert True "Retornou Nothing para um consulta de ID inexistente. Correto."
        Just _ -> assert False "Erro: Retornou algum funcionário indevidamente."

excluirFuncionarioExistenteTeste :: IO()
excluirFuncionarioExistenteTeste = do
    putStrLn "\n\n *Testando excluir um funcionário existente* "
    let listaAtual = [funcionarioTeste2,funcionarioTeste3]

    case excluirFuncionario "98765432100" listaAtual of
         Left msg -> assert False ("Falha na remoção. " ++ msg)
         Right listaSemZefa -> do

            case excluirFuncionario "12341234999" listaSemZefa of
                Left msg -> assert False ("Falha na remoção. " ++ msg)
                Right listaVazia -> assert (null listaVazia) "Operação de remoção dos únicos dois funcionários realizada com sucesso. Lista vazia, agora!"


excluirFuncionarioInexistenteTeste :: IO()
excluirFuncionarioInexistenteTeste = do
    putStrLn "\n\n* Testando excluir um funcionário inexistente * "
    let listaAtual =  [funcionarioTeste1]
    let resultado = excluirFuncionario "00001111222" listaAtual
    case resultado of
        Left _ -> assert True "Retornou erro ao tentar excluir um ID inexistente na lista. Correto!"
        Right _ -> assert False "Erro:  aceitou tentar remover um ID inexistente na lista. "


exibirFuncionarioAfastadoTeste :: IO()
exibirFuncionarioAfastadoTeste = do
    putStrLn "\n\n * Testando listar funcionários afastados * "
    let listaGeral = [funcionarioTeste1,funcionarioTeste2,funcionarioTeste3, funcionarioTeste4]
    let resultado = exibirFuncionarioAfastado listaGeral
    
    verificarFiltro resultado
    where
        verificarFiltro :: [Funcionario] -> IO()
        verificarFiltro lista
            |  length lista == 1 && nomeFunc (head lista) == "Juca" = assert True "Filtro aplicado com sucesso."
            |  otherwise = assert False "Falha ao filtrar!"

exibirFuncionarioAtivoTeste :: IO()
exibirFuncionarioAtivoTeste = do
    putStrLn "\n\n * Testando listar funcionários ativos * "
    let listaGeral = [funcionarioTeste1,funcionarioTeste2,funcionarioTeste3,funcionarioTeste4]
    let resultado = exibirFuncionarioAtivo listaGeral

    verificarFiltro resultado
    where
        verificarFiltro :: [Funcionario] -> IO()
        verificarFiltro lista
            | (length lista /= 2) = assert False "Falha! Retorno deveria ter sido de duas pessoas!"
            | "Zeca" `notElem` nomesProcurados = assert False "Falha! Deveria ter retornado Zeca na lista. Não foi encontrado!"
            | "Cajuina" `notElem` nomesProcurados = assert False "Falha! Deveria ter retornado Ana na lista. Não foi encontrada!"
            | otherwise = assert True "Filtro aplicado com sucesso! Funcionários ativos: Ana e Zeca!"
            where
                nomesProcurados = map nomeFunc lista

exibirFuncionarioDesligadoTeste :: IO()
exibirFuncionarioDesligadoTeste = do
    putStrLn "\n\n * Testando listar funcionários desligados * "
    let listaGeral = [funcionarioTeste1,funcionarioTeste2,funcionarioTeste3,funcionarioTeste4]
    let resultado = exibirFuncionarioDesligado listaGeral
    
    verificarFiltro resultado

    where
        verificarFiltro :: [Funcionario] -> IO()
        verificarFiltro lista
            | (length lista == 1 && nomeFunc (head lista) == "Zefa") = assert True "Filtro *Desligado* aplicado com sucesso."
            | otherwise = assert False "Falha ao filtrar."

runFuncionarioTests :: IO ()
runFuncionarioTests = do
  putStrLn "\n* Testes inicializados! *"

  adicionarFuncionarioTeste
  adicionarFuncionarioDuplicadoTeste
  adicionarCPFInvalidoTeste
  modificarFuncionarioTeste
  buscarFuncionarioTeste
  excluirFuncionarioExistenteTeste
  excluirFuncionarioInexistenteTeste
  exibirFuncionarioAfastadoTeste
  exibirFuncionarioAtivoTeste
  exibirFuncionarioDesligadoTeste

  putStrLn "\n* Testes encerrados! *\n"
