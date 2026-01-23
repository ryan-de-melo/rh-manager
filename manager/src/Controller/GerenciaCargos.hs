module Controller.GerenciaCargos where

import Model.TiposDados

existeCargo :: Id -> [Cargo] -> Bool
existeCargo _ [] = False
existeCargo idBuscado (c:cs)
    | idCargo c == idBuscado = True
    | otherwise = existeCargo idBuscado cs

adicionarCargo :: Cargo -> [Cargo] -> Either String [Cargo]
adicionarCargo novoCargo listaCargos
    | existeCargo (idCargo novoCargo) listaCargos =
        Left "Erro: Já existe um cargo com o ID informado!"
    | salario novoCargo < 0 =
        Left "Erro: Salário não pode ser negativo!"
    | cargaHoraria novoCargo <= 0 =
        Left "Erro: Carga horária inválida!"
    | otherwise =
        Right (novoCargo : listaCargos)

adicionarCargoValidado
    :: Cargo
    -> [Funcionario]
    -> [Departamento]
    -> [Cargo]
    -> Either String [Cargo]
adicionarCargoValidado novoCargo funcionarios departamentos cargos
    | not (funcionarioExiste (show (idSupervisor novoCargo)) funcionarios) =
        Left "Erro: Supervisor informado não existe no sistema!"
    | not (existeDepartamento (deptoAssociado novoCargo) departamentos) =
        Left "Erro: Departamento associado não existe!"
    | existeCargo (idCargo novoCargo) cargos =
        Left "Erro: Já existe cargo com esse ID!"
    | otherwise =
        Right (novoCargo : cargos)

modificarCargo :: Cargo -> [Cargo] -> Either String [Cargo]
modificarCargo _ [] =
    Left "Erro: Cargo não encontrado para alteração!"
modificarCargo novoCargo (c:cs)
    | idCargo c == idCargo novoCargo =
        Right (novoCargo : cs)
    | otherwise =
        case modificarCargo novoCargo cs of
            Left msg -> Left msg
            Right novaLista -> Right (c : novaLista)

buscarCargo :: Id -> [Cargo] -> Maybe Cargo
buscarCargo _ [] = Nothing
buscarCargo idBuscado (c:cs)
    | idCargo c == idBuscado = Just c
    | otherwise = buscarCargo idBuscado cs

excluirCargo :: Id -> [Cargo] -> Either String [Cargo]
excluirCargo _ [] =
    Left "Erro: Cargo inexistente!"
excluirCargo idRemocao (c:cs)
    | idCargo c == idRemocao =
        Right cs
    | otherwise =
        case excluirCargo idRemocao cs of
            Left msg -> Left msg
            Right novaLista -> Right (c : novaLista)