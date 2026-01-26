module Controller.GerenciaCargos where

import Model.TiposDados
import Controller.ConsultasBasicas

adicionarCargo :: Cargo -> [Cargo] -> Either String [Cargo]
adicionarCargo novoCargo listaCargos
    | existeCargo (idCargo novoCargo) listaCargos =
        Left "Erro: Já existe um cargo com o ID informado!"
    | salario novoCargo < 0 =
        Left "Erro: Salário não pode ser negativo!"
    | cargaHoraria novoCargo <= 0 || cargaHoraria novoCargo > 44 =
        Left "Erro: Carga horária inválida!"
    | otherwise =
        Right (novoCargo : listaCargos)

adicionarCargoValidado
    :: Cargo
    -> [Departamento]
    -> [Cargo]
    -> Either String [Cargo]
adicionarCargoValidado novoCargo departamentos cargos
    | not (existeDepartamento (deptoAssociado novoCargo) departamentos) =
        Left "Erro: Departamento associado não existe!"
    | existeCargo (idCargo novoCargo) cargos =
        Left "Erro: Já existe cargo com esse ID!"
    | salario novoCargo < 0  =
        Left "Erro: Salário não pode ser negativo!"
    | otherwise =
        adicionarCargo novoCargo cargos

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