module Controller.ConsultasBasicas where

import Model.TiposDados

-- FUNCIONÁRIOS
existeFuncionario :: String -> [Funcionario] -> Bool
existeFuncionario _ [] = False
existeFuncionario idPossivel (f:fs)
    | idFunc f == idPossivel = True
    | otherwise = existeFuncionario idPossivel fs


-- DEPARTAMENTOS
existeDepartamento :: Id -> [Departamento] -> Bool
existeDepartamento _ [] = False
existeDepartamento idBuscado (d:ds)
    | idDepto d == idBuscado = True
    | otherwise = existeDepartamento idBuscado ds

-- CARGOS
existeCargo :: Id -> [Cargo] -> Bool
existeCargo _ [] = False
existeCargo idBuscado (c:cs)
    | idCargo c == idBuscado = True
    | otherwise = existeCargo idBuscado cs

quantidadeFuncionariosNoDepto :: Id -> [Funcionario] -> Int
quantidadeFuncionariosNoDepto idDepto =
    length . filter (\f -> deptoFunc f == idDepto)
