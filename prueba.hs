--1
pesoPino :: Int -> Int
pesoPino altura = primerosMetros altura * 300 + metrosRestantes altura * 200

primerosMetros :: Int -> Int
primerosMetros = min 3

metrosRestantes :: Int -> Int
metrosRestantes altura = max 0 (altura - 3)

--2

esPesoUtil :: Int -> Bool
esPesoUtil peso = peso > 400 && peso < 1000

--3

sirvePino :: Int -> Bool
sirvePino = esPesoUtil . pesoPino
