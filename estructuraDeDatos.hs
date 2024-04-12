--Clase 3
--Listas por extension
listaDeNumeros :: [Int]
listaDeNumeros = [1,2,3]

listaDeBooleanos :: [Bool]
listaDeBooleanos = [True,False]

longitudDe :: [a] -> Int
longitudDe lista = length lista

--Structs


data Botella = UnaBotella{
    color :: String,
    capacidad :: Int,
    tieneBombilla :: Bool
}

esLinda :: Botella -> Bool
esLinda botella = (color botella) == "Negro" && not (tieneBombilla botella)

--

data Nomu = Nomu{
    capacidadesFisicas :: [String],
    cantidadDeVida :: Int,
    cantidadDeFuerza :: Int,
    poderes :: [Poder]
}

data Poder = Poder{
    cantidadDeCuracionPorUso :: Int,
    cantidadDeDanioPorUso :: Int,
    rangoDeAtaque :: Int,
    probabilidadDeDanioCritico :: Int
}

-- PARTE 1
puedeVer :: Nomu -> Bool
puedeVer nomu = (elem "ojos" (capacidadesFisicas nomu)) 

categoriaNomus :: Nomu -> String
categoriaNomus nomu
    | fuerzaMenorA nomu 3000 && fuerzaMayorA nomu 1000 = "Comun"
    | fuerzaMenorA nomu 10000 && fuerzaMayorA nomu 3000 = "Fuerte"
    | fuerzaMayorA nomu 10000 = "high-end"
    | otherwise = "Pichi"

fuerzaMenorA :: Nomu -> Int -> Bool
fuerzaMenorA nomu fuerza = (< fuerza) . cantidadDeFuerza $ nomu

fuerzaMayorA :: Nomu -> Int -> Bool
fuerzaMayorA nomu fuerza = (> fuerza) . cantidadDeFuerza $ nomu

-- PARTE 2
probabilidadDeDanioCriticoDelUltimoPoder :: Nomu -> Int
probabilidadDeDanioCriticoDelUltimoPoder nomu = probabilidadDeDanioCritico . last . poderes $ nomu

poderCuerpoACuerpo :: Poder -> Bool
poderCuerpoACuerpo poder = (< 100) . rangoDeAtaque $ poder

soloDeCuracion :: Poder -> Bool
soloDeCuracion poder = ((== 0 ) . cantidadDeDanioPorUso $ poder) && ((> 0) . cantidadDeCuracionPorUso $ poder)