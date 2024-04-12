
sueldoPorCargo :: String -> Float
sueldoPorCargo "titular" = 149000
sueldoPorCargo "adjunto" = 116000
sueldoPorCargo "ayudante" = 66000

bonusPorAntiguedad :: Int -> Float
bonusPorAntiguedad anios 
    | anios =< 5 && anios >= 3 = 1.20
    | anios < 10 = 1.30
    | anios =< 24 = 1.50
    | anios > 24 = 2.2

cantidadPorHoras :: Int -> Float
cantidadPorHoras horasTrabajadas 
    | horasTrabajadas <= 25 && horasTrabajadas > 10 =  2
    | horasTrabajadas > 25 && horasTrabajadas < 35 =  3
    | horasTrabajadas > 37 =  4
    | otherwise = 1

cantidadPorHoras2 :: Int -> Float
cantidadPorHoras2 horasTrabajadas = round (horasTrabajadas / 10 )

sueldoDocente :: String -> Int -> Int -> Float
sueldoDocente puesto antiguedad horasTrabajadas = sueldoPorCargo puesto * cantidadPorHoras horasTrabajadas * bonusPorAntiguedad antiguedad