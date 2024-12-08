-- Implementação quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let menores = quicksort [y | y <- xs, y <= x]
        maiores = quicksort [y | y <- xs, y > x]
    in menores ++ [x] ++ maiores

main :: IO ()
main = do
    let lista = [10, 3, 5, 7, 2, 8, 1, 9]
    putStrLn $ "Lista original: " ++ show lista
    putStrLn $ "Lista ordenada: " ++ show (quicksort lista)
