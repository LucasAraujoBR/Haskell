-- Crie uma função em Haskell que recebe um inteiro n e um caractere c. Caso 
-- o caractere c seja igual a 'p', sua função deve retornar a soma de todos os número fibonacci pares até n, 
-- caso o caractere c seja igual a 'i',
-- sua função deve retornar a soma de todos os número fibonacci ímpares até n, caso contrário, retorna zero.


module Q2 where


fib :: Int -> Int                   -- função de formula do fibonacci
fib 0 = 0                           -- deixei por padrão fib 0 e fib 1 por serem esse valor padronizado, não sendo necessária a soma
fib 1 = 1
fib n = fib (n-1) + fib (n-2)       -- formula de fibonacci


map' :: (a -> b) -> [a] -> [b]      -- transforma em lista
map' _ [] = []                      -- var vazia
map' f (n:ns) = f n : map' f ns     -- logica do range da função map


sumFib :: Int -> Char -> Int 
sumFib n c | (c == 'p') = sum (map' fib [0,2..n])   -- condicionais para escolha da letra que define se vai ser par ou impar
           | (c == 'i') = sum (map' fib [1,3..n])   -- utilizei a função map para transformar em lista e poder somar os fibonacci
           | otherwise  = 0                         -- [0,2..n] pula de dois em dois fazendo ser par
                                                    -- [1,3..n] segue a ordem de 1 - 3 - 5 ... fazendo ser o somatório ímpar
                                                    -- caso não escolha par ou ímpar ele retorna 0