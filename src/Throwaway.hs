module Throwaway where

addNumbers :: Int
addNumbers = 10 + 20

fatorialInteiro :: Int -> Int
fatorialInteiro 0 = 1
fatorialInteiro 1 = 1
fatorialInteiro n = n * fatorialInteiro (n-1)

all4Equal :: Integer-> Integer -> Integer -> Integer -> Bool
all4Equal a b c d = (a == b) && (b == c) && (a == d)

equalCount :: Integer -> Integer -> Integer -> Integer
equalCount a b c | (a == b)   &&  (c == b)                                                    = 3
                 | (a==b)     &&  (a/=c)           || (a==c)&&(a/=b)   || (c==b) && (c/=a)    = 2
                 | (a==b)     &&  (b/=c) && (a/=c) || (c==b) && (b/=a) && (c/=a)              = 1
                 | otherwise                                                                  = 0


mediaInt :: Double -> Double -> Double -> Double
mediaInt a b c = (a+b+c)/3


protocoloVacinacao :: Int -> Bool -> Bool -> Int
protocoloVacinacao  idade comorbidade profissional | (profissional == True) || (idade >= 70) = 1                --Mes 1 Profissional de saúde ou ter mais de 70 anos.  
                                                   | (idade >=65 && idade < 70 && comorbidade == False) = 2
                                                   | (idade >=60 && idade < 65 && comorbidade == False) = 3
                                                   | (idade >=55 && idade < 60 && comorbidade == False) = 4
                                                   | (idade >=45 && idade < 55 && comorbidade == False) = 5
                                                   | (idade >=40 && idade < 45 && comorbidade == False) = 6
                                                   | (idade >=35 && idade < 40 && comorbidade == False) = 7
                                                   | (idade >=30 && idade < 35 && comorbidade == False) = 8
                                                   | (idade >=25 && idade < 30 && comorbidade == False) = 9
                                                   | (idade >=20 && idade < 25 && comorbidade == False) = 10
                                                   | (idade >=15 && idade < 20 && comorbidade == False) = 11
                                                   | (idade >=10 && idade < 15 && comorbidade == False) = 12
                                                   | (idade >=5  && idade < 10 && comorbidade == False) = 13
                                                   | (idade >=0  && idade < 5  && comorbidade == False) = 14
                                                   | (idade >=65 && idade < 70 && comorbidade == True) = 1
                                                   | (idade >=60 && idade < 65 && comorbidade == True) = 2
                                                   | (idade >=55 && idade < 60 && comorbidade == True) = 3
                                                   | (idade >=45 && idade < 55 && comorbidade == True) = 4
                                                   | (idade >=40 && idade < 45 && comorbidade == True) = 5
                                                   | (idade >=35 && idade < 40 && comorbidade == True) = 6
                                                   | (idade >=30 && idade < 35 && comorbidade == True) = 7
                                                   | (idade >=25 && idade < 30 && comorbidade == True) = 8
                                                   | (idade >=20 && idade < 25 && comorbidade == True) = 9
                                                   | (idade >=15 && idade < 20 && comorbidade == True) = 10
                                                   | (idade >=10 && idade < 15 && comorbidade == True) = 11
                                                   | (idade >=5  && idade < 10 && comorbidade == True) = 12
                                                   | (idade >=0  && idade < 5  && comorbidade == True) = 13

                                                