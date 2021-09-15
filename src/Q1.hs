-- Um país deseja criar um protocolo de vacinação que informa a quantidade de meses para uma pessoa tomar a vacina de acordo com 3 fatores:

-- - idade.

-- - presença de comorbidades

-- - se é profissional de saúde.

-- As regras são:

-- a) caso seja profissional de saúde, o seu mês será 1. 

-- b) pessoas acima de 70 anos também são vacinadas no mês 1. 

-- c) abaixo de 70 anos, a cada cinco anos aumenta-se a quantidade de meses. Assim, pessoas entre 65 e 70 (incompletos) são o mês 2, entre 60 e 65 são no mês 3, e assim por diante. 

-- d) caso a pessoa tenha comorbidade, seu mês de referência é reduzido em uma posição, com o mínimo sendo o mês 1.

-- Assim, crie uma função em Haskell que recebe uma tupla de três elementos (Int, Bool, Bool) representando uma pessoa, onde o primeiro elemento é a idade, o segundo é um booleano informando se a pessoa tem comorbidade e o terceiro é um booleano informando se a pessoa é profissional de saúde, e deve retornar a quantidade de meses que a pessoa deverá esperar para ser vacinada.
module Q1 where

--Optei por usar uma simples validação de requisitos e atribui o valor do mes.
protocoloVacinacao :: (Int,Bool,Bool) -> Int    --aceita idade Int, comorbidade Bool e profissional Bool
protocoloVacinacao  (idade,comorbidade,profissional)    | (profissional == True) || (idade >= 70) = 1                --Mes 1 Profissional de saúde ou ter mais de 70 anos.  
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
