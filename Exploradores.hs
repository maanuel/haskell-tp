module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

import Prelude hiding ((<*>))
import Test.HUnit

--Definiciones de tipos

type Explorador a b = a -> [b]

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

data RoseTree a = Rose a [RoseTree a] deriving Eq

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show x = concatMap (++"\n") (padTree 0 x)

padTree :: Show a => Int -> RoseTree a -> [String]
padTree i (Rose x ys) =  ((pad i) ++  (show x) ) : (concatMap (padTree (i + 4)) ys)

pad :: Int -> String
pad i = replicate i ' '


instance Show a => Show (AB a) where
  show = padAB 0 0
  
padAB _ _ Nil = ""
padAB n base (Bin i x d) = pad n ++ show x ++ padAB 4 (base+l) i ++ "\n" ++ padAB (n+4+base+l) base d where l = length $ show x

--Ejercicio 1
expNulo :: Explorador a b
expNulo _ = []

expId :: Explorador a a
expId a = [a]

expHijosRT :: Explorador (RoseTree a) (RoseTree a)
expHijosRT (Rose a []) = []
expHijosRT (Rose a xb) = xb

expHijosAB :: Explorador(AB a) (AB a)
expHijosAB Nil= []
expHijosAB (Bin i x d)= [ i , d]

expTail :: Explorador [a] a
expTail [] = []
expTail (x:xs) = xs

--Ejercicio 2
--foldNat f a b 
foldNat :: ( Integer -> b -> b) -> b -> Integer -> b
foldNat f b 0 = b
foldNat f b x = f x ( foldNat f b (x-1))

--foldRT f b
foldRT :: ( a -> [b] -> b) -> (RoseTree a) -> b
foldRT f ( Rose a xs )= f a ( map (foldRT f) xs )

--foldAB f z b
-- z va a ser nuestro caso base
foldAB :: ( a -> b -> b -> b) -> b -> (AB a) -> b
foldAB f z Nil = z
foldAB f z (Bin i x d) = f x (foldAB f z i)(foldAB f z d)

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons = foldr (\x y -> [[x]] ++ y) []

sufijos :: Explorador [a] [a]
sufijos = foldr (\x (y:ys) -> [x:y]++(y:ys)) [[]] 

--Ejercicio 4
-- versión con recursión explicita
-- usar el esquema de fold no nos parecía conveniente porque necesitabamos
-- no solo el resultado anterior, sino todos los anteriores. 
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman 0 = [[]]
listasQueSuman n = concat[ map ((:) (n-i)) (listasQueSuman i) | i <-[0..(n-1)] ]

-- intento con fold
-- listasQueSumanHasta si arrastra los resultados anteriores.
--listasQueSumanHasta n = foldNat f [[]] n
--            where f = (\n recu -> [ ((n-sum(xs):xs)) | xs <- recu ] ++ recu)
--listasQueSuman' n = filter (suma n) (listasQueSumanHasta n)


--Ejercicio 5
preorder :: Explorador (AB a) a
preorder = foldAB (\x i d -> [x]++i++d) []

inorder :: Explorador (AB a) a
inorder = foldAB (\x i d -> i ++ [x] ++ d) []

-- toma un arbol binario de tipo a y devuelve una lista con elementos de tipo a
postorder :: Explorador (AB a) a 
postorder = foldAB (\x i d -> i ++ d ++ [x]) []


--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = foldRT (\x y  -> x : (concat y) )

fRT x [] = [x]
fRT x lss = concat lss

hojasRT :: Explorador (RoseTree a) a
hojasRT = foldRT fRT


-- AUXILIAR PARA RAMASRT
unirAdelante a [] = [[a]]
unirAdelante a ls = [ (a:y) | y<- ls ]

-- recursión es un lista de lista de listas
-- porque a la lista de hijos se le aplica map a cada hijo de una funcion que devuelve una lista de listas.
-- tenemos una lista que tiene como elementos a la lista de caminos (una por hijo)
-- concatenamos y tenemos una lista de caminos (lista de lista - indistintamente de que hijo era) y luego agregamos el valor actual al principio.
-- ej:
-- [hijo0, hijo1]
-- [ramasRT hijo0, ramasRT hijo1] aca ya tenemos una lista de listas de listas
-- [ [[x1,x2,x3], [x1,x5]] , [[x9, x11], [x9,x12]]]
-- las concatenamos
-- [[x1,x2,x3], [x1,x5], [x9, x11], [x9,x12]]
-- agregamos el valor actual adelante de cada una y tenemos los caminos desde el valor actual.

ramasRT :: Explorador (RoseTree a) [a]
-- ramasRT rose = foldRT (\valor recursion -> (map (\ x -> [valor]++x) (concat recursion))) rose
ramasRT rose = foldRT (\valor recursion -> unirAdelante valor (concat recursion)) rose

--Ejercicio 7

--para testear

--isRose :: RoseTree a -> Bool
--isRose (Rose x []) = True
--isRose (Rose x l) = False

-- ifExp toma una funcion y dos exploradores, y devuelve un nuevo explorador
-- si pensamos e = (ifExp f exp1 exp2), e es un explorador que, dada una estructura x de tipo a, 
-- devuelve exp1 x si f x es verdadero, y exp2 x si f x es falso
-- es decir, ifExp f exp1 exp2 devuelve una funcion
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp f exp1 exp2 = (\x -> if f x then (exp1 x) else (exp2 x)) 

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) f1 f2 = \x -> (f1 x) ++ (f2 x)

--Ejercicio 9
(<.>) :: Explorador b c -> Explorador a b -> Explorador a c
(<.>) = (\f g x -> concat (map f (g x) ))

--Ejercicio 10
(<^>) :: Explorador a a -> Integer -> Explorador a a
(<^>) exp n = foldNat (\x rec -> ( (<.>) exp rec ) ) exp (n-1)

-- auxiliar para listasDeLongitud
-- esta listasQueSuman'' admite tambien una longitud deseada.
-- si usamos la original el algoritmo demora más de lo deseado.
listasQueSuman'' :: Integer -> Integer -> [[Integer]]
listasQueSuman'' n 1 = [[n]]
listasQueSuman'' n l = [ xs ++ [n-i]| i <- [(l-1)..(n-1)], xs <- (listasQueSuman'' i  (l-1))]

--Ejercicio 11 (implementar al menos una de las dos)

-- No pudimos pasar la recursión explicita a un esquema de recursión (dentro de listasQueSuman'')
-- por eso resolvimos el siguiente sin recursión explícita.
listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud l = [ xs | n<-[l..], xs <- (listasQueSuman'' n l) ]

(<*>) :: Explorador b b -> Explorador b [b] 
(<*>) f = g
        where g a = takeWhile (\x -> not (null x)) (iterate ( \lsA -> concat ( map f lsA)) [a])

-- **** Tests ****
-- auxiliar para un test
isRose :: RoseTree a -> Bool
isRose (Rose x []) = True
isRose (Rose x l) = False

-- Ejercicio 1
testExpNulo = TestCase (assertEqual "resultado de expNulo," ([] :: [[Bool]]) (expNulo (\x -> [])))
testExpId = TestCase (assertEqual "resultado de expId," ([True]) (expId True) )
testExpHijosRT = TestCase (assertEqual "Resultado de expHijosRT," (  [Rose 3 [(Rose 2 [])]]  ) (expHijosRT (Rose 5 [(Rose 3 [(Rose 2 [])])]) ) )
testExpHijosAB = TestCase (assertEqual "Resultado expHijosAB," ([(Bin Nil 3 Nil), Nil])  (expHijosAB (Bin (Bin Nil 3 Nil) 2 Nil)) )
testExpTail = TestCase (assertEqual "Resultado expTail," ([2,3]) (expTail [1,2,3]) ) 

--Ejercicio 3
testSingletons = TestCase (assertEqual "resultado de singletons, " ([ [[9,10]] ,[[10]] ]) (singletons [[9,10], [10]]))
testSufijos = TestCase (assertEqual "resultado de sufijos," ([[1,2,3], [2,3], [3], [] ]) (sufijos [1,2,3]))

--Ejercicio 4
testListasQueSuman = TestCase( assertEqual "resultado de listasQueSuman," ([[2], [1,1]]) (listasQueSuman 2))

--Ejercicio 5
testPreorder = TestCase ( assertEqual "resultado preorder," ([1, 2, 4, 5, 3, 6, 7]) (preorder (Bin (Bin (Bin Nil 4 Nil) 2 (Bin Nil 5 Nil)) 1 (Bin (Bin Nil 6 Nil) 3 (Bin Nil 7 Nil)))))
testInorder = TestCase ( assertEqual " resultado inorder," ([4, 2, 5, 1, 6, 3, 7]) (inorder (Bin (Bin (Bin Nil 4 Nil) 2 (Bin Nil 5 Nil)) 1 (Bin (Bin Nil 6 Nil) 3 (Bin Nil 7 Nil)))))
testPostorder = TestCase ( assertEqual "resultado postorder," ([4, 5, 2, 6, 7, 3, 1]) (postorder (Bin (Bin (Bin Nil 4 Nil) 2 (Bin Nil 5 Nil)) 1 (Bin (Bin Nil 6 Nil) 3 (Bin Nil 7 Nil)))))

-- Ejercicio 6
testDfsRT = TestCase( assertEqual "resultado de dfsRT," ([1,2,1,1]) (dfsRT (Rose 1 [(Rose 2 [(Rose 1 [])]),  (Rose 1 [])]) ))
testHojasRT = TestCase( assertEqual "resultado de hojasRT," ([1,1]) (hojasRT (Rose 1 [(Rose 2 [(Rose 1 [])]),  (Rose 1 [])])) )
testRamasRT = TestCase (assertEqual "resultado de ramasRT," ([[1,2,1],[1,1]]) (ramasRT (Rose 1 [(Rose 2 [(Rose 1 [])]),  (Rose 1 [])])))

--Ejercicio 7
testIfExpTrue = TestCase ( assertEqual "resultado ifExp," ([2]) (ifExp isRose dfsRT hojasRT (Rose 2 [])) )
testIfExpFalse = TestCase ( assertEqual "resultado ifExp," ([3,1,2]) ( ifExp isRose dfsRT hojasRT (Rose 5 [(Rose 3 []), (Rose 4 [ (Rose 1 [])]), (Rose 2 [])]) ) )

-- Ejercicio 8
testMasMas = TestCase (assertEqual "resultado de ++, " ([10,20])  ( ((\x -> [x*5]) <++> (\y -> [y*10])) 2)  )

--Ejercicio 9
testPunto = TestCase ( assertEqual "resultado <.>," ([1,1,1,2,1,3,1,4,1,5]) (  (<.>) ((:) 1) singletons [1,2,3,4,5] ) )

--ejercicio 11
testAsterisco = TestCase (assertEqual "resultado de *," ([ [(Rose 1 [(Rose 2 []),(Rose 3 [])])], [(Rose 2 []), (Rose 3 [])] ]) ((<*>) expHijosRT (Rose 1 [(Rose 2 []),(Rose 3 [])])))

-- test = TestCase( assertEqual "resultado de ," () ())
-- Las funciones folds no las testeamos directamente, ya son testeadas con los ejercicios
-- que las usan.
tests = TestList [TestLabel "expNulo" testExpNulo, 
                  TestLabel "expId" testExpId,
                  TestLabel "expHijosRT" testExpHijosRT,
                  TestLabel "expHijosAB" testExpHijosAB,
                  TestLabel "expTail" testExpTail,
                  TestLabel "singletons" testSingletons,
                  TestLabel "sufijos" testSufijos,
                  TestLabel "listasQueSuman" testListasQueSuman,
                  TestLabel "dfsRT" testDfsRT,
                  TestLabel "hojasRT" testHojasRT,
                  TestLabel "ramasRT" testRamasRT,
                  TestLabel "++" testMasMas,
                  TestLabel "*" testAsterisco,
                  TestLabel "preorder" testPreorder,
                  TestLabel "inorder" testInorder,
                  TestLabel "postorder" testPostorder,
                  TestLabel "ifExpTrue" testIfExpTrue,
                  TestLabel "ifExpFalse" testIfExpFalse,
                  TestLabel "punto" testPunto
                ]




main :: IO Counts
main = do runTestTT tests