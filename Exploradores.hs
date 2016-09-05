module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

import Prelude hiding ((<*>))

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
-- HACERLA SIN RECURSION EXPLICITA!!!!!!!!!!!!!!!!!!
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman 0 = [[]]
listasQueSuman n = concat[ map ((:) (n-i)) (listasQueSuman i) | i <-[0..(n-1)] ]

--Ejercicio 5
--preorder :: undefined
preorder Nil = []
preorder (Bin izq val der) = [val]++(preorder izq)++(preorder der)

inorder :: Explorador (AB a) a
inorder Nil = []
inorder (Bin i x d) = foldAB (\x y z -> y ++ [x] ++ z) [] (Bin i x d) 

-- postorder con recursion explicita:
-- postorder Nil = []
-- postorder (Bin izq val der) = (postorder izq) ++ (postorder der) ++ [val]

-- toma un arbol binario de tipo a y devuelve una lista con elementos de tipo a
postorder :: Explorador (AB a) a 
postorder = foldAB (\x i d -> i ++ d ++ [x]) []


--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = foldRT (\x y  -> x : (concat y) )

-- hojasRT con recursion explicita
--hojasRT (Rose x []) = [x]
--hojasRT (Rose x ys) = concat(map hojasRT ys)

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


--listasQueSuman' 0 l = [[]]
--listasQueSuman' n l = concat[ map (filter' i n l) (listasQueSuman' i l) | i <-[0..(n-1)] ]
                           
--filter' i n l xs | ((toInteger (length xs)) == (l-1) ) = []
--filter' i n l xs | otherwise =  ((n-i) : xs)

listasQueSumanHasta n = foldNat f [[]] n
            where f = (\n recu -> [ ((n-sum(xs):xs)) | xs <- recu ] ++ recu)

suma l xs | sum(xs) == l = True
             | otherwise = False


listasQueSuman' n = filter (suma n) (listasQueSumanHasta n)

iguales l xs | (toInteger (length xs))== l = True
             | otherwise = False

--listita :: Explorador Integer [Integer]
listita l = [ xs | n<-[l..], xs <- (filter (iguales l) (listasQueSuman' n))]

--Ejercicio 11 (implementar al menos una de las dos)
listasDeLongitud :: Explorador Integer [Integer]
--listasDeLongitud = undefined
listasDeLongitud = (\l -> (listita l))

(<*>) :: Explorador a a -> Explorador a [a] 
(<*>) = undefined

