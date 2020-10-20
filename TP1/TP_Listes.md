# Haskell (INFO3)
## Listes

**Question 1: Expressions de Liste**
1. Une liste s’ecrit entre crochets, avec les éléements de la liste séparés par des virgules. Rappelez ce que font les operateurs ``[]``, ``:`` et ``++``.
* ``[]`` : Liste vide (aussi la fin de liste).
* ``:`` : Opérateur d'ajout d'un élément au début d'une liste.
* ``a ++ b`` : Concaténation de listes ; La première liste va être dupliquée et son dernier élément va pointer sur le premier élément de b.
2. Evaluez les expressions de liste suivantes :
```haskell
1:[2] -- [1, 2]
[3,4]++[1,2] -- [3, 4, 1, 2]
[3..10] -- [3, 4, 5, 6, 7, 8, 9, 10]
tail [1..4] ++ 5:[] -- [2, 3, 4, 5]
head [1..4] : [5] -- [1, 5]
reverse [1..4] ++ [5] -- [4, 3, 2, 1, 5]
1 : reverse [2..5] -- [1, 5, 4, 3, 2]
```
**Question 2: Définition de fonctions sur les listes**
1. Rappelez ce que font les fonctions suivantes définies dans ``prelude.hs`` :
* ``head`` : Retourne le premier élément de la liste.
* ``tail`` : Retourne la liste sans le premier élément.
* ``reverse`` : Inverse la liste.
* ``length`` : Retourne la longueur de la liste.
* ``drop`` : Prend en paramètre un entier N et une liste L et retourne L sans les N premiers éléments.
* ``take`` : Prend en paramètre un entier N et une liste L et retourne une liste avec les N premiers éléments de L.
* ``!!`` : Retourne l'élément de la liste avec l'indice indiqué.
* ``tails`` : Retourne une liste de listes contenant successivement les queues de chaque liste.
![alt text](Haskell_Caterpillar.PNG "Haskell Caterpillar")

2. La fonction ``last``, définie dans ``prelude.hs``, selectionne le dernier éléement d’une liste. Simulez dans ghci le comportement de last exclusivement a l’aide des fonctions :
```haskell
lst = [1, 2, 3, 4, 5]
```
* ``head`` et ``reverse``
```haskell
head (reverse lst)
```
* ``length`` et ``!!``
```haskell
lst !! (length lst - 1)
```
* ``head``, ``drop`` et ``length``.
```haskell
head (drop (length - 1) lst)
```
Donnez ensuite une version récursive de la fonction ``last``.
```haskell
-- To be written in a file `Last.hs`
last' :: [a] -> a
last' []			= error "empty list"
last' (_ : x : [])	= x
last' (_ : xs)		= last' xs

{- Teacher's reference
last' [] = error "*** Exception: last: empty list"
last' [x] = x
last' (_ : xs) = last' xs
-}
```

3. La fonction ``init``, definie dans ``prelude.hs``, supprime le dernier élément d’une liste. Simulez dans ghci le comportement de init exclusivement a l’aide des fonctions suivantes :
* ``take`` et ``length``
```haskell
take (length lst - 1) lst
```
* ``tail`` et ``reverse``
```haskell
reverse(tail (reverse lst))
```
* ``tails``, ``reverse`` et ``!!``
```haskell

```
Donner ensuite une version recursive de la fonction
```haskell
-- To be written in a file `Init.hs`
init' :: [a] -> [a]
init' []	= error "empty list"
init' [x] 	= []
init' (x : xs) = x : init' xs
```

**Question 3: Chaînes de caractères**
Un palindrome un mot dont l’ordre des lettres reste le meme qu’on le lise de gauche à droite
ou de droite à gauche, comme dans la phrase "Esope reste ici et se repose".

1. Comment tester si un mot (i.e., une chaîne de caractères sans caractère espace) est un
palindrome ? (Accents et majuscules ne sont pas utilisés ici.)
On divise la chaîne de caractères par deux (si longueur impaire, on ne tient pas compte du caractère du milieu) et on compare la première partie avec la deuxième partie en reversed.

2. Comment tester si une chaîne de caracteres est un palindrome ? (Accents et majuscules ne sont pas utilisés ici mais le mot peut contenir des caractères espace dont il ne faut pas tenir compte.)
On enlève les caractères espace et on refait la manip décrite ci-dessus.

3. Ecrire une fonction qui teste si un mot est un palindrome ? (Accents et majuscules ne sont pas utilisés ici.) Quel doit être le type de cette fonction ?
```haskell
isPalindrome :: [a] -> Bool
isPalindrome [] = error "empty string cannot be a palindrome"
isPalindrome xs = xs == reverse xs
```

4. Ecrire une fonction qui teste si une chaîne de caractères est un palindrome ? (Accents et majuscules ne sont pas utilisés ici mais la chaîne peut contenir des caractères espace
dont il ne faut pas tenir compte..) Quel doit être le type de cette fonction ?
```haskell
import Data.Char(isSpace)
import Data.List
trimmed = dropWhileEnd isSpace . dropWhile isSpace


```

**Question 4: Types**
1. Quel est le type des valeurs suivantes :
* [’a’,’b’,’c’] :: [Char]
* [1, 2, 3] :: Num a => a
* [[’a’,’b’],[’c’,’d’]] :: [[Char]]
* [[’1’,’2’],[’3’,’4’]] :: Num a => [[a]]
* (’a’,’b’) :: (Char, Char)
* (’a’,’b’,’c’) :: (Char, Char, Char)
* (1,2) :: Num a => (a, a)
* (1,2,3) :: Num a => (a, a, a)
* [(False,’0’),(True,’1’)] :: Num a => [(Bool, a)]
* ([False,True],[’0’,’1’]) :: Num a => ([Bool], [a])
* [tail,init,reverse] :: [[a] -> [a]]
* ([tail,init,reverse],[take,drop]) :: ([[a1] -> [a1]], [Int -> [a2] -> [a2]])

2. Expliquer la session suivante :
```haskell
>>>import Data.List
>>>:type(head, take)(head, take)::([a]->a,Int ->[a1]->[a1])
>>>:type[head, take]

<interactive>:1:8:
	Couldn’tmatchtype ’Int’with’[[a] ->[a]]’
	Expected type:[[a]->[a]]->[a]->[a]
	Actual type: Int ->[a]->[a]
	In the expression:take
	In the expression:[head, take]
Prelude Data.List>
```
L'interpréteur râle car head et take ont des types différents. Un tuple admet des éléments de type différents tandis que la liste a besoin nécessairement d'éléments de même type.

**Question 5: Fonctions**

* second xs=head (tail xs)
* appl (f,x)=f x
* pair x y=(x,y)
* mult x y=x\*y
* double=mult 2
* palindrome xs=reverse xs == xs
* twice f x=f (f x)
* incrAll xs=map (+1) xs
* norme xs=sqrt (sum (map f xs))wheref x=xˆ2.

1. Calculez les types de ces fonctions, en n’oubliant pas les contraintes de classe.
* second :: [a] -> a
* appl :: (t1, t2, t1) -> t2
* pair :: a1 -> a2 -> (a1, a2)
* mult :: Num a => a -> a -> a
* double :: Num a => a -> a
* palindrome :: [a] -> Bool
* twice :: (t -> t) -> t -> t
* incrAll :: Num a => [a] -> [a]
* norme :: Num a => [a] -> a

2. Donnez une forme curifiée de la fonction appl.
```haskell
appl' f x = f x
```

3. Quelles sont les *fonctions d'ordre supérieur* ?
Ce sont les fonctions qui prennent d'autres fonctions en paramètre : appl, twice

4. Quelles sont les *fonctions polymorphes* ?
Ce sont des fonctions dont les paramètres peuvent prendre plusieurs types.

**Question 6: Compréhensions de listes**

1. A l’aide d’une compréhension de liste, calculer la liste de tous entiers positifs impairs.
```haskell
take 10 [x | x <- [1..], odd x]
```
2. A l’aide d’une compréhension de liste, calculer la liste de carrés des entiers pairs (i.e.,les entiers $i^2$ pour i = 2,4,...).
```haskell
getSquareList xs = [x * x | x <- xs, mod x 2 == 0]

getSquareList' xs = [x * x | x <- xs, even x]
```
3. A l’aide d’une compréhension de liste, calculer la liste : ``[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]``
```haskell
[[1..n] | n <- [1..5]]
```
4. A l’aide d’une compréhension de liste, calculer la liste des paires d’entiers (n, m)... :
```haskell
[(n, m) | n <- [1..20], m <- [n..20], sum [x | x <- [1..n]] == m ]
```
5. ) En arithmétique, un nombre parfait est un entier naturel n tel que σ(n) = 2n, ou σ(n) est la somme des diviseurs positifs de n. Cela revient a dire qu’un entier naturel est
parfait s’il est egal à la moitié de la somme de ses diviseurs ou encore à la somme de ses diviseurs stricts. Ainsi 6 est un nombre parfait car 2 × 6 = 12 = 1 + 2 + 3 + 6, ou
encore 6 = 1 + 2 + 3. Les trois premiers nombres parfaits sont
* 6 = 1 + 2 + 3,
* 28 = 1 + 2 + 4 + 7 + 14, et
* 496 = 1 + 2 + 4 + 8 + 16 + 31 + 62 + 124 + 248.
A l’aide d’une compréhension de liste, calculer la liste des nombres parfaits (et, par
exemple, donner le quatrieme ; indice il s’agit de 8128). Existe-t-il un nombre parfait
impair ?
```haskell
[(n, m) | n <- [1000.1300]
		, m <- [n+1..1300]
		, sum [div | div <- [1..n], n `mod` div == 0] == n+m
		, sum [div | div <- [1..m], m `mod` div == 0] == n+m]
```
**Question 7: Fonctions simples**
1. Ecrire une fonction permettant de compter le nombre d’éléments dans une liste (sans utiliser length bien sur !).
```haskell
lenght' [] = 0
lenght' (_ : xs) = 1 + lenght' xs 
```

2. Ecrire une fonction permettant de renverser une liste (sans utiliser ´ reverse bien sur !)
```haskell
reverse' [] = []
reverse = F.foldl (flip (:)) []
```

3. Ecrire une fonction permettant de calculer le nombre de voyelles dans une chaîne de caracteres. (Nous ne préoccupons pas des accents). 
```haskell
vowelNb xs = length [x | x <- xs, x `elem` ['a', 'e', 'i', 'o', 'u']]
```

4. La fonction splitAt de type ``Int -> [a] -> ([a], [a])`` retourne un couple de
listes obtenu en cassant une liste a une position donnée.
```
>>> :type splitAt
splitAt :: Int -> [a] -> ([a], [a])
>>> splitAt 0 [1..10]
([],[1,2,3,4,5,6,7,8,9,10])
>>> splitAt 5 [1..10]
([1,2,3,4,5],[6,7,8,9,10])
>>> splitAt 10 [1..10]
([1,2,3,4,5,6,7,8,9,10],[])
>>> splitAt 3 []
([],[])
>>>
```
Proposez une implementation de splitAt.
```haskell
splitAt' n xs = (L.take n xs, L.drop n xs)
```
5. La suite de Fibonacci est une suite d’entiers dans laquelle chaque terme est la somme
des deux termes qui le prec´ edent. Elle commence généeralement par les termes 0 et 1
(parfois 1 et 1) et ses premiers termes sont : 0, 1, 1, 2, 3, 5, 8, 13, 21, . . . (suite A000045 de l’OEIS (On-Line Encyclopedia of Integer Sequences)). Ecrire une fonction ´ fibonacci de type ``fib :: (Num a1, Num a, Eq a) => a -> a1`` permettant de calculer
un terme de la suite de fibonacci.
```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' n = L.head . L.drop n $ aux
	where
		aux = 0 : 1 : next aux
		next (a : t@(b : _)) = (a+b) : next t
```