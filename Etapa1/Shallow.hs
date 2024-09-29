{-# LANGUAGE TupleSections #-}
module Shallow where

import Data.List hiding (union)
import qualified Data.Set as S
import Debug.Trace

{-
    Punct bidimensional, reprezentat ca pereche de coordonate reale (x, y).
    type introduce un sinonim de tip, similar cu typedef din C.
    ex: (2.5, 3.0)
-}
type Point = (Float, Float)

{-
    Tip de funcție care primește un punct, și este parametrizat în raport
    cu tipul rezultatului.

    ex: o fct care intoarce coordonata x a unui punct
    getX :: Pointed Float
    getX (x, _) = x
-}
type Pointed a = Point -> a -- mod de a defini functii care iau un punct si intorc ceva (ceva ul acela poate avea orice tip)

{-
    Regiune bidimensională, reprezentată ca o funcție caracteristică
    (Point -> Bool). Pentru un punct care aparține regiunii, se întoarce True;
    altfel, False.

    Obs: cand vad Region ca argument la o functie ma gandesc ca de fapt primesc
         un punct Point si in fct de anumite aspecte intorc True sau False
         pentru punctul respectiv            
-}
type Region = Pointed Bool

{-
    Transformare bidimensională, reprezentată ca o funcție peste puncte.
    
    Primeste un punct si intoarce alt punct, adica modifica pozitia punctului in plan
-}
type Transformation = Point -> Point

{-
    *** TODO ***

    Implementați funcția inside, care verifică dacă un punct aparține unei
    regiuni (ea însăși reprezentată ca o funcție caracteristică).

    Constrângeri: funcția trebuie implementată point-free.

    Hint: implementați mai întâi funcția cu explicitarea parametrului formal
    (point-wise), și de-abia apoi transformați-o în stil point-free.

    Exemple:

    > inside (0, 0) (== (0, 0))
    True

    > inside (1, 1) (== (0, 0))
    False

    explicatie:
    -- fct inside primeste un punct (Point) si o regiune (Region) si
       vf daca punctul respectiv apartine regiunii intorcand T sau F (Bool)
    -- Regiunea este reprezentată printr-o funcție care primește un punct și
       întoarce True sau False în funcție de faptul dacă punctul respectiv
       aparține sau nu regiunii.
    
    varianta point-wise:
    inside :: Point -> Region -> Bool
    inside punct regiune = regiune punct

    rezolvare:
    -- Inițial, inside primește argumentele Point și Region, dar pentru a aplica
       funcția Region la punctul dat, avem nevoie de Region înaintea punctului.
       = > le inversez ordinea folosind flip.
    -- ($) este un opeartor de aplicare de functie - permite evaluarea functiei
       din stanga lui folosind argumentul din dreapta lui => aplica functia
       Region pe punctul Point
-}
inside :: Point -> Region -> Bool
inside = flip ($)

{-
    *** TODO ***

    Implementați funcția fromPoints, care construiește o regiune pe baza unei
    liste de puncte.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > fromPoints [(0, 0), (1, 1)] (0, 0)
    True

    > inside (0, 0) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    True

    > fromPoints [(0, 0), (1, 1)] (0, 1)
    False

    > inside (0, 1) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    False

    explicatie:
    -- [Point]: este o lista de puncte: [(0, 0), (1, 1)]
    -- Region: Point -> Bool

    rezolvare:
    -- Functia primeste ca argumente o lista de puncte si un punct separat
       urmand sa intoarca True sau False in functie daca punctul se afla in
       lista respectiva sau nu.
    -- functia elem primeste un element si o lista si verifica daca elementul
       respectiv se afla in lista intorcand un Bool, insa eu am mai intai lista
       de puncte si apoi punctul => dau flip si apoi aplic asupra lor functia `elem`
-}
fromPoints :: [Point] -> Region
fromPoints = flip elem

{-
    *** TODO ***

    Implementați funcția rectangle, care generează o regiune aferentă
    unui dreptunghi, cu lățime și înălțime date, simetric față de originea
    (0, 0). De exemplu, un dreptunghi cu lățimea 2 și înălțimea 2 va avea
    punctul din stânga-sus (-1, 1), iar din dreapta-jos, (1, -1).

    Exemple:

    > rectangle 2 2 (0, 0)
    True

    > rectangle 2 2 (-1, 1)
    True

    > rectangle 2 2 (1, -1)
    True

    > rectangle 2 2 (2, 2)  
    False
-}
rectangle :: Float -> Float -> Region
rectangle width height = \(x, y) -> -- (x, y) reprezinta coordonatele punctului pe care il primeste Region
    let halfWidth = width / 2
        halfHeight = height / 2
    in -halfWidth <= fst (x, y) && fst (x, y) <= halfWidth && -- vf daca x apartine [-halfWidth, halfWidth] 
       -halfHeight <= snd (x, y) && snd (x, y) <= halfHeight -- vf daca y apartine [-halfHeight, halfHeight]

{-
    *** TODO ***

    Implementați funcția circle, care generează o regiune aferentă unui cerc,
    cu rază dată și centrul în originea (0, 0).

    Exemple:

    > circle 1 (0, 0)
    True

    > circle 1 (1, 0)
    True
    
    > circle 1 (0, 1)
    True
    
    > circle 1 (1, 1)
    False

    explicatie:
    -- argumentul Float reprezinta raza cercului
    -- argumentul Region reprezinta un Punct, iar daca acel punct se afla
       in cercul de raza Float se va intoarce True, altfel False
    
    -- (x, y) reprezinta punctul pe care il primeste Region
    -- conditie: x^2 + y^2 <= radius^2
-}
circle :: Float -> Region
circle radius = \(x, y) -> fst (x, y)^2 + snd (x, y)^2 <= radius^2


{-
    *** TODO ***

    Implementați funcția plot, care generează diagrama unei regiuni,
    pe o suprafață de desenare de dimensiuni fixate. Punctul (0, 0)
    se află în centrul suprafeței de desenare, iar lățimea și înălțimea
    unui cadran (dintre cele 4) sunt primite ca parametri. De exemplu, dacă
    lățimea este 2 și înălțimea este 1, punctul din stânga-sus al suprafeței
    este (-2, 1), iar cel din dreapta-jos, (2, -1). Pentru fiecare punct
    cu coordonate întregi de pe suprafața de desenare, se introduce caracterul
    '*', dacă punctul aparține regiunii de desenat, sau '.', altfel. Funcția
    se utilizează în conjuncție cu funcția printPlot, definită mai jos
    în schelet, pentru o mai bună vizualizare.

    Constrângeri: funcția trebuie implementată cu list comprehensions,
    fără recursivitate explicită.

    Hints:
    * fromIntegral pentru conversia de la Int la Float.
    * intercalate pentru alipirea mai multor liste folosind un element
      de legătură.

    Exemple:

    > printPlot 2 1 $ fromPoints [(0, 0), (1, 1)]
    ...*.
    ..*..
    .....

    > printPlot 2 2 $ rectangle 2 2
    .....
    .***.
    .***.
    .***.
    .....

    Deși dimensiunile dreptunghiului sunt 2 și 2, apariția a câte 3 caractere
    '*' pe orizontală și pe verticală poate fi înțeleasă dacă vă gândiți
    la coordonatele vizate, -1, 0 și 1, în toate combinațiile (x, y).

    > printPlot 2 2 $ circle 2     
    ..*..
    .***.
    *****
    .***.
    ..*..

    rezolvare:
    -- folosesc list comprehensions pt a genera o lista de linii, unde fiecare linie
       este o lista de caractere
    
    intercalate "\n" [line y | y <- reverse [-height .. height]]
    -- genereaza 2 * height + 1 linii
    -- reverse [-height .. height] pt a egenra liniile din diagrama de la cea
       mai de sus pana la cea mai de jos
    
    line y = [point (fromIntegral x) (fromIntegral y) | x <- [-width .. width]]
    -- creeaza lista de caractere pentru linia y
    -- pentru fiecare x din intervalul [-width .. width] si coordonata y aplic functia
       point care primeste doua coordonate (x, y) si daca punctul format din aceste 2 
       coordonate se afla in regiunea data ca argument la functie => pun *, altfel .
    
    point x y = if region (x, y) then '*' else '.'
    -- daca punctul de coordonate x si y se afla in region (regiunea data ca param la fct)
       afisez '*', altfel afisez '.'
-}
plot :: Int -> Int -> Region -> String
plot width height region = intercalate "\n" [line y | y <- reverse [-height .. height]] -- genereaza liniile
    where line y = [point (fromIntegral x) (fromIntegral y) | x <- [-width .. width]] -- creeaza lista de caractere pt linia curenta
          point x y = if region (x, y) then '*' else '.'


{-
    Utilizați această funcție pentru vizualizarea diagramelor,
    după ce implementați funcția plot.
-}
printPlot :: Int -> Int -> Region -> IO ()
printPlot width height region = putStrLn $ plot width height region

{-
    *** TODO ***

    Implementați funcțiile promoteUnary și promoteBinary, care primesc
    o funcție unară (a -> b), respectiv binară (a -> b -> c), și o promovează
    pentru a opera pe rezultatul(-ele) unor funcții (Point -> a) etc.

    Constrângeri: funcția promoteUnary trebuie implementată point-free.

    Hint: dacă expandăm referirile la Pointed din tipul funcției promoteUnary,
    obținem (a -> b) -> (Point -> a) -> (Point -> b). Practic, trebuie
    construită o funcție cu tipul (Point -> b), pornind de la o funcție cu tipul
    (Point -> a) și aplicând apoi funcția cu tipul (a -> b) pe rezultatul ei.
    Extindeți apoi ideea pentru promoteBinary.

    Exemple:

    > promoteUnary (+ 1) (\(x, _) -> x) (3, 2)
    4.0

    > promoteBinary (+) (\(x, _) -> x) (\(_, y) -> y) (3, 2)
    5.0

    explicatie:
    -- fct unara: primeste un arg si pe baza lui intoarce un rez
    -- fct binara: primeste 2 arg si pe baza lor intoarce un rez
-}
promoteUnary :: (a -> b) -> Pointed a -> Pointed b
promoteUnary = (.)
--promoteUnary f pointed = \x -> f (pointed x) -- point wise
{-
    (a -> b) - functia unara
    Pointed a - singurul arg al fct unare
    Pointed b - rezultratul intors

    (.) - operator de compunere a functiilor in Haskel: (f . g) x = f(g(x))
    semnatura (.) in haskell: (b -> c) -> (a -> b) -> a -> c
    expl: primeste 2 functii(b -> c si a -> b) si intoarce o noua fct(a -> c)

    promoteUnary este definit ca o compunere de functii: primeste o functie unara
    f:a->b si o functie unara Pointed a si le compune pentru a obtine o noua
    functie unara Pointed b. Aceastra functie, Pointed b este rezultatul aplicarii
    functiei f pe rezultatul obtinut atunci cand se aplica functia Pointed a
-}

promoteBinary :: (a -> b -> c) -> Pointed a -> Pointed b -> Pointed c
promoteBinary f pointed1 pointed2 = \point -> f (pointed1 point) (pointed2 point)
-- promoteBinary f pointed1 pointed2 point
{-
    (a -> b -> c) - functie binara - are 2 argumente
    Pointed a - functie unara
    Pointed b - functie unara
    Pointed c = rezultat = functie unara

    se defineste o fct lambda care primeste un punct point. Se aplica functiile
    unare pointed1 si pointed2 asupra lui point pentru a obtine cele 2 argumente
    ale functie f
-}

{-
    *** TODO ***

    Implementați funcțiile complement, union și intersection, care determină
    complementul, reuniunea, respectiv intersecția a două regiuni.

    Constrângeri: funcțiile trebuie implementate point-free, utilizând
    promoteUnary sau promoteBinary, după caz.

    Exemple:

    > printPlot 2 2 $ complement $ circle 2
    **.**
    *...*
    .....
    *...*
    **.**

    > printPlot 2 2 $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    *....
    ..*..
    .***.
    ..*..
    ....*

    > printPlot 2 2 $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    .....
    .....
    ..*..
    .....
    .....
-}
complement :: Region -> Region
complement = promoteUnary (not)
{-
   functia complement primeste o regiune 'Region', care este o fct car vf daca un
   punct apartine regiunii respective sau nu. Aplicand functia 'not' pe rezultatele
   functiei Region, se inverseaza aceste rezultate: pct care apartineau initial
   regiunii devin puncte care nu mai apartin si invers

   point-wise:
   complement region = \point -> not (region point)
-}

union :: Region -> Region -> Region
union = promoteBinary (||)
{-
   functia union primeste 2 regiuni Region si introarce o alta regiune Region,
   respectand urmatoarea conditie: pt un anumit punct p, 'regiune1 p' si 'regiune2 p'
   sunt valorile rezultate (2 valori de tip Bool) => pe aceste 2 rezultate se aplica
   functia (||) ... daca cel putin unul dintre rezultate este True atunci punctul p
   va apartine regiunii rezultate

   point-wise:
   union regiune1 regiune2 = \point -> regiune1 point || regiune2 point
-}

intersection :: Region -> Region -> Region
intersection = promoteBinary (&&)
{-
    point-wise:
    intersection reg1 reg2 = \point -> reg1 point && reg2 point
-}

{-
    *** TODO ***

    Implementați funcția translation, care generează o translație
    cu deplasamente primite ca parametri. Deși contraintuitiv, deplasamentele
    trebuie scăzute, nu adunate, din coordonatele punctului transformat.
    De exemplu, dacă punctul (0, 0) aparține unei regiuni de interes, atunci
    punctul (1, 2) va trebui să aparțină regiunii în urma translației
    cu deplasamentele 1 și 2. Din moment ce funcția caracteristică a regiunii
    întoarce True pentru (0, 0), nu pentru (1, 2), cele două deplasamente
    trebuie scăzute.

    Exemple:

    > translation 1 2 (1, 2)
    (0.0,0.0)
-}
translation :: Float -> Float -> Transformation
translation tx ty = \(x, y) -> (x - tx, y - ty)

{-
    *** TODO ***

    Implementați funcția scaling, care generează o scalare cu un factor primit
    ca parametru. Similar cu observația de la funcția translate, factorul
    contribuie prin împărțire, nu prin înmulțire.

    Exemple:

    > scaling 2 (2, 2)
    (1.0,1.0)
-}
scaling :: Float -> Transformation
scaling factor = \(x, y) -> (x / factor, y / factor)

{-
    *** TODO ***

    Implementați funcția applyTransformation, care aplică o transformare asupra
    unei regiuni.

    Constrângeri: funcția trebuie implementată point-free.

    Exemple:

    > printPlot 2 2 $ applyTransformation (translation 1 0) (circle 2)
    ...*.
    ..***
    .****
    ..***
    ...*.

    > printPlot 2 2 $ applyTransformation (scaling 0.5) (circle 2)    
    .....
    ..*..
    .***.
    ..*..
    .....

    explicatie:
    functia primeste o transformare si o regiune si aplica transformarea
    respectiva asupra regiunii intorcand o noua regiune
-}
applyTransformation :: Transformation -> Region -> Region
applyTransformation = flip (.)
{-
    in mod normal, cand folosesc compunerea de functii (.), prima functie
    specificata este aplicata ultima, iar a doua functie este aplicata prima

    in cazul de fata, mai intai vrem sa aplicam mai intai transformarea, iar 
    abia apoi regiunea sa fie aplicata rezultatului transformarii => flip

    point-wise:
    applyTransformation transformation region = region . transformation
-}

{-
    *** TODO ***

    Implementați funcția combineTransformations, care combină transformările
    dintr-o listă într-o singură transformare. Ordinea de aplicare
    a transformărilor este dată de ordinea lor în listă.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > printPlot 2 2 $ applyTransformation
        (combineTransformations [translation 1 0, scaling 0.5]) (circle 2)
    .....
    ...*.
    ..***
    ...*.
    .....

    Echivalent cu:

    > printPlot 2 2 $ applyTransformation (translation 1 0) $
        applyTransformation (scaling 0.5) (circle 2)
-}
combineTransformations :: [Transformation] -> Transformation
combineTransformations = foldr (.) id . reverse
{-
    - primul pas este inversarea listei de transf => [scaling 0.5, translation 1 0]
    - rezultatul acestei inversari este transmis mai departe functiei foldr (.) id
    - fct foldr (.) id combina fct din lista inversata intr o singura fct,
      compunandu le de la dreapta la stanga

    daca avem de exemplu lista: [translation 1 0, scaling 0.5]
    - prima data aplic transformarea scaling 0.5 asupra functiei id
    - dupa aplic translation 1 0 asupra rezultatului de mai devreme

    
-}

{-
    *** TODO ***

    Funcția circles de mai jos generează o regiune formată din n cercuri de rază
    2, translatate succesiv cu 6 unități pe orizontală.

    Explicați la prezentare utilitatea evaluării leneșe pentru determinarea
    eficientă a apartenenței unui punct la regiunea construită prin reuniune.

    Hint: utilizați trace (vezi laboratorul 7) în funcția circle pentru afișarea
    punctului primit ca parametru și evaluați expresiile de mai jos:
    > inside (0, 0) $ circles 3
    > inside (6, 0) $ circles 3
    > inside (12, 0) $ circles 3
    > inside (18, 0) $ circles 3

    Exemple:

    > printPlot 15 3 $ circles 3
    ...............................
    ...............*.....*.....*...
    ..............***...***...***..
    .............*****.*****.*****.
    ..............***...***...***..
    ...............*.....*.....*...
    ...............................

    Răspuns: ...............
    Datorită evaluării leneșe, inside va evalua doar componentele necesare pentru
    a determina dacă punctul aparține regiunii => se construiesc cercuri ori pana
    la n, ori pana se gaseste punctul (0, 0)
-}
circles :: Int -> Region
circles n
    | n <= 0    = const False
    | otherwise = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             (circles (n - 1)))

{-
    *** TODO ***

    Explicați la prezentare cum se comportă reuniunea infinită de mai jos
    când se verifică apartenența unui punct care NU aparține regiunii.

    Răspuns: ...............
    se ruleaza la infinit
-}
infiniteCircles :: Region
infiniteCircles = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             infiniteCircles)

{-
    *** TODO BONUS ***

    Implementați funcția bfs, care realizează o căutare în lățime într-un spațiu
    oarecare de stări de tipul a, pornind de la o stare inițială start și de la
    o funcție expand, care determină pentru o stare curentă lista stărilor vecin.
    Funcția întoarce o listă, eventual infinită, de perechi de forma
    (stare, distanță), unde stările sunt ordonate conform parcurgerii în lățime,
    iar distanța reprezintă numărul de expandări realizate pentru a obține
    starea respectivă.

    Atenție! Pot exista multiple căi către aceeași stare, astfel fiind necesară
    reținerea stărilor deja vizitate utilizând o mulțime (Set, vezi modulul
    Data.Set). Observați la începutul acestui fișier linia "import qualified
    Data.Set as S". Acest lucru înseamnă că toate tipurile și funcțiile din acel
    modul trebuie prefixate cu "S."; de exemplu: S.Set, S.insert etc.

    Hint: dacă ar exista o cale unică de la starea inițială la orice altă stare,
    nefiind necesară reținerea stărilor deja vizitate, și nici nu s-ar solicita
    calculul distanțelor, funcția ar putea fi implementată prin:

    bfs :: a -> (a -> [a]) -> [a]
    bfs start expand = result
      where
        result = start : concat (map expand result)

    map operează independent pe stări și este insuficient de expresiv pentru
    a permite purtarea mulțimii de stări vizitate de la o stare la alta. Pentru
    acest lucru, ar fi necesar foldl. Funcționala predefinită mapAccumL
    realizează exact această combinație, map + foldl. O puteți utiliza pentru
    a extinde implementarea de mai sus.
-}
bfs :: (Ord a) => a -> (a -> [a]) -> [(a, Int)]
-- helper este o fct ajutatoare care primeste o lista si o
-- multime vida (pt a tine evidenta starilor vizitate)
bfs start expand = helper [(start, 0)] S.empty
  where
    helper [] _ = [] -- cazul de baza: daca coada e goala returnez o lista vida
    helper ((state, dist) : queue) visited -- scot primul element din coada
      | state `S.member` visited = helper queue visited -- daca state a fost deja vizitat, continui recursiv cu restul cozii
      | otherwise = (state, dist) : helper (queue ++ map (\s -> (s, dist + 1)) neighbors) (S.insert state visited)
      -- daca state nu a fost vizitat adaug perecha (state, dist) al rezultatul final si continui recursiv:
      -- - extind coada cu vecinii starii curente, fiecare avand distanta + 1
      -- - adaug starea state in visited pt a stii ca a fost vizitata
      where
        neighbors = expand state -- neighbors este lista starilor vecine obtinuta prin aplicarea fct expand pe state

{-
    functia bfs ia 2 param: start, care este starea initiala si expand care este
    o functie care primeste o stare si returneaza lista starilor vecine

    functia helper este utilizata pt a parcurge spatiul de stari si pt a construi
    lista rezultata de perechi (stare, distanta). Funcția helper este apelată cu:
     - O listă inițială care conține perechea (start, 0) (starea inițială și distanța 0).
     - O mulțime vidă S.empty pentru a ține evidența stărilor vizitate.

    functia helper ia 2 param: queue: o listă de perechi (stare, distanță) reprezentând
    stările de explorat și distanțele lor si visited: o mulțime de stări vizitate pentru
    a evita re-explorarea aceluiași nod.
-}

{-
    *** TODO BONUS ***

    Implementați funcția regionAvoidingBfs, care determină distanța minimă
    de la orice punct la un nod de start, obținută prin deplasări către nord,
    est, sud sau vest, și ocolind regiunea primită ca parametru.

    Constrângeri: utilizați funcția bfs.

    Exemple:

    > lookup (3, 0) $ regionAvoidingBfs (-3, 0) $ circles 3
    Just 12

    Explicație: distanța de la punctul (-3, 0) la punctul (3, 0) este 12,
    și este descrisă mai jos, prin distanțele către punctele intermediare.
    Au fost folosite și cifre hexazecimale, pentru încadrarea într-un singur
    caracter. Distanța 0 corespunde punctului (-3, 0), iar distanța C (12),
    punctului (3, 0).

    ...................567...................
    ..................34*89...*.....*........
    .................12***AB.***...***.......
    .................0*****C*****.*****......
    ...................***...***...***.......
    ....................*.....*.....*........
    .........................................
-}
regionAvoidingBfs :: Point -> Region -> [(Point, Int)]
regionAvoidingBfs start region = bfs start expand
  where
    expand (x, y) = filter (\p -> not (inside p region)) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)] -- E V N S

-- folosesc bfs pt ca vreau sa generez lista cu perechi si distante, iar
-- lookup alege distanta pentru perechea (3, 0) exemplu

-- ma duc in toate directiile E V N S si calculez distanta la punctele
-- care nu sunt in regiune => d asta folosesc not inside

