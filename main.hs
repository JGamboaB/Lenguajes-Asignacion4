import Data.Set
import Data.List ()
import Distribution.Simple.Program.HcPkg (list)

data Proposition = Const Bool
    | Variable String
    | Negacion Proposition
    | Conjuncion (Proposition, Proposition)
    | Disyuncion (Proposition, Proposition)
    | Implicacion (Proposition, Proposition)
    | Equivalencia (Proposition, Proposition)
    deriving Show


-- / / Proposiciones Prueba
prop1, prop2, prop3, prop4 :: Proposition

prop1 = Disyuncion (Conjuncion (Variable "p", Variable "q"), Variable "r")
prop2 = Disyuncion(Implicacion(Variable "A1", Conjuncion(Variable "B", Negacion(Variable "3"))),Disyuncion(Conjuncion(Variable "D", Implicacion(Variable"Q", Negacion(Negacion(Variable "P")))), Negacion(Variable "B")));
prop3 = Implicacion (Conjuncion (Variable "p", Variable "q"), Variable "p") -- TAUTOLOGIA: data:image/png;base64,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
prop4 = Equivalencia (Equivalencia(Variable "a", Variable "b"), Conjuncion(Implicacion(Variable "a", Variable "b"), Implicacion(Variable "b", Variable "a"))) -- TAUTOLOGIA: https://www.ecured.cu/images/thumb/a/aa/Tautologia.png/260px-Tautologia.png

-- / /



-- / / / vars: determina la lista de las distintas variables proposicionales / / / 

uniq :: Ord a => [a] -> [a] -- ---> Delete duplicates, make unique <--- 
uniq = toList . fromList -- Basada en: https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem

varsAux :: Proposition -> [String]
varsAux n = case n of
    (Const _) -> []
    (Variable v) -> [v]
    (Negacion p) -> varsAux p
    (Conjuncion (p1, p2)) -> varsAux p1 ++ varsAux p2
    (Disyuncion (p1, p2)) -> varsAux p1 ++ varsAux p2
    (Implicacion (p1, p2)) -> varsAux p1 ++ varsAux p2
    (Equivalencia (p1, p2)) -> varsAux p1 ++ varsAux p2

vars :: Proposition -> [String]
vars n = uniq (varsAux n)

-- Test: a = vars prop1 



-- / / / gen_bools: produce todas las posibles combinaciones de valores booleanos para n variables proposicionales (uses vars as parameter) / / / 
-- Basada en https://stackoverflow.com/questions/35120766/generating-a-truth-table

gen_bools :: Traversable t => t b -> [t Bool]
gen_bools v = mapM (const [True, False]) v

-- Test: b = gen_bools a



-- / / / as_vals: dada una lista de variables proposicionales sin repeticiones, la combina con una lista de valores booleanos (uses vars and one list of gen_bools) -> zip / / / 

as_vals :: [String] -> [Bool] -> [(String, Bool)]
as_vals = zip

--  Test: as_vals a (b !! 0)



-- / / / evalProp: evalúa una proposición dada una asignación de valores (uses as_vals) / / / 

-- / / Search function

-- Gets the value assigned to the variable
searchValAux :: Eq t => t -> [(t, p)] -> Int -> p
searchValAux varName list index = do
    let val = list !! index
    if fst val == varName
        then snd val
        else searchValAux varName list (index-1)

-- Calls the aux
searchVal :: Eq t => t -> [(t, p)] -> p
searchVal var list = searchValAux var list (length list -1)


-- / / Operators needed

-- Implication
(==>) :: Bool -> Bool -> Bool
(==>) a b = not a || b

-- Equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) a b = a ==> b && b ==> a


evalProp :: Proposition -> [(String, Bool)] -> Bool
evalProp n val_list = case n of
    (Const valor) -> valor
    (Variable var) -> searchVal var val_list
    (Negacion p) -> not (evalProp p val_list)
    (Conjuncion (p1, p2)) -> evalProp p1 val_list && evalProp p2 val_list
    (Disyuncion (p1, p2)) -> evalProp p1 val_list || evalProp p2 val_list
    (Implicacion (p1, p2)) -> evalProp p1 val_list ==> evalProp p2 val_list
    (Equivalencia (p1, p2)) -> evalProp p1 val_list <=> evalProp p2 val_list



-- / / / taut (if evalProp == True para todos los valores posibles) / / / 

tautAux :: Proposition -> [[Bool]] -> [String] -> Int -> IO ()
tautAux n full_list varNames index = do
    let list = full_list !! index
    let val_list = as_vals varNames list
    if index == -1 -- Every single one has been True -> "Si es una tautologia"
        then putStrLn "Si es una tautologia"
        else if not(evalProp n val_list) -- False, must give answer on where it failed -> Ej: "No es una tautologia debido a que se falsifica con A: False, B: True"
            then putStrLn ("No es una tautologia debido a que se falsifica con " ++ show val_list)
            else tautAux n full_list varNames (index - 1) -- Recursiva


taut :: Proposition -> IO ()
taut n = do
    print n
    let varNames = vars n
    let full_list = gen_bools varNames
    tautAux n full_list varNames (length full_list -1)

-- Test: taut prop4



-- / / / fnd: Forma Normal Disyuntiva (Consigo los miniterminos de cuando es true y se suman -> Escribirlo de forma Proposicion) / / / 
-- OR of ANDs, a sum of products (* : and / + : or)

-- / / Minterms -> Consigue matriz de minterms (cada variable dentro de cada lista es una multiplicacion/conjuncion/and y las listas en si son una suma/disyuncion/or)
getMinterms :: Proposition -> [[Bool]] -> [String] -> Int -> [[(String, Bool)]]
getMinterms n full_list varNames index = do
    let list = full_list !! index
    let val_list = as_vals varNames list
    if index == -1 -- End
        then []
        else if evalProp n val_list -- True, get val_list
            then val_list : getMinterms n full_list varNames (index - 1)
            else getMinterms n full_list varNames (index - 1)


-- / Transforma los valores de una lista de 1 minterm en sus respectivas operaciones (Conjuncion / Negacion)
toConjAux :: [(String, Bool)] -> Int -> Proposition
toConjAux list index = do
    let tup0 = head list
    let v0 = Variable (fst tup0)
    let tup1 = list !! 1
    let v1 = Variable (fst tup1)
    let tupX = list !! index
    let vX = Variable (fst tupX)

    if index >= 2 
        then if snd tupX
            then Conjuncion(toConjAux list (index-1), vX)
            else Conjuncion(toConjAux list (index-1), Negacion vX)
        else if snd tup0 && snd tup1
            then Conjuncion(v0, v1)
            else if not(snd tup0) && not (snd tup1)
                then Conjuncion(Negacion v0, Negacion v1)
                else if not(snd tup0)
                    then Conjuncion(Negacion v0, v1)
                    else Conjuncion(v0, Negacion v1)
         

toConj :: [(String, Bool)] -> Proposition
toConj list = do toConjAux list (length list -1)


-- / Transforma los valores de una lista de todos los minterms en sus respectivas operaciones (Disyuncion / Conjuncion / Negacion)
toDisAux :: [[(String, Bool)]] -> Int -> Proposition
toDisAux list index = do
    let p0 = toConj (head list)
    let p1 = toConj (list !! 1)
    let pX = toConj (list !! index)

    if index >= 2
        then Disyuncion(toDisAux list (index-1), pX)
        else Disyuncion(p0, p1)

toDis :: [[(String, Bool)]] -> Proposition
toDis list = do toDisAux list (length list -1)


-- / Forma Normal Disyuntiva 
fnd :: Proposition -> Proposition
fnd n = do
    let varNames = vars n
    let full_list = gen_bools varNames
    let min = getMinterms n full_list varNames (length full_list -1)
    toDis min


{- Comprobacion fdn

-- prop1
minterms: [[("p",False),("q",False),("r",True)],[("p",False),("q",True),("r",True)],[("p",True),("q",False),("r",True)],[("p",True),("q",True),("r",False)],[("p",True),("q",True),("r",True)]]
-> p'q'r + p'qr + pq'r + pqr' + pqr

res: Disyuncion (Disyuncion (Disyuncion (Disyuncion (Conjuncion (Conjuncion (Negacion (Variable "p"),Negacion (Variable "q")),Variable "r"),Conjuncion (Conjuncion (Negacion (Variable "p"),Variable "q"),Variable "r")),Conjuncion (Conjuncion (Variable "p",Negacion (Variable "q")),Variable "r")),Conjuncion (Conjuncion (Variable "p",Variable "q"),Negacion (Variable "r"))),Conjuncion (Conjuncion (Variable "p",Variable "q"),Variable "r"))

-- prop4

minterms: [[("a",False),("b",False)],[("a",False),("b",True)],[("a",True),("b",False)],[("a",True),("b",True)]]
-> a'b' + a'b + ab' + ab

res: Disyuncion (Disyuncion (Disyuncion (Conjuncion (Negacion (Variable "a"),Negacion (Variable "b")),Conjuncion (Negacion (Variable "a"),Variable "b")),Conjuncion (Variable "a",Negacion (Variable "b"))),Conjuncion (Variable "a",Variable "b"))

Disyuncion (
    Disyuncion (
        Disyuncion (
            Conjuncion (
                Negacion (Variable "a"),
                Negacion (Variable "b")
            ),Conjuncion (
                Negacion (Variable "a"),
                Variable "b")
        ),Conjuncion (
            Variable "a",
            Negacion (Variable "b")
        )
    ),Conjuncion (
        Variable "a",
        Variable "b"
    )
)

-}



-- / / / bonita (print bonito?) / / / 
