import Data.Set
import Data.List ()
import Data.Typeable

data Proposicion = Const Bool
    | Variable String
    | Negacion Proposicion
    | Conjuncion (Proposicion, Proposicion)
    | Disyuncion (Proposicion, Proposicion)
    | Implicacion (Proposicion, Proposicion)
    | Equivalencia (Proposicion, Proposicion)
    deriving Show


-- / / / vars: determina la lista de las distintas variables proposicionales / / / 

uniq :: Ord a => [a] -> [a] -- ---> Delete duplicates, make unique <--- 
uniq = toList . fromList

varsAux :: Proposicion -> [String]
varsAux n = case n of
    (Const _) -> []
    (Variable v) -> [v]
    (Negacion p) -> varsAux p
    (Conjuncion (p1, p2)) -> varsAux p1 ++ varsAux p2
    (Disyuncion (p1, p2)) -> varsAux p1 ++ varsAux p2
    (Implicacion (p1, p2)) -> varsAux p1 ++ varsAux p2
    (Equivalencia (p1, p2)) -> varsAux p1 ++ varsAux p2

vars :: Proposicion -> [String]
vars n = uniq (varsAux n)



-- / / / gen_bools: produce todas las posibles combinaciones de valores booleanos para n variables proposicionales (uses vars as parameter) / / / 
gen_bools :: Traversable t => t b -> [t Bool]
gen_bools v = mapM (const [True, False]) v



-- / / / as_vals: dada una lista de variables proposicionales sin repeticiones, la combina con una lista de valores booleanos (uses vars and one list of gen_bools) -> zip / / / 
as_vals :: [String] -> [Bool] -> [(String, Bool)]
as_vals = zip



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


--evalProp :: Proposicion -> [(String, Bool)] -> Bool
evalProp n val_list = case n of
    (Const valor) -> valor
    (Variable var) -> searchVal var val_list
    (Negacion p) -> not (evalProp p val_list)
    (Conjuncion (p1, p2)) -> evalProp p1 val_list && evalProp p2 val_list
    (Disyuncion (p1, p2)) -> evalProp p1 val_list || evalProp p2 val_list
    (Implicacion (p1, p2)) -> evalProp p1 val_list ==> evalProp p2 val_list
    (Equivalencia (p1, p2)) -> evalProp p1 val_list <=> evalProp p2 val_list



-- / / / taut (if evalProp == True para todos los valores posibles) / / / 

tautAux :: Proposicion -> [[Bool]] -> [String] -> Int -> IO ()
tautAux n full_list varNames index = do
    let list = full_list !! index
    let val_list = as_vals varNames list
    if index == -1 -- Every single one has been True -> "Si es una tautologia"
        then putStrLn "Si es una tautologia"
        else if not(evalProp n val_list) -- False, must give answer on where it failed -> Ej: "No es una tautologia debido a que se falsifica con A: False, B: True"
            then putStrLn ("No es una tautologia debido a que se falsifica con " ++ show val_list)
            else tautAux n full_list varNames (index - 1) -- Recursiva


taut :: Proposicion -> IO ()
taut n = do
    print n
    let varNames = vars n
    let full_list = gen_bools varNames
    tautAux n full_list varNames (length full_list -1)



-- / / / fnd: Forma Normal Disyuntiva (Consigo los miniterminos de cuando es true y se suman -> Escribirlo de forma Proposicion) / / / 
-- OR of ANDs, a sum of products (* : and / + : or)

-- / / Minterms -> Consigue matriz de minterms (cada variable dentro de cada lista es una multiplicacion/conjuncion/and y las listas en si son una suma/disyuncion/or)
getMinterms :: Proposicion -> [[Bool]] -> [String] -> Int -> [[(String, Bool)]]
getMinterms n full_list varNames index = do
    let list = full_list !! index
    let val_list = as_vals varNames list
    if index == -1 -- End
        then []
        else if evalProp n val_list -- True, get val_list
            then val_list : getMinterms n full_list varNames (index - 1)
            else getMinterms n full_list varNames (index - 1)


-- / Transforma los valores de una lista de 1 minterm en sus respectivas operaciones (Conjuncion / Negacion)
toConjAux :: [(String, Bool)] -> Int -> Proposicion
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
         

toConj :: [(String, Bool)] -> Proposicion
toConj list = do toConjAux list (length list -1)


-- / Transforma los valores de una lista de todos los minterms en sus respectivas operaciones (Disyuncion / Conjuncion / Negacion)
toDisAux :: [[(String, Bool)]] -> Int -> Proposicion
toDisAux list index = do
    let p0 = toConj (head list)
    let p1 = toConj (list !! 1)
    let pX = toConj (list !! index)

    if index >= 2
        then Disyuncion(toDisAux list (index-1), pX)
        else Disyuncion(p0, p1)

toDis :: [[(String, Bool)]] -> Proposicion
toDis list = do toDisAux list (length list -1)


-- / / Forma Normal Disyuntiva 
fnd :: Proposicion -> Proposicion
fnd n = do
    let varNames = vars n
    let full_list = gen_bools varNames
    let min = getMinterms n full_list varNames (length full_list -1)
    toDis min



-- / / / bonitaAux (print) / / / 

-- Paréntesis al lado que asocian si la precedencia de ese lado es menor a la del caso actual

bonitaAux :: Proposicion -> [Char]
bonitaAux n = case n of
    (Const c) -> if c then "True" else "False"
    (Variable v) -> v

    (Negacion p) ->  case p of 
        (Variable _) -> "~" ++ bonitaAux p
        _ -> "~(" ++ bonitaAux p ++ ")"

    -- bonitaAux p1 ++ " /\\ " ++ bonitaAux p2
    (Conjuncion (p1, p2)) -> case p1 of 
        (Variable _) -> bonitaAux p1 ++ " /\\ " ++ bonitaAux p2
        (Negacion _) -> bonitaAux p1 ++ " /\\ " ++ bonitaAux p2
        (Conjuncion (_, _)) -> bonitaAux p1 ++ " /\\ " ++ bonitaAux p2
        _ -> "(" ++bonitaAux p1 ++ ") /\\ " ++ bonitaAux p2

    -- bonitaAux p1 ++ " \\/ "++ bonitaAux p2
    (Disyuncion (p1, p2)) -> case p1 of
        (Variable _) -> bonitaAux p1 ++ " \\/ "++ bonitaAux p2
        (Negacion _) -> bonitaAux p1 ++ " \\/ "++ bonitaAux p2
        (Conjuncion (_, _)) -> bonitaAux p1 ++ " \\/ "++ bonitaAux p2
        (Disyuncion (_, _)) -> bonitaAux p1 ++ " \\/ "++ bonitaAux p2
        _ -> "(" ++ bonitaAux p1 ++ ") \\/ " ++ bonitaAux p2

    -- bonitaAux p1 ++ " => "++ bonitaAux p2
    (Implicacion (p1, p2)) -> case p1 of -- >> Derecha << --
        (Equivalencia (_, _)) -> bonitaAux p1 ++ " => (" ++ bonitaAux p2 ++ ")"
        _ -> bonitaAux p1 ++ " => " ++ bonitaAux p2 
    
    -- bonitaAux p1 ++ " <=> "++ bonitaAux p2
    (Equivalencia (p1, p2)) -> bonitaAux p1 ++ " <=> "++ bonitaAux p2

bonita :: Proposicion -> IO ()
bonita n = do 
    print n
    putStrLn (bonitaAux n) 



-- / / / / / / / / / / / / / / / / / / / / / / Pruebas / / / / / / / / / / / / / / / / / / / / / /  --

-- / / Proposiciones Prueba
prop1, prop2, prop3, prop4, prop5 :: Proposicion

prop1 = Disyuncion (Conjuncion (Variable "p", Variable "q"), Variable "r")
prop2 = Equivalencia(Implicacion(Variable "a", Variable "b"), Disyuncion(Negacion (Variable "a"), Variable "b")) -- TAUTOLOGIA
prop3 = Disyuncion(Implicacion(Variable "A0", Conjuncion(Variable "B", Negacion(Variable "C1"))),Disyuncion(Conjuncion(Variable "D", Implicacion(Variable"T", Negacion(Negacion(Variable "R")))), Negacion(Variable "B")));

-- Extra
prop4 = Equivalencia (Equivalencia(Variable "a", Variable "b"), Conjuncion(Implicacion(Variable "a", Variable "b"), Implicacion(Variable "b", Variable "a"))) -- TAUTOLOGIA
prop5 = Implicacion (Conjuncion (Variable "p", Variable "q"), Variable "p") -- TAUTOLOGIA


-- vars
vars1 = vars prop1
vars2 = vars prop2
vars3 = vars prop3

-- gen_bools
gb1 = gen_bools vars1
gb2 = gen_bools vars2
gb3 = gen_bools vars3

-- as_vals
aV1 = as_vals vars1 (head gb1)
aV2 = as_vals vars2 (gb2 !! 1)
aV3 = as_vals vars3 (gb3 !! 2)


-- evalProp
eval1 = evalProp prop1 aV1
eval2 = evalProp prop2 aV2
eval3 = evalProp prop3 aV3

-- taut
taut1 = taut prop1
taut2 = taut prop2
taut3 = taut prop3

-- fnd
fnd1 = fnd prop1
fnd2 = fnd prop2
fnd3 = fnd prop3

-- bonita
b1 = bonita prop1
b2 = bonita prop2
b3 = bonita prop3
b4 = bonita (Conjuncion(Disyuncion(Variable "a", Variable "b"), Variable "c"))
