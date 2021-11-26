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

A1 => B /\ ~3 \/ D /\ Q => ~~P \/ ~B
A1 => B /\ ~3 \/ D /\ Q => ~~P \/ ~B

Disyuncion (Implicacion (Variable "A1",Conjuncion (Variable "B",Negacion (Variable "3"))),Disyuncion (Conjuncion (Variable "D",Implicacion (Variable "Q",Negacion (Negacion (Variable "P")))),Negacion (Variable "B")))

Disyuncion (
    Implicacion (
        Variable "A1",
        Conjuncion (
            Variable "B",
            Negacion (Variable "3")
        )
    ), Disyuncion (
        Conjuncion (
            Variable "D",
            Implicacion (
                Variable "Q",
                Negacion (Negacion (Variable "P"))
            )
     ),Negacion (Variable "B")
    )
)



Parentesis:
    - Si es una negacion de algo que no es una variable
    - Si uno de los valores no es una variable, se pone parentesis
-}