------------------------ Definição do tipo de dados ------------------------
-- Tipo de Dado Algébrico para representar expressões matemáticas
data Expressao = 
    Const Int
    | Var String
    | Soma Expressao Expressao
    | Produto Expressao Expressao
    | Potencia Expressao Int
    deriving(Show, Eq)

------------------------ Função de pré-processamento ------------------------
-- Adiciona espaços ao redor de parênteses para facilitar a tokenização
adicionaEspacos :: Char -> [Char]
adicionaEspacos c = if c `elem` "()"
                    then " " ++ [c] ++ " "
                    else [c]

------------------------ Função de tokenização ------------------------
-- Espera uma expressão pré-fixa como string e retorna uma lista dos símbolos

tokenize :: String -> [String]
tokenize s = words (concatMap adicionaEspacos s)

------------------------ Função de parsing ------------------------
-- Retorna a expressão e os tokens restantes
parse :: [[Char]] -> (Expressao, [[Char]])

parse ("(":tokens) =
    let 
        (op:tokens1) = tokens
    in 
        case op of
            "+" ->
                let 
                    (expr1, tokens2) = parse tokens1
                    (expr2, tokens3) = parse tokens2
                in 
                    (Soma expr1 expr2, tail tokens3)
            
            "*" ->
                let 
                    (expr1, tokens2) = parse tokens1
                    (expr2, tokens3) = parse tokens2
                in 
                    (Produto expr1 expr2, tail tokens3)

            "^" -> -- É uma potência
               let 
                    (expr1, tokens2) = parse tokens1
                   -- O segundo operando é um Int
                    (Const n, tokens3) = parse tokens2 -- Simplificação: assumindo n é Const
               in 
                    --(Potencia expr1 (round n), tail tokens3) -- tail consome o ")"
                    (Potencia expr1 n, tail tokens3) -- tail consome o ")"

            _   -> error "Operador desconhecido"

parse (token:resto) =
    case reads token :: [(Int, String)] of
        -- Tenta ler como um número
        [(val, "")] -> (Const val, resto)
        -- Se falhar, é uma variável
        _           -> (Var token, resto)

------------------------ String em Notação Infixa ------------------------
expToString :: Expressao -> String
expToString (Const n) = show n
expToString (Var x) = x
expToString (Soma eq1 eq2) = "(" ++ expToString eq1 ++ "+" ++ expToString eq2 ++ ")" -- (d)
expToString (Produto eq1 eq2) = "(" ++ expToString eq1 ++ "*" ++ expToString eq2 ++ ")"
expToString (Potencia eq1 n) = "(" ++ expToString eq1 ++ "^" ++ show n ++ ")"

------------------------ Função de diferenciação ------------------------
-- Recebe uma expressão e a variável em relação à qual se deve derivar
-- Retorna a expressão derivada

derivar :: Expressao -> String -> Expressao

-- (d/dx)(c) = 0
derivar (Const n) _ = Const 0

-- (d/dx)(x) = 1
derivar (Var x) var
    | x == var  = Const 1
    | otherwise = Const 0

-- (d/dx)(u+v) = (d/dx)(u) + (d/dx)(v)
derivar (Soma eq1 eq2) var = Soma (derivar eq1 var) (derivar eq2 var)

-- (d/dx)(u*v) = u*(d/dx)(v) + v*(d/dx)(u)
derivar (Produto eq1 eq2) var = Soma (Produto (derivar eq1 var) eq2) (Produto eq1 (derivar eq2 var))

-- (d/dx)(u^n) = n*u^(n-1) * (d/dx)(u)
derivar (Potencia eq1 n) var = Produto (Produto (Const n) (Potencia eq1 (n - 1))) (derivar eq1 var)

--------------Simplificação--------------

simplificar :: Expressao -> Expressao
simplificar (Const n) = Const n
simplificar (Var x) = Var x

-- Simplificar a soma
simplificar (Soma c1 c2) = 
    let
        c1_valor = simplificar c1
        c2_valor = simplificar c2
    in
        case (c1_valor,c2_valor) of
            (Const 0, exp) -> exp
            (exp, Const 0) -> exp
            (Const n1, Const n2) -> Const (n1 + n2)
            (f, exp) -> Soma f exp

-- Simplificar a multiplicação
simplificar (Produto c1 c2) =
    let
        c1_valor = simplificar c1
        c2_valor = simplificar c2
    in
        case (c1_valor, c2_valor) of
            (Const 0, _) -> Const 0
            (_, Const 0) -> Const 0
            (Const 1, exp) -> exp
            (exp, Const 1) -> exp
            (Const n1, Const n2) -> Const (n1 * n2)
            (Const n1, Produto (Const n2) exp) -> simplificar (Produto (Const (n1*n2)) exp)
            (Produto (Const n1) exp, Const n2) -> simplificar (Produto (Const (n1*n2)) exp)
            (f, exp) -> Produto f exp

-- Simplificar a potencia
simplificar (Potencia c1 n) =
    let
        c1_valor = simplificar c1
    in
        case (c1_valor, n) of
            (_, 0) -> Const 1
            (exp, 1) -> exp
            (Const 0, _) -> Const 0
            (Const 1, _) -> Const 1
            (Const b, expo) | expo > 0 -> Const (b ^ expo)
            (exp, expo) -> Potencia exp expo

---------------------------------------

main :: IO ()
main = do
    let prefixa = "(+ (* 3 (^ x 2)) (* 2 x))"
    let (expr, _) = parse (tokenize prefixa)
    let derivada = derivar expr "x" 
    let derivada_simplificada = simplificar derivada
    putStrLn $ "Expressão original (prefixa): " ++ prefixa
    putStrLn $ "Expressão original (infixa): " ++ expToString expr
    putStrLn $ "Expressão derivada (infixa): " ++ expToString derivada
    putStrLn $ "Expressão derivada simplificada (infixa): " ++ expToString derivada_simplificada