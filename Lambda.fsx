type Symbol = string


type Core =
    | Int of int
    | Float of float

    
type Term =
    | S of Symbol
    | A of Term * Term
    | L of Symbol * Term
    | Core of Core


type Command = Stop | TryAgain


let tuple x y = x, y


let rec sprint =
    function
        | S x -> sprintf "%s" x
        | A (f, x) -> sprint x |> sprintf "(%s %s)" (sprint f)
        | L (x, t) -> sprint t |> sprintf "(λ%s. %s)" x
        | Core x -> sprintf "%A" x


let log tag x =
    // do sprint x |> printfn "%s: %s" tag
    do printfn "%s: %A" tag x
    x


let flip f x y = f y x


let rename s = s + "'"


let rec alpha symbol =
    let alph term = alpha symbol term

    function
        | S s ->
          if s = symbol then rename s |> S else S s

        | A (f, x) -> A (alph f, alph x)

        | L (s, t) ->
            if s = symbol
                then rename s
                else s
                |> (alph t |> flip tuple >> L)

        | Core x -> Core x


let rec beta symbol argument =
    let bet term = beta symbol argument term

    function
        | S x -> if x = symbol then argument else S x 
        | A (f, t) -> A (bet f, bet t)

        | L (s, t) ->
            match s = symbol, S s = argument with
                | false, false -> s, bet t
                | false, true -> rename s, t |> alpha s |> bet
                | true, _ -> s, t
                |> L
                
        | Core x -> Core x


let rec run =
    function
        | S x -> S x, Stop
        | A (L (s, t), a) -> t |> beta s a |> run

        | A (f, x) ->
            match run f, run x with
                | (f, Stop), (x, _) -> A (f, x), Stop
                | (f, _), (x, _) -> A (f, x) |> run

        | L (x, t) ->
            let t, _ = run t
            L (x, t), TryAgain

        | Core x -> Core x, Stop
