type Symbol = string

type Term =
    | S of Symbol
    | A of Term * Term
    | L of Symbol * Term
    | Core of Core


and Core =
    | Int of int
    | Float of float


type Command = Stop | Continue


let tuple x y = x, y


let rec sprint =
    function
        | S x -> sprintf "%s" x
        | A (f, x) -> sprint x |> sprintf "(%s %s)" (sprint f)
        | L (x, t) -> sprint t |> sprintf "(λ%s. %s)" x
        | Core x -> sprintf "(%A)" x

let rec treettb level isNewLine =
    let pre =
        if isNewLine
            then String.replicate level "| " |> sprintf "\n%s"
            else ""

    function
        | S x -> sprintf "%s%s" pre x

        | A (f, x) ->
            treettb level true x
                |> sprintf "%s@ %s%s" pre (treettb (level + 1) false f)

        | L (x, t) -> treettb level true t |> sprintf "%sλ %s%s" pre x
        | Core x -> sprintf "%s%A" pre x

let rec treeltr level isNewLine =
    let pre =
        if isNewLine
            then String.replicate level "| " |> sprintf "\n%s"
            else ""

    function
        | S x -> sprintf "%s%s" pre x

        | A (f, x) ->
            treeltr level true f
                |> sprintf "%s@ %s%s" pre (treeltr (level + 1) false x)

        | L (x, t) ->
            S x
                |> treeltr level true
                |> sprintf "%sλ %s%s" pre (treeltr (level + 1) false t)

        | Core x -> sprintf "%s%A" pre x

let log tag x =
    do sprint x |> printfn "%s: %s" tag
    // do printfn "%s: %A" tag x
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
            match run f with
                | f', Stop -> A (f', x), Stop 
                | f', Continue -> A (f', x) |> run

        | L (x, t) -> L (x, t), Continue 
        | Core x -> Core x, Stop
