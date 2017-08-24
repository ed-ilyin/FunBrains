type Symbol = string


type Core =
    | Int of int
    | Float of float

    
type Term =
    | Symbol of Symbol
    | Apply of Term * Term
    | Lambda of Symbol * Term
    | Core of Core


type Command = Stop | TryAgain

let tuple x y = x, y


let rec sprint =
    function
        | Symbol x -> sprintf "%s" x
        | Apply (f, x) -> sprint x |> sprintf "(%s %s)" (sprint f)
        | Lambda (x, t) -> sprint t |> sprintf "(λ%s. %s)" x
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
        | Symbol s ->
            if s = symbol then rename s |> Symbol else Symbol s

        | Apply (f, x) -> Apply (alph f, alph x)

        | Lambda (s, t) ->
            if s = symbol
                then rename s
                else s
                |> (alph t |> flip tuple >> Lambda)

        | Core x -> Core x


let rec beta symbol argument =
    let bet term = beta symbol argument term

    function
        | Symbol x -> if x = symbol then argument else Symbol x 
        | Apply (f, t) -> Apply (bet f, bet t)

        | Lambda (s, t) ->
            match s = symbol, Symbol s = argument with
                | false, false -> s, bet t
                | false, true -> rename s, t |> alpha s |> bet
                | true, _ -> s, t
                |> Lambda
                
        | Core x -> Core x


let rec run =
    function
        | Symbol x -> Symbol x, Stop
        | Apply (Lambda (s, t), a) -> t |> beta s a |> run

        | Apply (f, x) ->
            match run f, run x with
                | (f, Stop), (x, _) -> Apply (f, x), Stop
                | (f, _), (x, _) -> Apply (f, x) |> run

        | Lambda (x, t) ->
            let t, _ = run t
            Lambda (x, t), TryAgain

        | Core x -> Core x, Stop


let curry f x y = f (x, y)
let (!) = Symbol
let (*) = curry Apply
let (:=) = curry Lambda
let int = Int >> Core
// let term = ("z" := ("x" := !"x" * !"z") * !"x") * (("y" := !"x" * !"y") * int 5)
// let term = ("x" := "x'" := !"x" * !"x'") * ("x" := !"x" * !"x") * ("x" := "x'" := !"x" * !"x'")
// let term = ("x" := !"x" * !"x") * ("y" := ("z" := !"y" * !"z"))
// let term = Int 5 |> Core
let term = ("w" := "1" := !"w" * !"1") * ("x" := !"x" * !"x") * ("y" := "z" := !"y" * !"z")
// let term = ("z" := (!"z" * (!"y" * !"z"))) * (!"z" * !"x") * !"z"
let t, cmd = run term
sprint term, sprint t, cmd
