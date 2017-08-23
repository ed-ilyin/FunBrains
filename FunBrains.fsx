type Symbol = string


type Core =
    | Int of int
    | Float of float

    
type Term =
    | Symbol of Symbol
    | Apply of Term * Term
    | Abstract of Symbol * Term
    | Core of Core


let tuple x y = x, y


let rec sprint =
    function
        | Symbol x -> sprintf "%s" x
        | Apply (f, x) -> sprint x |> sprintf "%s (%s)" (sprint f)
        | Abstract (x, t) -> sprint t |> sprintf "%s -> %s" x
        | Core x -> sprintf "%A" x


let log tag x =
    do sprint x |> printfn "%s: %s" tag
    x


let flip f x y = f y x


let rename s = s + "'"


let rec alpha symbol =
    let alph term = alpha symbol term

    function
        | Symbol s ->
            if s = symbol then rename s |> Symbol else Symbol s

        | Apply (f, x) -> Apply (alph f, alph x)

        | Abstract (s, t) ->
            if s = symbol
                then rename s
                else s
                |> (alph t |> flip tuple >> Abstract)

        | Core x -> Core x

let rec apply argument =
    let rec beta symbol =
        let bet term = beta symbol term

        function
            | Symbol x -> if x = symbol then argument else Symbol x 
            | Apply (f, t) -> bet t |> apply <| bet f

            | Abstract (s, t) ->
                match s = symbol, Symbol s = argument with
                    | false, false -> s, bet t
                    | false, true -> rename s, alpha s t |> bet
                    | true, _ -> s, t
                    |> Abstract
                    
            | Core x -> Core x

    argument
        |> sprint
        |> sprintf "apply to %s of"
        |> log
        >> function
        | Symbol x -> Apply (Symbol x, argument)

        | Apply (f, x) -> f |> apply x |> functionapply argument
        
        | Abstract (x, t) -> beta x t
        | Core x -> Core x


let rec run =
    function
        | Symbol x -> Symbol x
        | Apply (f, x) -> f |> apply x
        | Abstract (x, t) -> Abstract (x, run t)
        | Core x -> Core x


let curry f x y = f (x, y)
let (!) = Symbol
let (*) = curry Apply
let (:=) = curry Abstract
let int = Int >> Core
// let term = ("z" := ("x" := !"x" * !"z") * !"x") * (("y" := !"x" * !"y") * int 5)
// let term = ("w" := "1" := !"w" * !"1") * ("x" := !"x" * !"x") * ("y" := "z" := !"y" * !"z")
// let term = ("x" := "x'" := !"x" * !"x'") * ("x" := !"x" * !"x") * ("x" := "x'" := !"x" * !"x'")
// let term = ("x" := !"x" * !"x") * ("y" := ("z" := !"y" * !"z"))
// let term = Int 5 |> Core
let term = ("z" := (!"z" * (!"y" * !"z"))) * (!"z" * !"x") * !"z"
sprint term, run term |> sprint