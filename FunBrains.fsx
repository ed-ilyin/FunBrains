type Symbol = string
type Index = int

type Term =
    | Symbol of Symbol * Index
    | Apply of Term * Term
    | Abstract of Symbol * Term
    | ExpectingGot of string * Term


let tuple x y = x, y


let rec sprint =
    function
        | Symbol (x, i) -> sprintf "%s" x
        | Apply (f, x) -> sprint x |> sprintf "%s %s" (sprint f)
        | Abstract (x, t) -> sprint t |> sprintf "(λ%s. %s)" x

        | ExpectingGot (expecting, got) ->
            sprintf "Expecting %s, but got %A" expecting got


let expectingGot expecting got = tuple expecting got |> ExpectingGot


let log tag x =
    do sprint x |> printfn "%s: %s" tag
    x


let rec apply func term =
    do printfn "applying %s to %s" (sprint func) (sprint term)

    let rec bind symbol ter body =
        do printfn "let %s = %s in %s" symbol (sprint ter) (sprint body)
        let bin = bind symbol ter

        match body with
            | Symbol (x, i) ->
                if x = symbol then term else Symbol (x, i)

            | Apply (f, x) -> bin f |> apply <| bin x

            | Abstract (x, t) ->
                if x = symbol
                    then tuple x t |> Abstract
                    else bin t |> tuple x |> Abstract

            | ExpectingGot (expecting, got) ->
                expectingGot expecting got
            
            |> log "binded"

    match func with
        | Symbol (x, i) -> Apply (Symbol (x, i), term)
        | Apply (f, x) -> apply (apply f x) term
        | Abstract (x, t) -> bind x term t
        | ExpectingGot (expecting, got) -> expectingGot expecting got
        |> log "applied"


let rec run =
    function
        | Symbol (x, i) -> Symbol (x, i)
        | Apply (f, x) -> apply f x
        | Abstract (x, t) -> run t |> tuple x |> Abstract
        | ExpectingGot (expecting, got) -> expectingGot expecting got


let curry f x y = f (x, y)
let (!) s = Symbol (s, 0)
let (*) = curry Apply
let (>) = curry Abstract
// let term = ("z" > ("x" > !"x" * !"z") * !"x") * (("y" > !"x" * !"y") * !"5")
let term = ("w" > ("1" > !"w" * !"1")) * ("x" > !"x" * !"x") * ("y" > ("z" > !"y" * !"z"))
// let term = ("x" > !"x" * !"x") * ("y" > ("z" > !"y" * !"z"))
sprint term, run term |> sprint
// and Abstraction =
//     | Double
//     | Multiply
//     | Partial of (Term -> Term)


// type Data =
//     | Int of int
//     | Float of float
//     | Error of string






// let int func =
//     function
//         | Int x -> func x
//         | got -> expectingGot "an Int" got
//         |> Partial
//         |> Function


// let float func =
//     function
//         | Float x -> func x
//         | got -> expectingGot "a Float" got
//         |> Partial
//         |> Function


// let (<*>) func argument =
//     match func with
//         | Function f ->
//             match f with
//                 | Double ->
//                     match argument with
//                         | Int x -> 2 * x |> Int
//                         | Float x -> 2. * x |> Float
//                         | _ -> Error "expecting an Int or Float"

//                 | Multiply ->
//                     match argument with
//                         | Int x -> (*) x >> Int |> int
//                         | Float x -> (*) x >> Float |> float
//                         | _ -> Error "expecting an Int or Float"

//                 | Partial f -> f argument

//         | got -> expectingGot "a Function" got

// let run =
//     function
//         | 
// // Double <*> Int 3
// // Function Multiply <*> Int 3 <*> Int 4
// let expression = Function Apply 

