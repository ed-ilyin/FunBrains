#load "Language.fsx"
open Language


let fac =
    y * ("f" 
            := "n"
            := iif
            * (iszero * !"n")
            * int 1
            * (mult * !"n" * (!"f" * (pred * !"n")))
        )


// let term = ch3
// let term = fac * int 0
// let term = fac * ch3
// let term = ("z" := ("x" := !"x" * !"z") * !"x") * (("y" := !"x" * !"y") * int 5)
// let term = ("x" := "x'" := !"x" * !"x'") * ("x" := !"x" * !"x") * ("x" := "x'" := !"x" * !"x'")
// let term = ("x" := !"x" * !"x") * ("y" := ("z" := !"y" * !"z"))
// let term = Int 5 |> Core
// let term = ("w" := "1" := !"w" * !"1") * ("x" := !"x" * !"x") * ("y" := "z" := !"y" * !"z")
// let term = ("w" := !"w" * !"w") * ("y" := "z" := !"y" * !"z")
// let term = ("z" := (!"z" * (!"y" * !"z"))) * (!"z" * !"x") * !"z"
// let term = !"a" * ("b" := (!"c" * !"d"))
// let term = plus * ch2 * ch3
// let K = "x" := "y" := !"x"
// let I = "x" := !"x"
// let w = "x" :=  !"x" * !"x"
// let W = w * w
// let term = ("x" := !"F" * !"x" * (!"G" * !"x") * !"x") * !"N"
let term = ("x" := "y" := !"x" * !"y") * !"y"
let t, _ = Lambda.run term
Lambda.treettb 0 true term |> printfn "%s"
Lambda.treettb 0 true t |> printfn "%s"
