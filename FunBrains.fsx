#load "Language.fsx"
open Language


let fac =
    y * (l^l^ iif * (iszero * !0) * ch1 * (mult * !0 * (!1 * (pred * !0))))


let tree = Lambda.treettb 0 false >> printfn "%s"
let print = Lambda.sprint >> printfn "%s"
let run term =
    let tree = Lambda.treettb 0 false
    do tree term |> printfn "%s"
    let term', _ = Lambda.run term
    do tree term' |> printfn "%s"
    term'

// let term = ch3
// let term = fac * int 0
// let term = fac * ch3
// let term = ("z" := ("x" := !"x" * !"z") * !"x") * (("y" := !"x" * !"y") * int 5)
// let term = ("x" := "x'" := !"x" * !"x'") * ("x" := !"x" * !"x") * ("x" := "x'" := !"x" * !"x'")
// let term = ("x" := !"x" * !"x") * ("y" := ("z" := !"y" * !"z"))
// let term = Int 5 |> Core
// let term = ("w" := !"w" * !"w") * ("y" := "z" := !"y" * !"z")
// let term = ("z" := (!"z" * (!"y" * !"z"))) * (!"z" * !"x") * !"z"
// let term = !"a" * ("b" := (!"c" * !"d"))
// let term = plus * ch2 * ch3
// let K = "x" := "y" := !"x"
// let I = "x" := !"x"
// let w = "x" :=  !"x" * !"x"
// let W = w * w
// let term = ("x" := !"F" * !"x" * (!"G" * !"x") * !"x") * !"N"
// let term = w * ch1
let stree = Lambda.stree "" >> printfn "%s"
print fac
stree fac
