#load "Language.fsx"
open Language

let fac = y * ("f" := "n" := iif * (iszero * !"n") * co1 * (mult * !"n" * (!"f" * (pred * !"n"))))
// let term = iif * fls * !"v" * !"w"
let term = w
let t, _ = Lambda.run term
Lambda.sprint t
