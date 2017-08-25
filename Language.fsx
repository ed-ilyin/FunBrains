#load "Lambda.fsx"
let curry f x y = f (x, y)
let (!) = Lambda.S
let (*) = curry Lambda.A
let (:=) = curry Lambda.L
let int = Lambda.Int >> Lambda.Core
// let term = ("z" := ("x" := !"x" * !"z") * !"x") * (("y" := !"x" * !"y") * int 5)
// let term = ("x" := "x'" := !"x" * !"x'") * ("x" := !"x" * !"x") * ("x" := "x'" := !"x" * !"x'")
// let term = ("x" := !"x" * !"x") * ("y" := ("z" := !"y" * !"z"))
// let term = Int 5 |> Core
// let term = ("w" := "1" := !"w" * !"1") * ("x" := !"x" * !"x") * ("y" := "z" := !"y" * !"z")
// let term = ("w" := !"w" * !"w") * ("y" := "z" := !"y" * !"z")
// let term = ("z" := (!"z" * (!"y" * !"z"))) * (!"z" * !"x") * !"z"
let tru = "t" := "f" := !"t"
let fls = "t" := "f" := !"f"
let iif = "b" := "x" := "y" := !"b" * !"x" * !"y"
let not = "b" := !"b" * fls * tru
let ``and`` = "x" := "y" := !"x" * !"y" * fls
let pair = "x" := "y" := "f" := !"f" * !"x" * !"y"
let fst = "p" := !"p" * tru
let snd = "p" := !"p" * fls
let ch0 = "s" := "z" := !"z"
let ch1 = "s" := "z" := !"s" * !"z"
let ch2 = "s" := "z" := !"s" * (!"s" * !"z")
let ch3 = "s" := "z" := !"s" * (!"s" * (!"s" * !"z"))
let ch4 = "s" := "z" := !"s" * (!"s" * (!"s" * (!"s" * !"z")))
let ch5 = "s" := "z" := !"s" * (!"s" * (!"s" * (!"s" * (!"s" * !"z"))))
let iszero = "n" := !"n" * ("x" := fls) * tru
let succ = "n" := "s" := "z" := !"s" * (!"n" * !"s" * !"z")
let plus = "m" := "n" := "s" := "z" := !"m" * !"s" * (!"n" * !"s" * !"z")
let mult = "m" := "n" := "s" := !"m" * (!"n" * !"s")
let co1 = "y" := "z" := !"y" * !"z"
let zp = pair * ch0 * ch0
let sp = "p" := pair * (snd * !"p") * (succ * (snd * !"p"))
let pred = "m" := fst * (!"m" * sp * zp)
let w = "x" := !"f" * (!"x" * !"x")
let y = "f" := w * w