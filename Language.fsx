#load "Lambda.fsx"
let curry f x y = f (x, y)
let (!) = Lambda.S
let (*) = curry Lambda.A
let (:=) = curry Lambda.L
let int = Lambda.Int >> Lambda.Core
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
let zp = pair * ch0 * ch0
let sp = "p" := pair * (snd * !"p") * (succ * (snd * !"p"))
let pred = "m" := fst * (!"m" * sp * zp)
let y = "f" := ("x" := !"f" * (!"x" * !"x")) * ("x" := !"f" * (!"x" * !"x"))