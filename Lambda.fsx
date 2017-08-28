type Index = uint32

type Term =
    | I of Index
    | A of Term * Term
    | L of Term
    | Core of Core


and Core =
    | Int of int
    | Float of float


type Command = NoOp | TryAgain


let tuple x y = x, y


let apply f x = A (f, x)


let rec sprint term =
    let (:=) t1 t2 =
        let s1 = sprint t1
        sprint t2 |> sprintf "(%s) (%s)" s1
    
    let (*) t1 t2 = sprint t2 |> sprintf "%s %s" (sprint t1)
    
    match term with
        | I i -> sprintf "%i" i
        | L t -> sprint t |> sprintf "λ %s"
        | A (I i1, I i2) -> I i1 * I i2
        | A (I i, L t) -> I i * L t

        | A (I i, A (f, x)) ->
            apply f x |> sprint |> sprintf "%s (%s)" (I i |> sprint)

        | A (L t, I i) ->
            I i |> sprint |> sprintf "(%s) %s" (L t |> sprint)

        | A (A (f, x), I i) -> apply f x * I i
        | A (f, x) -> f := x
        | Core x -> sprintf "(%A)" x


let rec stree tab =
    let tree term = stree tab term
    let right shift term = stree (tab + shift) term

    let (:=) t1 t2 =
        let s1 = tree t1
        right "  " t2 |> sprintf "%s\n%s∙ %s" s1 tab
    
    let (*) t1 t2 = tree t2 |> sprintf "%s %s" (tree t1)
    
    function
        | I i -> sprintf "%i" i
        | L t -> right "  " t |> sprintf "λ %s" 
        | A (I i1, I i2) -> I i1 * I i2
        | A (I i, L t) -> I i * L t
        | A (A (f, x), I i) -> apply f x * I i
        | A (f, x) -> f := x
        | Core x -> sprintf "(%A)" x


let rec treettb level isNewLine =
    let pre =
        if isNewLine
            then String.replicate level "| " |> sprintf "\n%s"
            else ""

    function
        | I x -> sprintf "%s%i" pre x

        | A (f, x) ->
            treettb level true x
                |> sprintf "%s@ %s%s" pre (treettb (level + 1) false f)

        | L t -> treettb level true t |> sprintf "%sλ %s" pre
        | Core x -> sprintf "%s%A" pre x

let rec treeltr level isNewLine =
    let pre =
        if isNewLine
            then String.replicate level "| " |> sprintf "\n%s"
            else ""

    function
        | I x -> sprintf "%s%i" pre x

        | A (f, x) ->
            treeltr level true f
                |> sprintf "%s@ %s%s"
                    pre
                    (treeltr (level + 1) false x)

        | L t -> treeltr (level + 1) false t |> sprintf "%sλ %s" pre

        | Core x -> sprintf "%s%A" pre x

let log tag x =
    do sprint x |> printfn "%s: %s" tag
    // do printfn "%s: %A" tag x
    x


let flip f x y = f y x


let rename s = s + "'"


// let rec alpha symbol =
//     let alph term = alpha symbol term

//     function
//         | S s -> if s = symbol then rename s |> S else S s 
//         | A (f, x) -> A (alph f, alph x)

//         | L (s, t) ->
//             if s = symbol
//                 then rename s
//                 else s
//                 |> (alph t |> flip tuple >> L)

//         | Core x -> Core x


let rec beta argument index term =
    let bet term i = beta argument i term

    let term' =
        match term with
            | I i -> if i = index then argument else I i
            | A (f, t) -> A (bet f index, bet t index)
            | L t -> index + 1u |> bet t
            | Core x -> Core x

    do sprint term'
        |> printfn "beta %s [ %i := %s ] -> %s" 
            (sprint term)
            index
            (sprint argument)
        
    term'


let rec isFree index term =
    let sterm = sprint term
    // do printfn "is free %i <- %s" index sterm

    let isFree' =
        match term with
            | I i -> i = index
            | L t -> isFree (index + 1u) t
            | A (f, x) -> isFree index f || isFree index x
            | Core _ -> false

    do printfn "is free %i %s -> %b" index sterm isFree'
    isFree'

let eta term =
    let sterm = sprint term
    do printfn "eta <- %s" sterm
    let term' = if isFree 0u term then L term else term
    do sprint term' |> printfn "eta %s -> %s" sterm
    term'


let rec run =
    let log d t =
        do sprint t
            |> printfn "run %s %s" d
            
        t

    log "<-"
        >> function
        | I i -> I i, NoOp
        | L t -> eta t, NoOp 
        | Core x -> Core x, NoOp

        | A (A (L t, a2), a1) ->
            t |> beta a2 0u |> flip tuple a1 |> A |> run

        | A (L t, a) -> beta a 0u t |> run

        | A (f, x) ->
            match run f with
                | f', NoOp -> A (f', x), NoOp 
                | f', TryAgain -> A (f', x) |> run

        >> (fun (t, cmd) -> log "->" t, cmd)
