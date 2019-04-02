namespace FSharp.Either

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b
    with
        member this.Swap() =
            match this with
            | Left(a) -> Right(a)
            | Right(b) -> Left(b)

module Either =
    let map f e =
        match e with
        | Left(l) -> Left(l)
        | Right(b) -> Right(f(b))

    let inline swap (e : Either<_,_>) = e.Swap()

    let swapped f e = e |> (swap >> f >> swap)

    let isLeft e =
        match e with
        | Left(_) -> true
        | _ -> false

    let isRight e = e |> isLeft |> not

    let fold l r e =
        match e with
        | Left(a) -> l(a)
        | Right(b) -> r(b)

    let bimap f g e =
        match e with
        | Left(a) -> Left(f(a))
        | Right(b) -> Right(g(b))

    let leftMap f e =
        match e with
        | Left(a) -> Left(f(a))
        | Right(b) -> Right(b)

    let iter f e =
        match e with
        | Left(_) -> ()
        | Right(a) -> f a

    let bind f e =
        match e with
        | Left(l) -> Left(l)
        | Right(b) -> f(b)

    let toSeq e =
        match e with
        | Left(_) -> Seq.empty
        | Right(b) -> b |> Seq.singleton

    let toList e =
        match e with
        | Left(_) -> []
        | Right(b) -> [b]

    let toArray e =
        match e with
        | Left(_) -> [||]
        | Right(b) -> [|b|]

    let toOption e =
        match e with
        | Left(_) -> None
        | Right(b) -> Some(b)

    let getOrElse f e =
        match e with
        | Left(_) -> f()
        | Right(b) -> b

    let orElse f e =
        match e with
        | Left(_) -> f()
        | Right(b) -> Right(b)

[<AutoOpen>]
module Operators =
    let inline (>>=) e f = Either.bind f e
