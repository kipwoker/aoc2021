module Day22

open System.Numerics
open Core

type State = Off | On

type Range = {
    Min : int
    Max : int
}

type Cuboid = {
    X : Range
    Y : Range
    Z : Range
}

type Command = {
    State: State
    Cuboid : Cuboid
}

let parseRange (x : string) =
    let parts = x.Split("..") |> Array.map int
    { Min = parts.[0]; Max = parts.[1] }

let parse (input : string[]) =
    input
    |> Array.map (fun line ->
        let parts = line.Split(' ')
        let state = match parts.[0] with
                    | "on" -> On
                    | _ -> Off
        let ranges = parts.[1].Split(',') |> Array.map(fun x -> x.Split('=').[1] |> parseRange)
        let cuboid = { X = ranges.[0]; Y = ranges.[1]; Z = ranges.[2] }
        { State = state; Cuboid = cuboid }
    )

let inline getLength range = max 0 (range.Max - range.Min + 1) |> bigint
let inline count c =
    let dx = getLength c.X
    let dy = getLength c.Y
    let dz = getLength c.Z
    dx * dy * dz

let inline maximin a b = max a.Min b.Min
let inline minimax a b = min a.Max b.Max

let inline intersect a b =
    let cuboid = {
                    X = { Min = maximin a.X b.X; Max = minimax a.X b.X }
                    Y = { Min = maximin a.Y b.Y; Max = minimax a.Y b.Y }
                    Z = { Min = maximin a.Z b.Z; Max = minimax a.Z b.Z }
                 }
    match count cuboid with
    | z when z = bigint 0 -> None
    | _ -> Some cuboid

let inline inverse state = if state = On then Off else On
let apply commands =
    commands
    |> Array.fold (fun result a ->
        let intersected = result
                          |> List.map (fun b -> (b.State, intersect a.Cuboid b.Cuboid))
                          |> List.filter (fun (_,y) -> y.IsSome)
                          |> List.map (fun (x,y) -> { State = inverse x; Cuboid = y.Value })
        let lighted = if a.State = On then [a] else []
        result @ intersected @ lighted
    ) []

let inline sign x = if x.State = On then bigint 1 else bigint -1
let calcOn commands limit =
    commands
    |> List.fold(fun sum command ->
        let cuboid = match limit with
                     | Some l -> intersect command.Cuboid l
                     | None -> command.Cuboid |> Some
        match cuboid with
        | Some cuboid' -> sum + count cuboid' * sign command
        | None -> sum
    ) BigInteger.Zero

let solve input =
    let commands = input |> parse
    let limit = { X = {Min = -50; Max = 50}; Y = {Min = -50; Max = 50}; Z = {Min = -50; Max = 50}}
    let commands' = apply commands
    let task1 = calcOn commands' (Some limit)
    let task2 = calcOn commands' None
    let result = (task1, task2)
    result.ToString()