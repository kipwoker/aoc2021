module Day17

open Core
open Microsoft.FSharp.Core

type Range = {
    Min: int
    Max: int
}

type Area = {
    X: Range
    Y: Range
}

type Catch = {
    MaxY: int
    FinishX: int
    FinishY: int
}

let parseRange(line: string) =
    let parts = line.Split("..")
    {
        Min = parts.[0] |> int
        Max = parts.[1] |> int
    }

let parse (input: string[]) =
    let ranges = input |> Array.map parseRange
    {
        X = ranges.[0]
        Y = ranges.[1]
    }

let inline isInRange value range = range.Min <= value && value <= range.Max
let inline contains (area : Area) (x,y) = isInRange x area.X && isInRange y area.Y
let inline getDx (vx) =
    match vx with
    | vx'' when vx'' < 0 -> 1
    | vx'' when vx'' > 0 -> -1
    | _ -> 0

let rec launch (vx: int, vy: int) (x,y) (maxY: int) (area: Area) : Catch option =
    match (x,y) with
    | (_, y') when y' < area.Y.Min -> None
    | (x', _) when (x' < area.X.Min && vx <= 0) || (x' > area.X.Max && vx >= 0) -> None
    | (x', y') when contains area (x',y') -> { MaxY = maxY; FinishX = x; FinishY = y } |> Some
    | _ ->
        let x' = x + vx
        let y' = y + vy
        let vx' = vx + getDx vx
        let vy' = vy - 1
        let maxY' = max maxY y'
        launch (vx', vy') (x',y') maxY' area

let launch' (area: Area) =
    [-200..200]
    |> List.map (fun vx ->
        [-200..200]
        |> List.map (fun vy -> launch (vx,vy) (0,0) 0 area))
    |> List.collect id

let findMaxY (area: Area) =
    area
    |> launch'
    |> List.maxBy (fun r -> if r.IsSome then r.Value.MaxY else -9000)

let countSuccessLaunches (area: Area) =
    area
    |> launch'
    |> List.fold (fun state r -> state + if r.IsSome then 1 else 0) 0

let solve1 input =
    let result = input |> parse |> findMaxY
    result.ToString()

let solve2 input =
    let result = input |> parse |> countSuccessLaunches
    result.ToString()