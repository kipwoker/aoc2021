module Day22

open Core

type State = Off | On

type Range = {
    Min : int
    Max : int
}

type Command = {
    State: State
    X : Range
    Y : Range
    Z : Range
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
        { State = state; X = ranges.[0]; Y = ranges.[1]; Z = ranges.[2] }
    )

let apply (commands: Command[]) (map : Map<int*int*int, State>) limit =
    let rx,ry,rz = limit
    commands
    |> Array.fold (fun map' command ->
        let xmin = max rx.Min command.X.Min
        let ymin = max ry.Min command.Y.Min
        let zmin = max rz.Min command.Z.Min
        let xmax = min rx.Max command.X.Max
        let ymax = min ry.Max command.Y.Max
        let zmax = min rz.Max command.Z.Max
        [xmin..xmax]
        |> List.fold (fun mapx x ->
            [ymin..ymax]
            |> List.fold (fun mapy y ->
                [zmin..zmax]
                |> List.fold (fun mapz z ->
                    mapz
                    |> Map.change (x,y,z) (fun _ -> Some command.State)
                ) mapy
            ) mapx
        ) map'
    ) map

let calcOn (map : Map<int*int*int, State>) =
    map |> Map.fold (fun sum _ state -> sum + (if state = On then 1 else 0)) 0

let solve input =
    let commands = input |> parse
    let limit = ({Min = -50; Max = 50}, {Min = -50; Max = 50}, {Min = -50; Max = 50})
    let map = apply commands Map.empty limit
    let task1 = map |> calcOn
    let result = task1
    result.ToString()