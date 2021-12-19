module Day19

open Core
open Microsoft.FSharp.Collections

type Point = {
    X : int
    Y : int
    Z : int
}

type Scanner = {
    Number: int
    Beacons: Point[]
}

let inline parsePoint (input: int[]) = { X = input.[0]; Y = input.[1]; Z = input.[2] }
let inline toArray (point: Point) = [| point.X; point.Y; point.Z |]

let rec parse (input: string[]) (num: int) =
    let beacons = input
                  |> Array.skip 1
                  |> Array.takeWhile (fun x -> x <> "" && x <> null)
                  |> Array.map (fun line -> line.Split(',') |> Array.map (fun x -> x |> int) |> parsePoint)
    let scanner = {
        Number = num
        Beacons = beacons
    }

    let blockSize = 2 + beacons.Length

    if input.Length - blockSize <= 0 then
        [scanner]
    else
        let input' = input |> Array.skip blockSize
        [scanner] @ parse input' (num + 1)

let inline swap array i j =
    let x = Array.get array i
    let y = Array.get array j
    let new' = Array.copy array
    Array.set new' i y
    Array.set new' j x
    new'

let rec permute indexes left =
    let left' = left + 1
    let length = Array.length indexes
    match left' with
    |  l when l >= length -> [indexes]
    | _ ->
        let result = permute indexes left'
        let result' = [left' .. length - 1] |> List.map (fun i -> permute (swap indexes i left) left') |> List.collect id
        result @ result'

let generateRotations indexes =
    let axis = [-1; 1]
    permute indexes 0
    |> List.map (fun p ->
        axis
        |> List.map (fun x ->
            axis
            |> List.map (fun y ->
                axis
                |> List.map (fun z -> [|p.[0] * x; p.[1] * y; p.[2] * z|])
            )
            |> List.collect id
        )
        |> List.collect id
        |> List.append [p]
    )
    |> List.collect id

let inline calc a b op = {X = op a.X b.X; Y = op a.Y b.Y; Z = op a.Z b.Z}
let inline add a b = calc a b (+)
let inline sub a b = calc a b (-)
let inline calcDistance a b =
    let point = sub a b
    abs point.X + abs point.Y + abs point.Z

let findPivotPoint' first second =
    let set = first |> Set.ofArray
    first
    |> Array.fold (fun fs f ->
        match fs with
        | Some _ -> fs
        | None ->
            second
            |> Array.fold (fun ss s ->
                match ss with
                | Some _ -> ss
                | None ->
                    let c = sub f s
                    let count = second |> Array.filter (fun s' -> set.Contains(add c s')) |> Array.length
                    if count >= 12 then c |> Some else None
            ) None
    ) None

let rotate rotation point =
    point
    |> toArray
    |> Array.zip rotation
    |> Array.map (fun (r, coordinate) -> (abs r, coordinate * sign r))
    |> Array.sortBy fst
    |> Array.map snd
    |> parsePoint

let findPivotPoint first second rotations =
    rotations
    |> List.map (fun r -> second |> Array.map (rotate r))
    |> List.fold (fun state rotation ->
        match state with
        | Some _ -> state
        | None ->
            let pivotPoint = findPivotPoint' first rotation
            match pivotPoint with
            | Some p -> (p, rotation) |> Some
            | None -> None
    ) None

let rec combine' (scanners : Scanner[]) map rotations =
    let length = scanners.Length
    let map1 = scanners
                |> Array.fold
                ( fun map' first ->
                    match first.Number with
                    | n when map' |> Map.containsKey n |> not -> map'
                    | _ ->
                        scanners
                        |> Array.fold (fun map'' second ->
                            match second.Number with
                            | n' when map'' |> Map.containsKey n' -> map''
                            | _ ->
                                let result = findPivotPoint first.Beacons second.Beacons rotations
                                match result with
                                | None -> map''
                                | Some (pivot, rotation) ->
                                    let firstPoint = map'' |> Map.find first.Number
                                    let secondPoint = add firstPoint pivot
                                    scanners.[second.Number] <- { second with Beacons = rotation }
                                    let map''' = map'' |> Map.add second.Number secondPoint
                                    map'''
                        ) map'
                ) map
    match map1.Count = length with
    | true -> map1
    | false -> combine' scanners map1 rotations

let combine scanners =
    let map = Map.ofList [(0, { X = 0; Y = 0; Z = 0 })]
    combine' scanners map

let countBeacons scanners map =
    scanners
    |> Array.fold (fun set scanner ->
        scanner.Beacons
        |> Array.fold (fun set' beacon -> set' |> Set.add (add (map |> Map.find scanner.Number) beacon)) set
    ) Set.empty
    |> Set.count

let findMaxDistance (map : Map<int, Point>) =
    let points = map |> Map.values
    points
    |> Seq.fold(fun max' first -> points |> Seq.fold (fun max'' second -> max max'' (calcDistance first second)) max') -1

let solve input =
    let r = (fun () ->
                let scanners = (parse input 0) |> Array.ofList
                let rotations = generateRotations [| 1 .. 3 |]
                let map = combine scanners rotations
                let result = (countBeacons scanners map, findMaxDistance map)
                result.ToString()
            ) |> timeOperation
    r.ToString()