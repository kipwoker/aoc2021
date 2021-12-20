module Day20

open Core
open Microsoft.FSharp.Collections

type Bounds ={
    MinX: int
    MaxX: int
    MinY: int
    MaxY: int
}

let inline mapCharToInt c = if c = '#' then 1 else 0

let parse (input: string[]) =
    let enhancement = (input |> Array.head).ToCharArray() |> Array.map mapCharToInt
    let map = input
                |> Array.skip 2
                |> Array.mapi (fun i s -> s.ToCharArray() |> Array.mapi (fun j c -> ((i,j), mapCharToInt c)))
                |> Array.collect id
                |> Array.filter (fun (_, value) -> value = 1)
                |> Map.ofArray
    (enhancement, map)

let getBounds map =
    let minX', maxX', minY', maxY' = map
                                     |> Map.fold (fun (minX, maxX, minY, maxY) (x,y) _ -> (min x minX, max x maxX, min y minY, max y maxY)) (100000, -100000, 100000, -100000)
    let frame = 1
    { MinX = minX' - frame; MaxX = maxX' + frame; MinY = minY' - frame; MaxY = maxY' + frame }

let print map =
    let bounds = getBounds map
    [bounds.MinX .. bounds.MaxX]
    |> List.iter (fun i ->
        [bounds.MinY .. bounds.MaxY]
        |> List.iter (fun j ->
            let value = map |> Map.tryFind (i,j)
            match value with
            | None -> printf "."
            | Some 0 -> printf "."
            | _ -> printf "#"
        )
        printfn ""
    )

let calcCell x y map def =
     let delta = [|-1 .. 1|]
     delta |> Array.map (fun i ->
         delta |> Array.map (fun j ->
            let el = map |> Map.tryFind (i + x, j + y)
            if el.IsSome then el.Value else def
         )
     )
     |> Array.collect id
     |> toDecimal

let enhance' enhancement map def =
    let bounds = getBounds map
    //printfn $"%A{bounds}"
    [| bounds.MinX .. bounds.MaxX |]
    |> Array.fold (fun map' x ->
        [| bounds.MinY .. bounds.MaxY |]
        |> Array.fold (fun map'' y ->
            let idx = calcCell x y map def
            let value = Array.get enhancement idx
            let current = map'' |> Map.tryFind (x,y)
            match (current, value) with
            | None, _ -> map'' |> Map.add (x,y) value
            | Some _, _ -> map'' |> Map.remove (x,y) |> Map.add (x,y) value
        ) map'
    ) map

let rec enhance enhancement map count def =
    //print map
    match count with
    | 0 -> map
    | _ ->
        let map' = enhance' enhancement map def
        let def' =
            if enhancement.[0] = 1 then
                (if def = 0 then 1 else 0)
            else
                0
        enhance enhancement map' (count - 1) def'

let calcLights map =
    map |> Map.fold (fun sum (_,_) value -> value + sum) 0

let solve input =
    let enhancement, map = input |> parse
    let calc count = enhance enhancement map count 0 |> calcLights
    let result = (calc 2, calc 50)
    result.ToString()