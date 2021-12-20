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
    { MinX = minX' - 1; MaxX = maxX' + 1; MinY = minY' - 1; MaxY = maxY' + 1 }

let calcCell x y map =
     let delta = [|-1 .. 1|]
     let binary = delta
                  |> Array.map (fun i ->
                      delta |> Array.map (fun j ->
                         let el = map |> Map.tryFind (i + x, j + y)
                         if el.IsSome then el.Value else 0
                      )
                  )
                  |> Array.collect id
     binary |> toDecimal

let enhance' enhancement map =
    let bounds = getBounds map
    printfn $"%A{bounds}"
    [| bounds.MinX .. bounds.MaxX |]
    |> Array.fold (fun map' x ->
        [| bounds.MinY .. bounds.MaxY |]
        |> Array.fold (fun map'' y ->
            let idx = calcCell x y map
            let value = Array.get enhancement idx
            let current = map'' |> Map.tryFind (x,y)
            match (current, value) with
            | None, 0 -> map''
            | None, _ -> map'' |> Map.add (x,y) value
            | Some _, 0 -> map'' |> Map.remove (x,y)
            | Some _, _ -> map''
        ) map'
    ) map

let rec enhance enhancement map count =
    match count with
    | 0 -> map
    | _ ->
        let map' = enhance' enhancement map
        enhance enhancement map' (count - 1)

let calcLights map =
    map |> Map.fold (fun sum (_,_) value -> value + sum) 0

let solve input =
    let enhancement, map = input |> parse
    let map' = enhance enhancement map 2
    let result = map' |> calcLights
    result.ToString()