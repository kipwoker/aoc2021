module Day6

let private parse (input: string[]) =
    input.[0].Split(',')
    |> Array.map (fun x -> x |> int)
    |> Array.groupBy id
    |> Array.map (fun (key, values) -> (key, values.Length |> bigint))
    |> Map.ofArray

let private addOrSet key value map =
    map |> Map.change key (fun x ->
        match x with
        | Some x' -> x' + value |> Some
        | None -> value |> Some
    )

let rec private iterate map =
    let map' = Array.init 10 (fun x -> x - 1)
                |> Array.fold (fun m index ->
                    let value = m |> Map.tryFind (index + 1)
                    match value with
                    | Some v -> m |> Map.remove index |> Map.add index v |> Map.remove (index + 1)
                    | None -> m
                ) map
    let minusOne = map' |> Map.tryFind -1
    match minusOne with
    | Some value ->
        map'
        |> addOrSet 6 value
        |> addOrSet 8 value
        |> Map.remove -1
    | None -> map'

let private generate map count =
    Array.init count id
    |> Array.fold (fun state _ -> iterate state) map

let private count map =
    map |> Map.fold (fun state _ v -> state + v) (0 |> bigint)

let solve (input: string[]) (generationsCount: int) : string =
    let origin = input |> parse
    let map = generate origin generationsCount
    let result = map |> count
    result.ToString()

let solve1 input = solve input 80
let solve2 input = solve input 256