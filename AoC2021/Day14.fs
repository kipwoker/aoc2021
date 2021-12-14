module Day14

open System

let parse (input: string[]) =
    let template = input.[0].ToCharArray()
    let map = input
                |> Array.skip 2
                |> Array.map (fun line ->
                    let parts = line.Split([|" -> "|], StringSplitOptions.None)
                    let left = parts.[0].[0]
                    let right = parts.[0].[1]
                    let value = parts.[1].[0]
                    ((left, right), value)
                )
                |> Map.ofArray
    (template, map)

let getPairs template =
    template
    |> Array.windowed 2
    |> Array.groupBy (fun c -> (c.[0], c.[1]))
    |> Array.map (fun (key, values) -> (key, values.Length |> bigint))

let transform pairs map =
    pairs
    |> Array.map (fun (pair, count) ->
            let insert = map |> Map.tryFind pair
            let left, right = pair
            match insert with
            | Some v -> [ (left , v), count; (v, right), count]
            | None -> [pair, count]
            )
    |> Array.collect (fun x -> x |> Array.ofList)
    |> Array.groupBy fst
    |> Array.map (fun (key , values) ->
        let sum = values |> Array.fold (fun state (_, value) -> state + value) (0 |> bigint)
        (key, sum)
    )

let rec transformManyTimes pairs map count =
    match count with
    | 0 -> pairs
    | _ ->
        let pairs = transform pairs map
        transformManyTimes pairs map (count - 1)

let calculate template pairs =
    let freqs = pairs
                |> Array.map (fun ((left, right), count) -> [| (left, count); (right,count) |])
                |> Array.collect id
                |> Array.groupBy fst
                |> Array.map (fun (key, values) ->
                    let value = values |> Array.sumBy snd
                    (key, value)
                )
    let minKey, min = freqs |> Array.minBy snd
    let maxKey, max = freqs |> Array.maxBy snd

    let first = template |> Array.head
    let last = template |> Array.last

    let hack = match (first, last, minKey, maxKey) with
               | f, l, min', max' when f = min' && l = max' || f = max' && l = min' -> 0
               | f, l, min', _ when f = min' || l = min' -> -1
               | f, l, _, max' when f = max' || l = max' -> 1
               | _ -> 0
               |> bigint

    let value = (max - min) / bigint 2 + hack
    value

let solve input count =
    let template, map = input |> parse
    let pairs = template |> getPairs
    let pairs' = transformManyTimes pairs map count
    let result = calculate template pairs'
    result.ToString()

let solve1 input = solve input 10
let solve2 input = solve input 40