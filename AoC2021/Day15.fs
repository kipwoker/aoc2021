module Day15

open Core
open Microsoft.FSharp.Collections

type Element = {
    Risk: int
    X: int
    Y: int
}

let parse (input: string[]) =
    input |> Array.map (fun x -> x.ToCharArray() |> Array.map charToInt)

let getGrid m n i j =
    [
        if i <= 0 then [] else [(i - 1, j)]
        if j <= 0 then [] else [(i, j - 1)]
        if i >= m - 1 then [] else [(i + 1, j)]
        if j >= n - 1 then [] else [(i, j + 1)]
    ] |> List.collect id

let compare (x : Element) (y: Element) =
    match (x.Risk > y.Risk, x.Risk < y.Risk) with
    | (true, _) -> -1
    | (_, true) -> 1
    | _ -> 0

let createHeap capacity = PriorityQueue(capacity, compare)

let rec dijkstra' (a : int[][]) (heap : PriorityQueue<Element>) visited =
    match heap.IsEmpty with
    | false ->
        let element = heap.RemoveHead()
        let (m, n) = getSizes a
        match (element.X, element.Y) = (m - 1, n - 1) with
        | true -> element.Risk
        | false ->
            let grid = getGrid m n element.X element.Y
            let visited' = grid
                            |> List.filter (fun (x,y) -> visited |> Set.contains (x,y) |> not)
                            |> List.map (fun (x,y) ->
                                    heap.Insert({ Risk = element.Risk + a.[x].[y]; X = x; Y = y})
                                    (x,y)
                                )
                            |> List.fold (fun state pair -> state |> Set.add pair) visited
            dijkstra' a heap visited'
    | true -> -1000

let dijkstra (a : int[][]) =
    let (m, n) = getSizes a
    let heap = createHeap (n * m)
    heap.Insert({ Risk = 0; X = 0; Y = 0 })
    let visited = [(0, 0)] |> Set.ofList
    dijkstra' a heap visited

let clone count (a: int[][]) =
    let m = a.Length
    let n = a.[0].Length
    let m' = m * count
    let n' = n * count
    Array.init m' (fun i ->
        Array.init n' (fun j ->
            let el = i /m + j / n + a.[i % m].[j % n]
            if el >= 10 then (el % 9) + 1 else el
        )
    )

let solve1 input =
    let result = input |> parse |> dijkstra
    result.ToString()

let solve2 input =
    let result = input |> parse |> clone 5 |> dijkstra
    result.ToString()