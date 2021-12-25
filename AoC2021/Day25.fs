module Day25

open Core

type Cucumber = East | South

let parse (input: string[]) =
    let m = input.Length
    let n = input.[0].Length
    let map = input
                |> Array.mapi (fun i line ->
                    line.ToCharArray()
                    |> Array.mapi (fun j el ->
                        match el with
                        | '>' -> ((i, j), East)
                        | 'v' -> ((i, j), South)
                        | _ -> ((-1,-1), East)
                    )
                    |> Array.filter (fun ((x,y),_) -> x <> -1 && y <> -1)
                )
                |> Array.collect id
                |> Map.ofArray
    (map, m, n)

let makeStep map m n cucumber f =
    [0..(m-1)]
    |> List.fold (fun map' i ->
       [0..(n-1)]
       |> List.fold (fun (map'', movesCount) j ->
           let el = map |> Map.tryFind (i,j)
           match el with
           | Some c when c = cucumber ->
               let i',j' = f i j
               let target = map |> Map.tryFind (i', j')
               match target with
               | None -> map'' |> Map.add (i', j') c, movesCount + 1
               | _ -> map'' |> Map.add (i, j) c, movesCount
           | Some c -> map'' |> Map.add (i, j) c, movesCount
           | _ -> map'', movesCount
       ) map'
    ) (Map.empty, 0)

let rec simulate map m n turnsCount =
    let eastMap, eastCount =  makeStep map m n East (fun i j -> (i, (j + 1) % n))
    let southMap, southCount =  makeStep eastMap m n South (fun i j -> ((i + 1) % m, j))
    let totalCount = eastCount + southCount

    if totalCount = 0 then
        turnsCount
    else
        simulate southMap m n (turnsCount + 1)

let solve input =
    let init, m, n = input |> parse
    let result = simulate init m n 1
    result.ToString()