module Day25

open Core

type Cucumber = East | South

let print map m n =
    [0..(m-1)]
    |> List.iter (fun i ->
        [0..(n-1)]
        |> List.iter (fun j ->
            let el = map |> Map.tryFind (i,j)
            match el with
            | Some East -> printf ">"
            | Some South -> printf "v"
            | None -> printf "."
        )
        printfn ""
    )
    printfn ""

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

let rec simulate map m n turnsCount =
    //print map m n
    let (eastMap, eastCount) = [0..(m-1)]
                                |> List.fold (fun map' i ->
                                    [0..(n-1)]
                                    |> List.fold (fun (map'', movesCount) j ->
                                        let el = map |> Map.tryFind (i,j)
                                        match el with
                                        | Some East ->
                                            let j' = (j + 1) % n
                                            //printfn $"%i{j} %i{j'} %i{n}"
                                            let target = map |> Map.tryFind (i, j')
                                            match target with
                                            | None -> map'' |> Map.add (i, j') East, movesCount + 1
                                            | _ -> map'' |> Map.add (i, j) East, movesCount
                                        | Some South -> map'' |> Map.add (i, j) South, movesCount
                                        | None -> map'', movesCount
                                    ) map'
                                ) (Map.empty, 0)

    //print eastMap m n

    let (southMap, totalCount) = [0..(m-1)]
                                 |> List.fold (fun map' i ->
                                     [0..(n-1)]
                                     |> List.fold (fun (map'', movesCount) j ->
                                         let el = eastMap |> Map.tryFind (i,j)
                                         match el with
                                         | Some South ->
                                             let i' = (i + 1) % m
                                             let target = eastMap |> Map.tryFind (i', j)
                                             match target with
                                             | None -> map'' |> Map.add (i', j) South, movesCount + 1
                                             | _ -> map'' |> Map.add (i, j) South, movesCount
                                         | Some East -> map'' |> Map.add (i, j) East, movesCount
                                         | _ -> map'', movesCount
                                     ) map'
                                 ) (Map.empty, eastCount)

    if totalCount = 0 then
        turnsCount
    else
        simulate southMap m n (turnsCount + 1)

let solve input =
    let init,m,n = input |> parse
    let result = simulate init m n 1
    result.ToString()