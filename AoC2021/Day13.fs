module Day13

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Axis = X | Y

type Fold = {
    Axis : Axis
    Number : int
}

let print (matrix :char[][]) =
    for i = 0 to matrix.Length - 1 do
        for j = 0 to matrix.[0].Length - 1 do
            printf $"%c{matrix.[i].[j]} "
        printfn ""
    printfn ""

let parseAxis axis =
    match axis with
    | "x" -> X
    | _ -> Y

let parse (input: string[]) =
    let points = input
                 |> Array.takeWhile (fun line -> line <> "")
                 |> Array.map (fun line ->
                      let parts = line.Split(',')
                      (parts.[1] |> int, parts[0] |> int)
                    )
                 |> Set.ofArray
    let folds = input
                |> Array.skipWhile (fun line -> line <> "")
                |> Array.skip 1
                |> Array.map (fun line ->
                    let parts = line.Split(' ')
                    let command = parts.[2].Split('=')
                    {
                        Axis = command.[0] |> parseAxis
                        Number = command.[1] |> int
                    }
                   )
                |> List.ofArray

    let maxX = points |> Set.map fst |> Set.maxElement
    let maxY = points |> Set.map snd |> Set.maxElement
    let m = maxX + 1
    let n = maxY + 1
    let matrix = Array.init m (fun i -> Array.init n (fun j -> if points.Contains((i, j)) then '#' else '.'))

    (
      matrix,
      folds
    )

let zip (a : char[][]) (b : char[][]) =
    let min' = min a.Length b.Length
    let prefix = Array.zip (a |> Array.take min') (b |> Array.take min')
                 |> Array.map (fun (x,y) ->
                         Array.zip x y
                         |> Array.map (fun (ex,ey) ->
                             match (ex,ey) with
                             | ('#', _) | (_, '#') -> '#'
                             | _ -> '.'
                         )
                     )
                 |> List.ofArray
    let suffix = (a |> Array.skip min' |> List.ofArray) @ (b |> Array.skip min' |> List.ofArray)
    (prefix @ suffix) |> Array.ofList

let overlap (a,b) = (zip (a |> Array.rev) (b |> Array.skip 1)) |> Array.rev
let split matrix fold = matrix |> Array.splitAt fold.Number |> overlap

let apply matrix fold =
    match fold.Axis with
    | Y -> split matrix fold
    | X -> (split (matrix |> Array.transpose) fold) |> Array.transpose

let applyFolds matrix folds = folds |> List.fold (apply) matrix

let countHashes matrix =
    matrix |> Array.sumBy (fun row -> row |> Array.sumBy (fun el -> if el = '#' then 1 else 0))

let solve1 input =
    let matrix, folds = input |> parse
    let matrix' = applyFolds matrix (folds |> List.take 1)
    let result = matrix' |> countHashes
    result.ToString()

let solve2 input =
    let matrix, folds = input |> parse
    let matrix' = applyFolds matrix folds
    print matrix'
    ""