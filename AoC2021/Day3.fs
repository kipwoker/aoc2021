module Day3

//Common
open Core

let private parse (input: string[]) : int[][] =
    input |> Array. map (fun x -> x.ToCharArray() |> Array.map (fun y -> if y = '1' then 1 else 0))

//Part1
let private sumByColumn (a : int[][]) : int[] =
    let n = a.Length
    let m = a.[0].Length
    let mutable r = Array.init m (fun _ -> 0)
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            r.[j] <- r.[j] + a.[i].[j]
    r

let private invert = Array.map (fun x -> if x = 0 then 1 else 0)

let solve1 (input: string[]) : string =
    let n = input.Length
    let half = n / 2 + 1
    let counts = input |> parse |> sumByColumn |> Array.map (fun x -> if x > half then 1 else 0)
    let gamma = counts |> toDecimal
    let epsilon = counts |> invert |> toDecimal

    let result = gamma * epsilon
    result.ToString()

//Part2
let private calculate (data: int[][]) (choose: int[] * int[] -> int * int[]) =
    let n = data.Length
    let m = data.[0].Length
    let mutable indexes = Array.ofSeq [ 0 .. n - 1 ]
    let result = Array.init m (fun _ -> 0)

    for j = 0 to m - 1 do
        if indexes.Length = 1 then
            Array.set result j data.[indexes.[0]].[j]
        else
            let (bit, newIndexes) = indexes
                                    |> Array.partition (fun i -> data.[i].[j] = 0)
                                    |> choose
            Array.set result j bit
            indexes <- newIndexes
    result

let solve2 (input: string[]) : string =
    let data = input |> parse

    let oxygen = calculate data (fun (zeroes, ones) -> if ones.Length >= zeroes.Length then (1, ones) else (0, zeroes)) |> toDecimal
    let carbon = calculate data (fun (zeroes, ones) -> if ones.Length >= zeroes.Length then (0, zeroes) else (1, ones)) |> toDecimal

    let result = oxygen * carbon
    result.ToString()