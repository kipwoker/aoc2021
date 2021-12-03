module Day3

//Common
let private toDecimal (bits: int[]) : int =
    let (_, total') = Array.foldBack (fun el (power, total) -> (power * 2, total + el * power)) bits (1, 0)
    total'

let private parse (input: string[]) : int[][] =
    input |> Array. map (fun x -> x.ToCharArray() |> Array.map (fun y -> if y = '1' then 1 else 0))

//Part1
let private indexZip (a : int[][]) : int[] =
    let n = a.Length
    let mutable r = Array.init a.[0].Length (fun x -> 0)
    for i = 0 to n - 1 do
        for j = 0 to a.[i].Length - 1 do
            r.[j] <- r.[j] + a.[i].[j]
    r

let solve1 (input: string[]) : string =
    let n = input.Length
    let half = n / 2 + 1
    let counts = input |> parse |> indexZip |> Array.map (fun x -> if x > half then 1 else 0)
    let gamma = counts |> toDecimal
    let epsilon = counts |> Array.map (fun x -> if x = 0 then 1 else 0) |> toDecimal

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
    let carbon = calculate data (fun (zeroes, ones) -> if zeroes.Length <= ones.Length then (0, zeroes) else (1, ones)) |> toDecimal

    let result = oxygen * carbon
    result.ToString()