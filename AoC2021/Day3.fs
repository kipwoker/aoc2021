module Day3

type State = {
    Power: int
    Total: int
}

let private toDecimal (bits: int[]) : int =
    let state = Array.foldBack
                    (fun el state ->{Power = state.Power * 2; Total = state.Total + el * state.Power})
                    bits
                    {Power = 1; Total = 0}
    state.Total

let private print (bits: int[]) =
    bits |> Array.iter (fun x -> printf $"%i{x}, ")
    printfn "\n"

let private indexZip (a : int[][]) : int[] =
    let n = a.Length
    let mutable r = Array.init a.[0].Length (fun x -> 0)
    for i = 0 to n - 1 do
        for j = 0 to a.[i].Length - 1 do
            r.[j] <- r.[j] + a.[i].[j]
    r

let private parse (input: string[]) : int[][] =
    input |> Array. map (fun x -> x.ToCharArray() |> Array.map (fun y -> if y = '1' then 1 else 0))

let solve1 (input: string[]) : string =
    let n = input.Length
    let half = n / 2 + 1
    let counts = input |> parse |> indexZip
    let gamma = counts
                   |> Array.map (fun x -> if x > half then 1 else 0)

    let epsilon = gamma |> Array.map (fun x -> if x = 0 then 1 else 0)

    let gammaRate = gamma |> toDecimal
    let epsilonRate = epsilon |> toDecimal

    let result = gammaRate * epsilonRate
    result.ToString()

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
    let n = input.Length
    let m = input.[0].Length
    let data = input |> parse

    let oxygen = calculate data (fun (zeroes, ones) -> if ones.Length >= zeroes.Length then (1, ones) else (0, zeroes))
    let carbon = calculate data (fun (zeroes, ones) -> if zeroes.Length <= ones.Length then (0, zeroes) else (1, ones))

    let oxygenRate = oxygen |> toDecimal
    let carbonRate = carbon |> toDecimal

    let result = oxygenRate * carbonRate
    result.ToString()