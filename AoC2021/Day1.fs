module Day1
let solve1 (input: string[]) : string =
    let numbers = input |> Array.map (fun x -> x |> int)
    let mutable result = 0
    for i = 1 to numbers.Length - 1 do
        if numbers.[i] > numbers.[i - 1] then result <- result + 1
    result.ToString()

let solve2 (input: string[]) : string =
    let numbers = input |> Array.map (fun x -> x |> int)
    let mutable sum = numbers.[0] + numbers.[1] + numbers.[2]
    let mutable result = 0
    for i = 3 to numbers.Length - 1 do
        let newSum = sum - numbers.[i - 3] + numbers.[i]
        if newSum > sum then result <- result + 1
        sum <- newSum
    result.ToString()