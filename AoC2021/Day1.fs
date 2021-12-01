module Day1
let solve (input: string[]) : string =
    let numbers = input |> Array.map (fun x -> x |> int)
    let mutable result = 0
    for i = 1 to numbers.Length - 1 do
        if numbers.[i] > numbers.[i - 1] then result <- result + 1
    result.ToString()