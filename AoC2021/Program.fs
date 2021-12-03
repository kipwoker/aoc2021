open System.Text
open Day2
open System.IO

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input3.txt", Encoding.UTF8)
    let output = input |> Day3.solve2
    printfn $"Output: %s{output}"
    0