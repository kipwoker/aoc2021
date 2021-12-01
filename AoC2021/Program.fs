open System.Text
open Day1
open System.IO

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input.txt", Encoding.UTF8)
    let output = input |> Day1.solve
    printfn $"Output: %s{output}"
    0