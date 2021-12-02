open System.Text
open Day2
open System.IO

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input2.txt", Encoding.UTF8)
    let output = input |> Day2.solve2
    printfn $"Output: %s{output}"
    0