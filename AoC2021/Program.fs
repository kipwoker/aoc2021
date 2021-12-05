open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input5.txt", Encoding.UTF8)
    let output = input |> Day5.solve1
    printfn $"Output: %s{output}"
    0