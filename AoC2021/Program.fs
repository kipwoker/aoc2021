open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input19.txt", Encoding.UTF8)
    let output = input |> Day19.solve
    printfn $"Output: %s{output}"
    0