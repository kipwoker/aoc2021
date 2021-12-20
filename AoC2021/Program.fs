open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input20.txt", Encoding.UTF8)
    let output = input |> Day20.solve
    printfn $"Output: %s{output}"
    0