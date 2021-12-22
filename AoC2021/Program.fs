open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input22.test2.txt", Encoding.UTF8)
    let output = input |> Day22.solve
    printfn $"Output: %s{output}"
    0