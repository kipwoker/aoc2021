open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input21.test.txt", Encoding.UTF8)
    let output = input |> Day21.solve
    printfn $"Output: %s{output}"
    0