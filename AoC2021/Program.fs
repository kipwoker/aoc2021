open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input15.test.txt", Encoding.UTF8)
    let output = input |> Day15.solve2
    printfn $"Output: %s{output}"
    0