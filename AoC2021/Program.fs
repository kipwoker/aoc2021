open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input9.txt", Encoding.UTF8)
    let output = input |> Day9.solve2
    printfn $"Output: %s{output}"
    0