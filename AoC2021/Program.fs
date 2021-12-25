open System.IO
open System.Text
open Core

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("input25.txt", Encoding.UTF8)
    let output = timeOperation(fun () -> input |> Day25.solve)
    printfn $"Output: %A{output}"
    0