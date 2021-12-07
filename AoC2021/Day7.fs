module Day7

let private parse (input: string[]) =
    input.[0].Split(',')
    |> Array.map (fun x -> x |> int)
    |> Array.sort

let private findMiddle (a : int[]) = a.[a.Length / 2]
let private findAvg (a : int[]) =
    let total = float (a |> Array.sum)
    let l = float a.Length
    let avg = round (total / l) |> int
    avg

let private calc1 (a : int[]) (norm: int) =
    printfn $"%i{norm}"
    a |> Array.fold (fun state el -> abs (el - norm) + state) 0

let private calc2 (a : int[]) (norm: int) =
    printfn $"%i{norm}"
    a |> Array.fold (fun state el ->
        let diff = abs (el - norm)
        let progression = diff * (diff + 1) / 2
        progression + state) 0

let solve1 (input: string[]) : string =
    let origin = input |> parse
    let norm = origin |> findMiddle
    let result = calc1 origin norm
    result.ToString()

let solve2 (input: string[]) : string =
    let origin = input |> parse
    let norm = origin |> findAvg
    let result = min (calc2 origin norm) (calc2 origin (norm - 1))
    result.ToString()