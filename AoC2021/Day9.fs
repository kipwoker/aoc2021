module Day9

let inline charToInt c = int c - int '0'

let private parse (input: string[]) =
    input |> Array.map (fun x -> x.ToCharArray() |> Array.map charToInt)

let isLowPoint (a : int[][]) i j =
    (i = 0 || a.[i - 1].[j] > a.[i].[j]) &&
    (i = a.Length - 1 || a.[i + 1].[j] > a.[i].[j]) &&
    (j = 0 || a.[i].[j - 1] > a.[i].[j]) &&
    (j = a.[0].Length - 1 || a.[i].[j + 1] > a.[i].[j])

let solve1 (input: string[]) : string =
    let matrix = input
                 |> parse
    let result = matrix
                 |> Array.mapi (fun i row -> row |> Array.mapi (fun j el -> (isLowPoint matrix i j, el + 1)))
                 |> Array.collect id
                 |> Array.filter fst
                 |> Array.map snd
                 |> Array.sum
    result.ToString()

//part2
let rec search (a: int[][]) (i: int) (j: int) (visited: Set<(int * int)> ref) : int =
    let m = a.Length
    let n = a.[0].Length
    if i < 0 || i >= m || j < 0 || j >= n || a.[i].[j] = 9 || !visited |> Set.contains((i, j)) then
        0
    else
        visited := Set.add (i, j) !visited

        let right = search a (i + 1) j visited
        let left = search a (i - 1) j visited
        let up = search a i (j + 1) visited
        let down = search a i (j - 1) visited

        (up + down + left + right + 1)


let solve2 (input : string[]) : string =
    let matrix = input
                 |> parse
    let visited = ref Set.empty
    let result = matrix
                 |> Array.mapi (fun i row -> row
                                             |> Array.mapi (fun j _ -> search matrix i j visited)
                    )
                 |> Array.collect id
                 |> Array.sortDescending
                 |> Array.take 3
                 |> Array.fold (fun state el -> el * state) 1
    result.ToString()