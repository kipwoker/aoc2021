module Day8

open System

let private parse (input: string[]) =
    input
    |> Array.map (fun line -> line.Split([|" | "|], System.StringSplitOptions.RemoveEmptyEntries).[1].Split(' '))

let countUniques (input: string[]) =
    input |> Array.map (fun x -> x.ToCharArray() |> Array.distinct |> Array.sumBy (fun _ -> 1))

let count1478 (input: int[]) =
    input |> Array.fold
            (fun state el ->
                state + match el with
                        | 2 | 3 | 4 | 7 -> 1
                        | _ -> 0
                )
            0

let solve1 (input: string[]) : string =
    let result = input |> parse |> Array.collect id |> countUniques |> count1478
    result.ToString()



//Part 2

type Puzzle = {
    Sample: string[]
    Question: string[]
}

let private parse' (input: string[]) =
    input
    |> Array.map (fun line ->
        let parts = line.Split([|" | "|], System.StringSplitOptions.RemoveEmptyEntries)
        {
          Sample = parts.[0].Split(' ')
          Question = parts.[1].Split(' ')
        }
    )

let log a =
    printfn $"%A{a}"
    a


let single set =
    match set |> Set.count with
    | 1 -> set |> Set.minElement
    | _ ->
        printfn $"Expected single element but there are some %A{set}"
        Char.MinValue

// 1111
//2    6
//2    6
// 7777
//3    5
//3    5
// 4444
let mapLetters (sample : string[]) : Map<char, int> =
    let m = sample |> Array.groupBy (fun x -> x.Length) |> Map.ofArray
    let one = (m |> Map.find 2).[0].ToCharArray() |> Set.ofArray
    let seven = (m |> Map.find 3).[0].ToCharArray() |> Set.ofArray
    let four = (m |> Map.find 4).[0].ToCharArray() |> Set.ofArray
    let twoThreeFive = (m |> Map.find 5) |> Array.map (fun x -> x.ToCharArray() |> Set.ofArray)
    let sixNineZero = (m |> Map.find 6) |> Array.map (fun x -> x.ToCharArray() |> Set.ofArray)
    let eight = (m |> Map.find 7).[0].ToCharArray() |> Set.ofArray

    let axis1 = Set.difference seven one |> single
    let axis4 = sixNineZero |> Array.map (fun s -> Set.difference s four |> Set.remove axis1) |> Set.intersectMany |> single
    let axis3 =  Set.difference eight four
                |> Set.remove axis1
                |> Set.remove axis4
                |> single
    let axis2or7 = Set.difference four one
    let five = twoThreeFive
                |> Array.find (fun s -> Set.difference s axis2or7 |> Set.count = 3)
    let axis5 = Set.difference five axis2or7
                |> Set.remove axis1
                |> Set.remove axis4
                |> single

    let axis6 = one |> Set.remove axis5 |> single
    let axis7 = twoThreeFive |> Array.filter (fun s -> Set.difference s five |> Set.count > 0) |> Array.head
                |> Set.remove axis1
                |> Set.remove axis3
                |> Set.remove axis4
                |> Set.remove axis5
                |> Set.remove axis6
                |> single
    let axis2 = four
                |> Set.remove axis5
                |> Set.remove axis6
                |> Set.remove axis7
                |> single
    [|
        (axis1 , 1)
        (axis2 , 2)
        (axis3 , 3)
        (axis4 , 4)
        (axis5 , 5)
        (axis6 , 6)
        (axis7 , 7)
    |] |> Map.ofArray

let matchDigit (input: int[]) =
    let pattern = input |> Array.sort |> Array.map(fun x -> x.ToString()) |> String.concat ""
    match pattern with
    | "123456" -> 0
    | "56" -> 1
    | "13467" -> 2
    | "14567" -> 3
    | "2567" -> 4
    | "12457" -> 5
    | "123457" -> 6
    | "156" -> 7
    | "1234567" -> 8
    | "124567" -> 9
    | _ ->
        printfn $"Pattern doesn't match %s{pattern}"
        -2000000

let answer (question: string[]) (map: Map<char, int>) =
    let digits = question
                 |> Seq.map (fun q ->
                        let digit = q.ToCharArray() |> Array.map (fun c -> map.[c]) |> matchDigit
                        digit.ToString()
                    )
    digits |> String.concat "" |> int

let solvePuzzle (puzzle: Puzzle) : int =
    let map = puzzle.Sample |> mapLetters
    answer puzzle.Question map

let solve2 (input: string[]) : string =
    let result = input
                 |> parse'
                 |> Array.fold (fun state puzzle -> (solvePuzzle puzzle) + state) 0
    result.ToString()