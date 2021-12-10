module Day10

type State =
    | Valid
    | Invalid of char
    | Incomplete of char list

let inline charToInt c = int c - int '0'

let private parse (input: string[]) =
    input |> Array.map (fun x -> x.ToCharArray() |> List.ofArray)

let isMatched a b =
    match (a, b) with
    | ('[', ']') -> true
    | ('{', '}') -> true
    | ('(', ')') -> true
    | ('<', '>') -> true
    | _ -> false

let rec check (line: char list) (stack: char list) : State =
    match line with
    | [] ->
        match stack with
        | [] -> Valid
        | _ -> Incomplete stack
    | H :: T when H = '[' || H = '(' || H = '<' || H = '{' ->
        check T ([H] @ stack)
    | H :: T ->
        match stack with
        | [] -> Invalid H
        | HS :: TS ->
            match (isMatched HS H) with
            | true -> check T TS
            | false -> Invalid H

let getInvalidScore c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ ->
        printfn $"Error %c{c}"
        -1000000

let getIncompleteScore' c =
    match c with
    | '(' -> 1 |> bigint
    | '[' -> 2 |> bigint
    | '{' -> 3 |> bigint
    | '<' -> 4 |> bigint
    | _ ->
        printfn $"Error %c{c}"
        -1000000 |> bigint

let rec getIncompleteScore stack (score : bigint) =
    match stack with
    | [] -> score
    | H :: T ->
        let newScore = bigint.Multiply(score, 5 |> bigint) + (getIncompleteScore' H)
        getIncompleteScore T newScore

let solve1 (input: string[]) : string =
    let result = input
                 |> parse
                 |> Array.map (fun line -> check line [])
                 |> Array.map (fun state ->
                        match state with
                        | Invalid c -> getInvalidScore c
                        | _ -> 0
                    )
                 |> Array.sum
    result.ToString()

let solve2 (input: string[]) : string =
    let zero = 0 |> bigint
    let array = input
                |> parse
                |> Array.map (fun line -> check line [])
                |> Array.map(fun state ->
                       match state with
                       | Incomplete stack -> getIncompleteScore stack (0 |> bigint)
                       | _ -> zero
                   )
                |> Array.filter (fun x -> x > zero)
                |> Array.sort
    printfn $"%A{array}"
    let middle = array.[array.Length / 2]
    middle.ToString()