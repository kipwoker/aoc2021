module Day24

open System.Numerics
open Core

type Command = {
    Add: int
    Offset: int
}

type Equation = {
    LeftIndex : int
    RightIndex : int
    RightAdd : int
}

let parse (input: string[]) =
    input
    |> Array.splitInto 14
    |> Array.map (fun chunk ->
        let add = chunk.[5].Split(' ').[2] |> int
        let offset = chunk.[15].Split(' ')[2] |> int
        { Add = add; Offset = offset }
    )
    |> List.ofArray

let add eq lst =
    match eq with
    | None -> lst
    | Some e -> lst @ [e]

let rec makeEquations (commands: Command list) (stack: (int * int) list) index equations =
    match commands with
    | [] -> equations
    | command::tail ->
        let stack', equation = if command.Add > 0 then
                                    ([(index, command.Offset)] @ stack, None)
                               else
                                    let pop::stack'' = stack
                                    let rightIndex, rightOffset = pop
                                    let equation = { LeftIndex = index; RightIndex = rightIndex; RightAdd = command.Add + rightOffset }
                                    (stack'', Some equation)
        let equations' = add equation equations
        makeEquations tail stack' (index + 1) equations'

let print eqs =
    eqs
    |> List.iter (fun eq -> printfn $"a[%i{eq.LeftIndex}] = a[%i{eq.RightIndex}] + %i{eq.RightAdd}")

let rec getCombinations (ranges : (Equation * (int * int) list) list) map =
    match ranges with
    | [] -> [map]
    | (eq, indexes)::tail ->
        indexes
        |> List.map (fun (left, right) ->
            let map' = map |> Map.add eq.LeftIndex left |> Map.add eq.RightIndex right
            let maps = getCombinations tail map'
            maps
        )
        |> List.collect id

let convertToNumber (map: Map<int,int>) : bigint =
    let digits = [0..map.Count-1]
                    |> List.fold (fun state idx -> state @ [(map |> Map.find idx) |> intToChar]) []
                    |> Array.ofList
    BigInteger.Parse(new string(digits))

let findDigits equations =
    let ranges = equations
                    |> List.map(fun eq ->
                        let possibleOptions = [1..9]
                                                |> List.map (fun l ->
                                                    [1..9]
                                                    |> List.map(fun r ->
                                                        if l = r + eq.RightAdd then (l, r) else (-1, -1)
                                                    )
                                                    |> List.filter (fun (l', r') -> l' <> -1 && r' <> -1)
                                                )
                                                |> List.collect id
                        (eq, possibleOptions)
                    )
    printfn $"%A{ranges}"

    let maps = getCombinations ranges Map.empty
    let numbers = maps |> List.map convertToNumber |> List.sort |> Array.ofList
    (numbers |> Array.head, numbers |> Array.last)

let solve input =
    let commands = input |> parse
    let equations = makeEquations commands [] 0 []
    print equations
    let result = findDigits equations
    result.ToString()