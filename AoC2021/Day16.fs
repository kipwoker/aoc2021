module Day16

open Core

type Node = {
    Version: int
    TypeId: int
    Length: int
    Value: bigint option
    Nodes: Node list
}

let hexToBin x =
   let s = match x with
           | '0' -> "0000"
           | '1' -> "0001"
           | '2' -> "0010"
           | '3' -> "0011"
           | '4' -> "0100"
           | '5' -> "0101"
           | '6' -> "0110"
           | '7' -> "0111"
           | '8' -> "1000"
           | '9' -> "1001"
           | 'A' -> "1010"
           | 'B' -> "1011"
           | 'C' -> "1100"
           | 'D' -> "1101"
           | 'E' -> "1110"
           | 'F' -> "1111"
           | _ -> "999999"
   s.ToCharArray()

let parse (input: string[]) =
    input |> Array.map(fun line -> line.ToCharArray() |> Array.map hexToBin |> Array.collect id)

let rec parseLiteralGroup (a : char[]) =
    let head = a |> Array.take 5
    let head' = head |> Array.skip 1
    match head.[0] with
    | '0' -> (head', 5)
    | _ ->
        let tail = a |> Array.skip 5
        let tail', length = parseLiteralGroup tail
        (Array.append head' tail', length + 5)

let parseLiteral (a : char[]) =
    let literal, length = parseLiteralGroup a
    (literal |> toBigDecimal, length)

let rec parseType1' (body: char[]) (nodes: Node list) (length: int) (count: int) d' : Node list * int =
    match count with
    | 0 -> (nodes, length)
    | _ ->
        let node = d' body
        let body' = body |> Array.skip node.Length
        let nodes' = nodes @ [node]
        let length' = length + node.Length
        parseType1' body' nodes' length' (count - 1) d'

let rec parseType0' (body: char[]) (nodes: Node list) (length: int) d' =
    match body.Length with
    | 0 -> (nodes, length)
    | _ ->
        let node = d' body
        let body' = body |> Array.skip node.Length
        let length' = length + node.Length
        let nodes' = nodes @ [node]
        parseType0' body' nodes' length' d'

let parseType0 (a: char[]) d' =
    let bitLength = 15
    let length = a |> Array.take bitLength |> toDecimal'
    let body = a |> Array.skip bitLength |> Array.take length
    let nodes = []
    parseType0' body nodes bitLength d'

let parseType1 (a: char[]) d' =
    let bitLength = 11
    let count = a |> Array.take bitLength |> toDecimal'
    let body = a |> Array.skip bitLength
    let nodes = []
    parseType1' body nodes bitLength count d'

let rec deserialize (a : char[]) : Node =
    let version = a |> Array.take 3 |> toDecimal'
    let typeId = a |> Array.skip 3 |> Array.take 3 |> toDecimal'

    match typeId with
    | 4 ->
        let body = a |> Array.skip 6
        let value, length = parseLiteral body
        { Version = version; TypeId = typeId; Nodes = []; Value = Some value; Length = 6 + length }
    | _ ->
        let lenId = a |> Array.skip 6 |> Array.head
        let body = a |> Array.skip 7
        let parse' = if lenId = '0' then parseType0 else parseType1
        let nodes, length = parse' body deserialize
        { Version = version; TypeId = typeId; Nodes = nodes; Value = None; Length = 7 + length }

let rec sumVersions (node: Node) : int =
    node.Version + (node.Nodes |> List.sumBy sumVersions)

let compare (nodes: Node list) c' comparer =
    let a = c' nodes.[0]
    let b = c' nodes.[1]
    bigint (if comparer a b then 1 else 0)

let rec calc (node: Node) : bigint =
    let nodes = node.Nodes
    match node.TypeId with
    | 4 -> node.Value.Value
    | 0 -> nodes |> List.sumBy calc
    | 1 -> nodes |> List.fold (fun state node -> state * (calc node)) (bigint 1)
    | 2 -> nodes |> List.map calc |> List.min
    | 3 -> nodes |> List.map calc |> List.max
    | 5 -> compare nodes calc (>)
    | 6 -> compare nodes calc (<)
    | 7 -> compare nodes calc (=)
    | _ ->
        printfn $"Unknown typeId %i{node.TypeId}"
        bigint -9000000

let solve input calculator =
    let result = input
                 |> parse
                 |> Array.map(fun i ->
                        let node = i |> deserialize
                        printfn $"%A{node}"
                        let result' = node |> calculator
                        printfn $"Result %A{result'}"
                        result'
                 ) |> Array.head
    result.ToString()

let solve1 input = solve input sumVersions
let solve2 input = solve input calc
