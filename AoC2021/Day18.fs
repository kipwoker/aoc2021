module Day18

open Core

type Priority = Left | Right

type Tree =
    | Leaf of int
    | Node of Tree * Tree

type Explode = int * int * Tree

type Step =
    | Explode of Explode
    | NoChange of Tree
    | Changed of Tree
    | Split of Tree

let rec parseTree (line : char[]) =
    match (line |> Array.head, line |> Array.last) with
    | '[', ']' ->
        let body = Array.sub line 1 (line.Length - 2)
        let idx, _ = body
                     |> Array.indexed
                     |> Array.fold (fun (idx, bracketCounter) (i, c) ->
                            match (idx, c) with
                            | -1, '[' -> (idx, bracketCounter + 1)
                            | -1, ']' -> (idx, bracketCounter - 1)
                            | -1, ',' when bracketCounter = 0 -> (i, bracketCounter)
                            | _ -> (idx, bracketCounter)
                        ) (-1, 0)
        let left, right' = body |> Array.splitAt idx
        let right = right' |> Array.skip 1
        Node (parseTree left, parseTree right)
    | _ -> Leaf (new string(line) |> int)

let parse (input: string[]) = input |> Array.map (fun line -> parseTree (line.ToCharArray()))

let rec add (root: Tree) value priority =
    match root with
    | Leaf leaf -> (true, Leaf (leaf + value))
    | Node (left, right) ->
        let node(first, second) = if priority = Left then Node (first, second) else Node (second, first)
        let first, second = if priority = Left then (left, right) else (right, left)
        let added, first' = add first value priority
        if added then
            (added, node (first', second))
        else
            let added', second' = add second value priority
            (added', node (first, second'))

let split' value =
    let left = value / 2
    let right = value - left
    Node (Leaf left, Leaf right)

let rec split (tree: Tree) : bool * Tree =
    match tree with
    | Node (Leaf left, right) when left >= 10 -> (true, Node (split' left, right))
    | Node (left, Leaf right) when right >= 10 -> (true, Node (left, split' right))
    | Node (left, right) ->
        let sl,left' = split left
        match sl with
        | false ->
            let sr, right' = split right
            match sr with
            | false -> (false, tree)
            | true -> (true, Node(left', right'))
        | true -> (true, Node(left', right))
    | Leaf _ -> (false, tree)

let rec explode (tree: Tree) (depth: int) : Explode option =
    match tree with
    | Node (Leaf left, Leaf right) when depth >= 5 ->
        let result = (left, right, Leaf 0) |> Some
        result
    | Leaf _ ->
        None
    | Node (left, right) ->
        let explode' = explode left (depth + 1)
        match explode' with
        | Some (el, er, left') ->
            let right' = if er <> 0 then add right er Left |> snd else right
            Some (el, 0, Node (left', right'))
        | None ->
            let explode'' = explode right (depth + 1)
            match explode'' with
            | Some (el, er, right') ->
                let left' = if el <> 0 then add left el Right |> snd else left
                Some (0, er, Node (left', right'))
            | None -> None

let rec print' (tree: Tree) =
    match tree with
    | Leaf tree' -> tree'.ToString()
    | Node(left, right) -> "[" + print' left + ", " + print' right + "]"
let print (tree: Tree) =
    printfn $"%s{print' tree}"

let rec reduceMany (tree: Tree) : Tree =
    let tree' = explode tree 1
    match tree' with
    | None ->
        let splinted, tree'' = split tree
        if splinted then
            reduceMany tree''
        else
            tree''
    | Some (_, _, t') ->
        reduceMany t'


let sum (left: Tree) (right: Tree) : Tree =
    let tree = Node (left, right)
    reduceMany tree

let rec calcMagnitude (tree: Tree) : int =
    match tree with
    | Leaf leaf -> leaf
    | Node(Leaf left, Leaf right) -> left * 3 + right * 2
    | Node(left, right) -> (calcMagnitude left) * 3 + (calcMagnitude right) * 2

let solve1 input =
    let trees = input |> parse
    let first = trees |> Array.head
    let tree = trees |> Array.skip 1 |> Array.fold sum first
    let result = tree |> calcMagnitude
    result.ToString()

let solve2 input =
    let trees = input |> parse
    let result = trees |> Array.mapi (fun i first ->
                                           [|i + 1 .. trees.Length - 1|]
                                           |> Array.map (fun j ->
                                               let second = trees.[j]
                                               max ((sum first second) |> calcMagnitude) ((sum second first) |> calcMagnitude)
                                               ))
                                           |> Array.collect id
                                           |> Array.max
    result.ToString()