module Core

open System.Diagnostics

type TimedOperation<'T> = {
    ElapsedMilliseconds : int64
    Value : 'T
}

let timeOperation<'T> (func: unit -> 'T): TimedOperation<'T> =
    let timer = new Stopwatch()
    timer.Start()
    let value = func()
    timer.Stop()
    { ElapsedMilliseconds=timer.ElapsedMilliseconds; Value=value }

let inline charToInt c = int c - int '0'
let inline getSizes<'T> (a : 'T[][]) = (a.Length, a.[0].Length)

let inline toDecimal (bits: int[]) : int =
    let (_, total') = Array.foldBack (fun el (power, total) -> (power * 2, total + el * power)) bits (1, 0)
    total'

let inline toDecimal' (bits: char[]) : int =
    let bits' = bits |> Array.map charToInt
    let (_, total') = Array.foldBack (fun el (power, total) -> (power * 2, total + el * power)) bits' (1, 0)
    total'

let inline toBigDecimal (bits: char[]) : bigint =
    let bits' = bits |> Array.map charToInt
    let (_, total') = Array.foldBack (fun (el: int) (power : bigint, total : bigint) ->
                            let el' = bigint el
                            (power * (bigint 2), total + el' * power)
                      ) bits' (bigint 1, bigint 0)
    total'

type PriorityQueue<'T> (capacity : int, comparer: 'T -> 'T -> int) =
    let queue : 'T array = Array.zeroCreate (capacity + 1)
    let mutable N = 0

    let IsLess i j = comparer queue.[i] queue.[j] < 0
    let Swap i j =
        let temp = queue.[i]
        queue.[i] <- queue.[j]
        queue.[j] <- temp
        i

    let rec Swim k =
        match (k > 1 && IsLess (k/2) k) with
        | true -> Swap (k/2) k |> Swim;
        | false -> ()

    let rec Sink k =
        match 2*k <= N, 2*k with
        | true, j when j < N && IsLess j (j + 1) ->
            match IsLess k (j+1), j+1 with
            | true, x ->
                Swap k x |> ignore
                Sink x
            | false, _ -> ()
        | true, j ->
            Swap k j |> ignore
            Sink j
        | false, _ -> ()

    member x.IsEmpty with get() = N = 0
    member x.Size with get() = N

    member x.Insert (item:'T) =
        N <- N + 1
        queue.[N] <- item
        Swim N

    member x.RemoveHead() =
        let head = queue.[1]
        Swap 1 N |> ignore
        N <- N - 1
        queue.[N+1] <- Unchecked.defaultof<'T>
        Sink 1
        head