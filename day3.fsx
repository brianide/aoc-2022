#load "util.fsx"
open Util

let getPriority sacks =
    let itemPriority = function
    | c when c >= 'a' && c <= 'z' -> int c - int 'a' + 1
    | c when c >= 'A' && c <= 'Z' -> int c - int 'A' + 27
    | c -> failwithf "Invalid character: %c" c

    sacks
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Seq.item 0
    |> itemPriority

let splitSack (line: string) =
    let mid = line.Length / 2
    let sack1 = line.[..mid-1]
    let sack2 = line.[mid..]
    [sack1; sack2]

inputLines.Value
|> Seq.map (splitSack >> getPriority)
|> Seq.sum
|> printfn "Part 1: %i"

inputLines.Value
|> Seq.chunkBySize 3
|> Seq.map getPriority
|> Seq.sum
|> printfn "Part 2: %i"