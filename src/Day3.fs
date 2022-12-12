module Day3

open System.IO
open Util.Extensions
open Util.Plumbing

let getPriority: seq<string> -> int =
    Seq.map Set.ofSeq
    >> Set.intersectMany
    >> Seq.head
    >> function
    | c when c >= 'a' && c <= 'z' -> int c - int 'a' + 1
    | c when c >= 'A' && c <= 'Z' -> int c - int 'A' + 27
    | c -> failwithf "Invalid character: %c" c

let parse = File.ReadAllLines >> Array.toSeq 

let splitSack (line: string) =
    let mid = line.Length / 2
    let sack1 = line.[..mid-1]
    let sack2 = line.[mid..]
    [sack1; sack2]

let solveSilver =
    parse
    >> Seq.map (splitSack >> getPriority)
    >> Seq.sum
    >> string

let solveGold =
    parse
    >> Seq.chunkBySize 3
    >> Seq.map getPriority
    >> Seq.sum
    >> string

let Solvers = simpleSolver solveSilver solveGold