module Day4

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let checkContained ((a, b), (x, y)) =
    a <= x && b >= y || x <= a && y >= b

let checkOverlap ((a, b), (x, y)) =
    a <= y && x <= b

let parse =
    File.ReadAllLines
    >> Array.toSeq
    >> Seq.map (String.regGroups @"^(\d+)-(\d+),(\d+)-(\d+)$")
    >> Seq.map (function
    | [Int64 a; Int64 b; Int64 x; Int64 y] -> (a, b), (x, y)
    | g -> failwithf "Invalid input line: %A" g)

let solveSilver =
    Seq.filter checkContained
    >> Seq.length
    >> string

let solveGold =
    Seq.filter checkOverlap
    >> Seq.length
    >> string

let Solver = chainSolver parse solveSilver solveGold