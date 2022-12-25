module Day25

open System.IO
open Util.Collections
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

let snafuToInt s =
    let charValue = function 
    | '=' -> -2L | '-' -> -1L | '0' -> 0L | '1' -> 1L | '2' -> 2L
    | c -> failwithf "Invalid digit: %c" c

    let rec recur place sum = function
    | [] -> sum
    | dig :: more -> recur (place * 5L) (sum + place * charValue dig) more
    
    recur 1 0 (Seq.rev s |> List.ofSeq)

let intToSnafu n =
    let chars = [| '0'; '1'; '2'; '='; '-' |]
    let rec recur digits value =
        if value > 0L then
            recur (chars[int (value % 5L)] :: digits) ((value + 2L) / 5L)
        else
            digits |> Array.ofList |> System.String.Concat

    if n = 0L then
        "0"
    else
        recur [] n

let parse file =
    File.ReadAllLines file
    |> Array.toList
    |> List.map (snafuToInt)

let solveSilver = List.sum >> intToSnafu

let Solver = chainSolver parse solveSilver solveSilver
