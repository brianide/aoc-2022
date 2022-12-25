module Day25

open System.IO
open Util.Extensions
open Util.Plumbing

let snafuToInt =
    let charValue = function 
    | '=' -> -2L | '-' -> -1L | '0' -> 0L | '1' -> 1L | '2' -> 2L
    | c -> failwithf "Invalid digit: %c" c

    let rec recur place sum = function
    | [] -> sum
    | dig :: more -> recur (place * 5L) (sum + place * charValue dig) more
    
    Seq.rev >> List.ofSeq >> recur 1 0

let intToSnafu =
    let chars = [| '0'; '1'; '2'; '='; '-' |]
    let rec recur digits = function
    | 0L -> digits |> Array.ofList |> System.String.Concat<char>
    | n -> recur (chars[int (n % 5L)] :: digits) ((n + 2L) / 5L)

    function 0L -> "0" | n -> recur [] n

let parse =
    File.ReadAllLines
    >> Array.toList
    >> List.map (snafuToInt)

let solveSilver = List.sum >> intToSnafu

let Solver = chainSolver parse solveSilver solveSilver
