module Day21

open System.IO
open Util.Collections
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

type Instruction =
| Constant of int64
| Add of string * string
| Subtract of string * string
| Multiply of string * string
| Divide of string * string

let parse file =
    let (|Operation|) = function
    | [reg1; "+"; reg2; ""] -> Add (reg1, reg2)
    | [reg1; "-"; reg2; ""] -> Subtract (reg1, reg2)
    | [reg1; "*"; reg2; ""] -> Multiply (reg1, reg2)
    | [reg1; "/"; reg2; ""] -> Divide (reg1, reg2)
    | x -> failwithf "Invalid input: %A" x

    let parseLine line =
        match String.regGroups @"(\w{4}): (?:(\w{4}) ([*/+-]) (\w{4})|(\d+))" line with
        | [label; ""; ""; ""; Int64 value] -> label, Constant value
        | label :: Operation op -> label, op
        | _ -> failwith "Invalid input"

    File.ReadAllLines file
    |> Array.map parseLine
    |> Map.ofArray

let rec evaluate (tab: Map<string, Instruction>) key =
    match tab[key] with
    | Constant n -> n
    | Add (a, b) -> (evaluate tab a) + (evaluate tab b)
    | Subtract (a, b) -> (evaluate tab a) - (evaluate tab b)
    | Multiply (a, b) -> (evaluate tab a) * (evaluate tab b)
    | Divide (a, b) -> (evaluate tab a) / (evaluate tab b)

let solveSilver input =
    evaluate input "root" |> string

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
