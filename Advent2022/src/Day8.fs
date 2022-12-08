module Day8

open System.IO
open Util.Extensions
open Util.Patterns

let parse (file: string) =
    []

let solveSilver file =
    parse file |> ignore
    ""

let solveGold file =
    parse file |> ignore
    ""

let Solvers = {| Gold = solveGold; Silver = solveSilver |}