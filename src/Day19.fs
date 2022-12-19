module Day19

open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

let parse file =
    File.ReadAllLines file
    []

let solveSilver input =
    ""

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
