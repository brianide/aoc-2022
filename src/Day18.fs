module Day18

open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

let parse file =
    File.ReadAllLines file
    |> Seq.map (fun n -> n.Split(',') |> Array.choose (|Int32|_|))
    |> Seq.map (function [|a; b; c|] -> (a, b, c) | _ -> failwith "Invalid input")
    |> Seq.toArray

let inline (.+) (a, b, c) (i, j, k) = (a + i, b + j, c + k)

let neighbors p =
    [|(1,0,0); (0,1,0); (0,0,1); (-1,0,0); (0,-1,0); (0,0,-1)|]
    |> Array.map (fun o -> p .+ o)

let solveSilver input =
    let points = Set.ofArray input
    
    points
    |> Seq.collect (neighbors >> Seq.filter (fun p -> not <| Set.contains p points))
    |> Seq.length
    |> string

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
