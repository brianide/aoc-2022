module Day10

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let Dims = {|Width = 40; Height = 6|}

let parse file =
    let parseLine line =
        match String.split " " line with
        | ["addx"; Int32 x] -> [0; x]
        | ["noop"] -> [0]
        | _ -> failwithf "Invalid instruction: %s" line

    File.ReadAllLines file |> Seq.collect parseLine

let runProgram =
    Seq.scan (fun (cycle, x) n -> (cycle + 1, x + n)) (1, 1)

let solveSilver =
    Seq.filter (fun (cycle, _) -> (cycle - 20) % 40 = 0)
    >> Seq.sumBy (fun (cycle, x) -> cycle * x)
    >> string

let solveGold =
    let format i (_, x) =
        if abs (i % Dims.Width - x) <= 1 then "#" else " "
        
    Seq.mapi format
    >> Seq.take (Dims.Width * Dims.Height)
    >> Seq.chunkBySize Dims.Width
    >> Seq.map (String.concat "")
    >> String.concat "\n"

let Solver = chainSolver (parse >> runProgram) solveSilver solveGold