module Day10Mutable

open System.Collections
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

let runProgram prog =
    let mutable cycle = 1
    let mutable x = 1
    let mutable sigTotal = 0
    let display = BitArray(Dims.Width * Dims.Height)

    let tick n =
        if (cycle - 20) % 40 = 0 then
            sigTotal <- sigTotal + x * cycle

        let crt = cycle - 1
        display[crt] <- abs (crt % Dims.Width - x) <= 1

        x <- x + n
        cycle <- cycle + 1

    Seq.iter tick prog
    (sigTotal, display)


let solveSilver =
    runProgram
    >> fst
    >> string

let solveGold =
    let format (arr: BitArray) =
        [0 .. arr.Count - 1]
        |> Seq.map (fun n -> if arr[n] then "#" else " ")
        |> Seq.chunkBySize Dims.Width
        |> Seq.map (String.concat "")
        |> String.concat "\n"

    runProgram
    >> snd
    >> format

let Solver = chainSolver parse (solveSilver) (solveGold)