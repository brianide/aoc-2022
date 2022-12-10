module Day10

open System.IO
open Util.Extensions
open Util.Patterns

type Instruction =
| ADD of int
| NOP

let parse (file: string) =
    let parseLine line =
        match String.split " " line with
        | ["addx"; Int32 x] -> seq {NOP; ADD x}
        | ["noop"] -> Seq.singleton NOP
        | _ -> failwithf "Invalid instruction: %s" line

    File.ReadAllLines file |> Seq.collect parseLine

let solveSilver file =
    let folder (cycle, x, total) inst =
        let newX =
            match inst with
            | ADD n -> x + n
            | NOP -> x

        if (cycle - 20) % 40 = 0 then
            printfn "%A" x
            (cycle + 1, newX, total + x * cycle)
        else
            (cycle + 1, newX, total)

    parse file
    |> Seq.fold folder (1, 1, 0)
    |> string

let solveGold file =
    parse file
    |> ignore
    ""

let Solvers = (solveSilver, solveGold)