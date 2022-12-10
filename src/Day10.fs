module Day10

open System.Collections
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
    let mutable pos = 1
    let mutable cycle = 1
    let display = BitArray(40 * 6)

    let tick inst =
        let crt = cycle - 1

        display[crt] <- abs (crt % 40 - pos) <= 1

        match inst with
        | ADD n -> pos <- pos + n
        | _ -> ()

        cycle <- cycle + 1

    parse file
    |> Seq.iter tick

    for i in 0 .. 5 do
        for j in 0 .. 39 do
            printf "%c" <| if display[i * 40 + j] then '#' else ' '
        printfn ""
    ""

let Solvers = (solveSilver, solveGold)