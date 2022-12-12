module Day11

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

type Monkey = {
    mutable Items: int64 list
    Operation: int64 -> int64
    Divisor: int64
    Routing: int * int
}

let parse file =
    let parseMonkey (lines: string[]) =
        let op =
            let (|Operator|_|) = function
                | "+" -> Some (+)
                | "*" -> Some (*)
                | _ -> None
            let (|Operand|_|) = function
                | "old" -> Some <| Operand id
                | Int64 n -> Some <| Operand (fun _ -> n)
                | _ -> None
            let (|Operation|) = function
                | [Operand a; Operator f; Operand b] -> fun x -> f (a x) (b x)
                | x -> failwithf "Invalid expression: %A" x

            lines[2][19 ..] |> String.split " " |> (|Operation|)

        let items = lines[1][18 ..] |> String.split ", " |> Seq.map int64 |> Seq.toList
        let divisor = lines[3][21 ..] |> int
        let targetTrue = lines[4][29 ..] |> int
        let targetFalse = lines[5][30 ..] |> int

        { Items = items; Operation = op; Divisor = divisor; Routing = (targetTrue, targetFalse)}

    File.ReadAllLines file
    |> Seq.chunkBySize 7
    |> Seq.map parseMonkey
    |> Seq.toArray

let solve sanity rounds (monkeys: Monkey[]) =
    let counts = Array.zeroCreate<int64> monkeys.Length
    let supermod = monkeys |> Seq.map (fun n -> n.Divisor) |> Seq.reduce (*)

    let tickMonkey n =
        let mon = monkeys[n]
        let (routeA, routeB) = mon.Routing

        let throw item =
            let newVal = mon.Operation item / sanity % supermod
            let dest = if newVal % mon.Divisor = 0 then routeA else routeB
            monkeys[dest].Items <- newVal :: monkeys[dest].Items
            counts[n] <- counts[n] + 1L

        Seq.iter throw mon.Items
        mon.Items <- []

    for _ in 1 .. rounds do
        for i in 0 .. monkeys.Length - 1 do
            tickMonkey i
    
    Seq.sortDescending counts |> Seq.take 2 |> Seq.reduce (*) |> string

let Solver = chainSolver parse (solve 3 20) (solve 1 10000)