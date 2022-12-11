module Day11

open System.IO
open Util.Extensions
open Util.Patterns

type OpValue = Prev | Lit of int64

type OpProc =
| Add of OpValue * OpValue
| Mul of OpValue * OpValue

type Monkey = {
    mutable Items: int64 list
    Operation: OpProc
    Routing: int64 * int * int
}

let parse (file: string) =
    let parseMonkey (lines: string[]) =
        let (|OpValue|) = function
        | "old" -> Prev
        | Int32 n -> Lit n
        | _ -> failwith "Invalid operand"

        //let id = String.regGroups @"Monkey (\d+):" lines[0] |> List.head
        let items = lines[1][18 ..] |> String.split ", " |> Seq.map int64 |> Seq.rev |> Seq.toList
        let inst = lines[2][19 ..] |> String.split " " |> function
            | [OpValue a; "+"; OpValue b] -> Add (a, b)
            | [OpValue a; "*"; OpValue b] -> Mul (a, b)
            | x -> failwithf "Invalid instruction: %A" x
        let divisor = lines[3][21 ..] |> int
        let targetTrue = lines[4][29 ..] |> int
        let targetFalse = lines[5][30 ..] |> int

        { Items = items; Operation = inst; Routing = (divisor, targetTrue, targetFalse)}

    File.ReadAllLines file
    |> Seq.chunkBySize 7
    |> Seq.map parseMonkey
    |> Seq.toArray

let solve sanity rounds file =
    let monkeys = parse file
    let counts = Array.zeroCreate<int> monkeys.Length

    let tickMonkey n =
        let mon = monkeys[n]
        let (div, routeA, routeB) = mon.Routing

        let foo x = function Lit n -> n | Prev -> x
        let bar n =
            match mon.Operation with
            | Add (a, b) -> foo n a + foo n b
            | Mul (a, b) -> foo n a * foo n b

        let throw item =
            let newVal = bar item / sanity
            let dest = if newVal % div = 0 then routeA else routeB
            monkeys[dest].Items <- newVal :: monkeys[dest].Items
            counts[n] <- counts[n] + 1

        Seq.rev mon.Items |> Seq.iter throw
        mon.Items <- []


    for _ in 1 .. rounds do
        for i in 0 .. monkeys.Length - 1 do
            tickMonkey i
    
    Seq.sortDescending counts |> Seq.take 2 |> Seq.reduce (*) |> string

let solveGold file =
    parse file
    |> ignore
    ""

let Solvers = (solve 3 20, solve 1 10000)