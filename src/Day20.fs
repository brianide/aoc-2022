module Day20

open System.IO
open Util.Extensions
open Util.Plumbing

type Digit = { Value: int; Order: int; mutable Moved: bool }

let parse file =
    File.ReadAllLines file |> Array.map int

let solveSilver input =
    let cipher = input |> Array.mapi (fun i v -> { Value = v; Order = i; Moved = false })

    for n in 0 .. cipher.Length - 1 do

        let src = Array.findIndex (fun v -> v.Order = n) cipher
        let dig = cipher[src]

        let dst =
            // printfn "%A" dig.Value
            if dig.Value > 0 then
                (src + dig.Value) % (cipher.Length - 1)
            elif dig.Value < 0 then
                (src + dig.Value - 1 + cipher.Length * 2) % cipher.Length
            else
                src

        if src < dst then
            for i in src .. dst - 1 do
                cipher[i] <- cipher[i + 1]
            cipher[dst] <- dig
        elif src > dst then
            for i in src .. -1 .. dst + 1 do
                cipher[i] <- cipher[i - 1]
            cipher[dst] <- dig
        dig.Moved <- true

        // Array.map(fun v -> v.Value) cipher |> printfn "%A"

    let zero = Array.findIndex (fun v -> v.Value = 0) cipher
    [1000; 2000; 3000]
    |> List.map (fun n -> cipher[(zero + n) % cipher.Length])
    |> List.map (fun n -> n.Value)
    |> sprintf "%A" 

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
