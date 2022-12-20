module Day20

open System.IO
open Util.Extensions
open Util.Plumbing

[<Struct>]
type Digit = { Value: int; Order: int }

let parse =
    File.ReadAllLines
    >> Array.map int
    >> Array.mapi (fun i v -> { Value = v; Order = i})

let shiftNumbers (cipher: Digit[]) =
    let cipher = Array.copy cipher
    
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
    cipher

let getCoords cipher =
    let zero = Array.findIndex (fun v -> v.Value = 0) cipher
    [1000; 2000; 3000]
    |> List.map (fun n -> cipher[(zero + n) % cipher.Length])
    |> List.map (fun n -> n.Value)
    |> List.sum

let solveSilver input =
    shiftNumbers input
    |> getCoords
    |> string

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
