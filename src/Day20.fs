module Day20

open System.IO
open Util.Extensions
open Util.Plumbing

[<Struct>]
type Number = { Value: int64; Order: int }

let parse =
    File.ReadAllLines
    >> Array.map int
    >> Array.mapi (fun i v -> { Value = v; Order = i})

let shiftNumbers (cipher: Number[]) =
    let modNum = cipher.LongLength - 1L

    for n in 0 .. cipher.Length - 1 do
        let src = Array.findIndex (fun v -> v.Order = n) cipher
        let num = cipher[src]

        // Figure out the destination; repetition of modulus accounts for multi-wrapping with large numbers
        let dst = ((int64 src + num.Value) % modNum + modNum) % modNum |> int

        // Shift other array nodes around to accomidate the number's new location
        if src < dst then
            for i in src .. dst - 1 do
                cipher[i] <- cipher[i + 1]
            cipher[dst] <- num
        elif src > dst then
            for i in src .. -1 .. dst + 1 do
                cipher[i] <- cipher[i - 1]
            cipher[dst] <- num
            
    cipher

let getCoords cipher =
    let zero = Array.findIndex (fun v -> v.Value = 0) cipher
    [1000; 2000; 3000]
    |> List.map (fun n -> cipher[(zero + n) % cipher.Length])
    |> List.map (fun n -> n.Value)
    |> List.sum

let solveSilver =
    shiftNumbers
    >> getCoords
    >> string

let solveGold =
    Array.map (fun { Value = v; Order = i } -> { Value = v * 811589153L; Order = i })
    >> fun n -> [1 .. 10] |> Seq.fold (fun acc _ -> shiftNumbers acc) n
    >> getCoords
    >> string

let withCopy fn = Array.copy >> fn

let Solver = chainSolver parse (withCopy solveSilver) (withCopy solveGold)
