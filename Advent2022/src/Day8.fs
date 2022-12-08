module Day8

open System.IO
open Util.Extensions
open Util.Patterns

let parse (file: string) =
    file
    |> File.ReadAllLines
    |> Array.map (Seq.map (fun n -> int n - int '0') >> Seq.toArray)

let genLines width height = seq {
    for y in 1 .. height - 2 do
        let k = [0 .. width - 1] |> List.map (fun x -> (x, y))
        yield k
        yield List.rev k
    
    for x in 1 .. width - 2 do
        let k = [0 .. height - 1] |> List.map (fun y -> (x, y))
        yield k
        yield List.rev k
}

let solveSilver file =
    let table = parse file
    let height = Array.length table
    let width = Array.length table[0]

    let folder (maximum, visible) (x, y) = 
        let curr = table[y][x]
        let newMax = max maximum curr
        let newVis = if newMax > maximum then (x, y) :: visible else visible
        (newMax, newVis)        

    genLines width height
    |> Seq.collect (List.fold folder (-1, []) >> snd)
    |> Seq.distinct
    |> Seq.length
    |> (+) 4
    |> string

let solveGold file =
    parse file |> ignore
    ""

let Solvers = {| Gold = solveGold; Silver = solveSilver |}