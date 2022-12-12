module Day8

open System.IO
open Util.Extensions
open Util.Plumbing

let parse file =
    let table =
        file
        |> File.ReadAllLines
        |> Array.map (Seq.map (fun n -> int n - int '0') >> Seq.toArray)

    let height = Array.length table
    let width = Array.length table[0]
    (table, width, height)

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

let solveSilver (table: int[][], width, height) =
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

let genCoords width height = seq {
    for y in 1 .. height - 2 do
        for x in 1 .. width - 2 do
            yield (x, y)
}

let genScenicLines width height x y = seq {
    yield [x + 1 .. width - 1] |> List.map (fun dx -> (dx, y))
    yield [x - 1 .. -1 .. 0] |> List.map (fun dx -> (dx, y))
    yield [y + 1 .. height - 1] |> List.map (fun dy -> (x, dy))
    yield [y - 1 .. -1 .. 0] |> List.map (fun dy -> (x, dy))
}

let solveGold (table: int[][], width, height) =
    let scoreLine cent line =
        line
        |> Seq.map (fun (x, y) -> table[y][x])
        |> Seq.tryFindIndex (fun n -> n >= cent)
        |> function
        | Some n -> n + 1
        | None -> Seq.length line

    let scoreTree (x, y) =
        let cent = table[y][x]

        genScenicLines width height x y
        |> Seq.map (scoreLine cent)

    genCoords width height
    |> Seq.map (scoreTree >> Seq.reduce (*))
    |> Seq.max
    |> string

let Solver = chainSolver parse solveSilver solveGold