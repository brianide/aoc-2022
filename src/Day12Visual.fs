module Day12Visual

open System
open System.IO
open Util.Extensions
open Util.Collections
open Util.Plumbing

type Scenario = {
    Grid: int[,]
    Start: int * int
    End: int * int
}

let parse file =
    let lines = File.ReadAllLines file
    let (height, width) = (lines.Length, lines[0].Length)
    
    let mutable (start, dest) = ((0, 0), (0, 0))
    let grid = Array2D.init height width <| fun r c ->
        match lines[r][c] with
            | 'S' -> start <- (r, c); int 'a'
            | 'E' -> dest <- (r, c); int 'z'
            | c -> int c

    { Grid = grid; Start = start; End = dest }

let getNeighbors (grid: int[,]) (r, c) =
    seq {(0, 1); (0, -1); (1, 0); (-1, 0)}
    |> Seq.map (fun (i, j) -> (r + i, c + j))
    |> Seq.filter (Array2D.isInside grid)

let calcDistances valid (grid: int[,]) start dest =
    let mutable explored = Set.singleton start
    let mutable queue = Queue.singleton start
    let mutable dists = Map.ofSeq <| seq {(start, 0)}

    let mutable step = 0

    Console.CursorVisible <- false
    Console.Clear()
    let oldColor = Console.ForegroundColor

    Console.ForegroundColor <- ConsoleColor.DarkGray
    for r in 0 .. Array2D.length1 grid - 1 do
        grid[r, *] |> Seq.iter (fun v -> printf "%c" (char v))
        printfn ""
    Console.ForegroundColor <- ConsoleColor.White

    let draw (r, c) d =
        Console.SetCursorPosition(c, r)
        Console.Write(if d >= 0 then (char d) else char grid[r, c])

    while not (Queue.isEmpty queue) do
        let (r, c), nextQueue = Queue.dequeue queue
        queue <- nextQueue

        getNeighbors grid (r, c)
        |> Seq.filter (fun (i, j) -> valid grid[r, c] grid[i, j])
        |> Seq.filter (fun p -> not <| Seq.contains p explored)
        |> Seq.iter (fun p ->
            explored <- Set.add p explored
            draw p -1
            let dist = dists[(r, c)] + 1
            dists <- Map.add p dist dists
            queue <- Queue.enqueue p queue)

        Threading.Thread.Sleep 2

    Array2D.coordSeq grid
    |> Seq.filter (fun p -> not <| Map.containsKey p dists)
    |> Seq.iter (fun p -> dists <- Map.add p Int32.MaxValue dists)

    Console.ForegroundColor <- ConsoleColor.DarkRed
    let mutable pos = dest
    while pos <> start do
        draw pos -1
        Threading.Thread.Sleep 30

        pos <-
        getNeighbors grid pos
        |> Seq.map (fun (r, c) -> (r, c), dists[r, c])
        |> Seq.filter (fun ((r, c), _) -> let (i, j) = pos in valid grid[r, c] grid[i, j])
        |> Seq.minBy snd
        |> fst

    Console.SetCursorPosition(0, Array2D.length1 grid)
    Console.ForegroundColor <- oldColor
    Console.CursorVisible <- true

    dists

let prepare file =
    let scen = parse file
    let dist = calcDistances (fun s d -> d - s >= -1) scen.Grid scen.End scen.Start
    (scen, dist)

let solveSilver (scen, dist) =
    Map.find scen.Start dist
    |> string

let solveGold (scen, dist) =
    Array2D.coordSeq scen.Grid
    |> Seq.filter (fun (r, c) -> scen.Grid[r, c] = 0)
    |> Seq.map (fun p -> Map.find p dist)
    |> Seq.min
    |> string

let Solver = chainSolver prepare solveSilver solveGold
