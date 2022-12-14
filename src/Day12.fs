module Day12

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
            | 'S' -> start <- (r, c); 0
            | 'E' -> dest <- (r, c); 25
            | c -> int c - int 'a'

    { Grid = grid; Start = start; End = dest }

let getNeighbors (grid: int[,]) (r, c) =
    seq {(0, 1); (0, -1); (1, 0); (-1, 0)}
    |> Seq.map (fun (i, j) -> (r + i, c + j))
    |> Seq.filter (Array2D.isInside grid)

let calcDistances stepCb valid grid start =
    let mutable explored = Set.singleton start
    let mutable queue = Queue.singleton start
    let mutable dists = Map.ofSeq <| seq {(start, 0)}

    let mutable maxDist = -1

    while not (Queue.isEmpty queue) do
        let (r, c), nextQueue = Queue.dequeue queue
        queue <- nextQueue

        let dist = dists[(r, c)] + 1
        if dist > maxDist then
            maxDist <- dist
            stepCb explored grid

        getNeighbors grid (r, c)
        |> Seq.filter (fun (i, j) -> valid grid[r, c] grid[i, j])
        |> Seq.filter (fun p -> not <| Seq.contains p explored)
        |> Seq.iter (fun p ->
            explored <- Set.add p explored
            dists <- Map.add p dist dists
            queue <- Queue.enqueue p queue)

    Array2D.coordSeq grid
    |> Seq.filter (fun p -> not <| Map.containsKey p dists)
    |> Seq.iter (fun p -> dists <- Map.add p Int32.MaxValue dists)

    dists

let prepare file =
    let scen = parse file
    let dist = calcDistances (fun _ _ -> ()) (fun s d -> d - s >= -1) scen.Grid scen.End
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

let renderFrame =
    let mutable count = 0
    fun dir pref explored grid ->
        let fmt c r h =
            if Set.contains (r, c) explored then
                byte (h * 4 + 150)
            else
                byte (h * 2 + 68)

        // Image.saveToPGM fmt (sprintf "%s/%s%08d.pgm" dir pref count) grid
        count <- count + 1

let run infile outdir prefix _ =
    let scen = parse infile
    ignore <| calcDistances (renderFrame outdir prefix) (fun s d -> d - s >= -1) scen.Grid scen.End
    ""

let Renderer = renderer run