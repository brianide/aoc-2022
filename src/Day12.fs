module Day12

open System.IO
open Util.Extensions
open Util.Collections

type Scenario = {
    Grid: int[,]
    Start: int * int
    End: int * int
}

let parse (file: string) =
    let lines = File.ReadAllLines file
    let (height, width) = (lines.Length, lines[0].Length)
    
    let mutable (start, dest) = ((0, 0), (0, 0))
    let grid = Array2D.init width height <| fun x y ->
        match lines[y][x] with
            | 'S' -> start <- (x, y); 0
            | 'E' -> dest <- (x, y); 25
            | c -> int c - int 'a'

    { Grid = grid; Start = start; End = dest }

let getNeighbors (grid: int[,]) (x, y) =
    seq {(0, 1); (0, -1); (1, 0); (-1, 0)}
    |> Seq.map (fun (i, j) -> (x + i, y + j))
    |> Seq.filter (Array2D.inside grid)

let pathfind valid grid start =
    let mutable explored = Set.singleton start
    let mutable queue = Queue.singleton start
    let mutable dists = Map.ofSeq <| seq {(start, 0)}

    while not (Queue.isEmpty queue) do
        let (x, y), nextQueue = Queue.dequeue queue
        queue <- nextQueue

        getNeighbors grid (x, y)
        |> Seq.filter (fun (i, j) -> valid grid[x, y] grid[i, j])
        |> Seq.filter (fun p -> not <| Seq.contains p explored)
        |> Seq.iter (fun (i, j) ->
            explored <- Set.add (i, j) explored
            let dist = dists[(x, y)] + 1
            dists <- Map.add (i, j) dist dists
            queue <- Queue.enqueue (i, j) queue)

    Array2D.coordSeq grid
    |> Seq.filter (fun p -> not <| Map.containsKey p dists)
    |> Seq.iter (fun p -> dists <- Map.add p System.Int32.MaxValue dists)

    dists

let solveSilver file =
    let scen = parse file
    let dist = pathfind (fun s d -> d - s <= 1) scen.Grid scen.Start
    string dist[scen.End]

let solveGold file =
    let scen = parse file
    let dist = pathfind (fun s d -> d - s >= -1) scen.Grid scen.End

    Array2D.coordSeq scen.Grid
    |> Seq.filter (fun (x, y) -> scen.Grid[x, y] = 0)
    |> Seq.map (fun p -> dist[p])
    |> Seq.min
    |> string

let Solvers = (solveSilver, solveGold)
