module Day12

open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns

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
            | 'S' ->
                start <- (x, y)
                0
            | 'E' ->
                dest <- (x, y)
                int 'z' - int 'a'
            | c ->
                int c - int 'a'

    { Grid = grid; Start = start; End = dest }

let getNeighbors (grid: int[,]) x y =
    seq {(0, 1); (0, -1); (1, 0); (-1, 0)}
    |> Seq.map (fun (i, j) -> (x + i, y + j))
    |> Seq.filter (Array2D.inside grid)

let pathfind {Grid = grid; Start = start; End = dest} =
    let mutable unvisited = Set.ofSeq <| seq {
        yield start

        for y in 0 .. Array2D.length2 grid - 1 do
            for x in 0 .. Array2D.length1 grid - 1 do
                if (x, y) <> start then
                    yield (x, y)
    }

    let mutable dist = unvisited |> Seq.map (fun p -> (p, if p = start then 0 else System.Int32.MaxValue)) |> Map.ofSeq
    let mutable prev = Map.empty
    
    while not (Set.isEmpty unvisited) do
        let (x, y) = Seq.minBy (fun p -> dist[p]) unvisited

        getNeighbors grid x y
        |> Seq.filter (fun (i, j) -> grid[i, j] - grid[x, y] <= 1)
        |> Seq.filter (fun p -> Seq.contains p unvisited)
        |> Seq.iter (fun (i, j) -> 
            let d = dist[(x, y)] + 1
            if d < dist[(i, j)] then
                dist <- Map.add (i, j) d dist
                prev <- Map.add (i, j) (x, y) prev)

        unvisited <- unvisited |> Set.remove (x, y)
    
    let path = seq {
        let mutable head = prev[dest]
        yield dest
        while Map.containsKey head prev do
            yield head
            head <- Map.find head prev
    }

    path

let solveSilver file =
    parse file
    |> pathfind
    |> Seq.length
    |> string

let solveGold file =
    parse file
    |> ignore
    ""

let Solvers = (solveSilver, solveGold)
