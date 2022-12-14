module Day14

open System
open System.IO
open Util.Extensions
open Util.Plumbing

type Tile = Air | Wall | Sand
type SandState = Falling | Resting | OffGrid

let drawGrid grid limit =
    let getChar = function Air -> '.' | Wall -> '#' | Sand -> 'O'

    let limit = if limit <= 0 then System.Int32.MaxValue else limit

    for r in 0 .. min (limit) (Array2D.length1 grid - 1) do
        grid[r, *]
        |> Seq.iter (getChar >> printf "%c")
        printfn ""

let parse file =
    let connect pts =
        match pts |> Array.toList with
        | [[x; y]; [i; j]] when x = i -> [for t in y .. sign (j - y) .. j do yield (x, t)]
        | [[x; y]; [i; j]] when y = j -> [for t in x .. sign (i - x) .. i do yield (t, y)]
        | _ -> failwith "Invalid input"

    let parseLine =
        String.split " -> "
        >> Seq.map (String.split "," >> Seq.map int >> Seq.toList)
        >> Seq.windowed 2
        >> Seq.collect connect
        >> Seq.toList

    let points = File.ReadAllLines file |> Seq.collect parseLine |> Set.ofSeq
    let (minx, maxx, height) = (Seq.map fst points |> Seq.min) - 20, (Seq.map fst points |> Seq.max) + 20, (Seq.map snd points |> Seq.max) + 3
    let width = maxx - minx + 1
    let grid = Array2D.init height width (fun r c -> if Set.contains (c + minx, r) points then Wall else Air)

    (grid, 500 - minx)

let dropSand (grid: Tile[,]) x =
    let prio = [(1, 0); (1, -1); (1, 1)]
    let mutable pos = (0, x)
    let mutable state = Falling
    
    while state = Falling do
        let (pc, pr) = pos

        let getState =
            function
            | p when not (Array2D.isInside grid p) -> OffGrid
            | (r, c) when grid[r, c] = Air -> Falling
            | _ -> Resting
        
        Seq.map (fun (c, r) -> (c + pc, r + pr)) prio
        |> Seq.map (fun p -> (p, getState p))
        |> Seq.tryFind (fun (_, state) -> state <> Resting)
        |> function
        | Some (_, OffGrid) | None -> state <- Resting
        | Some (p, s) -> pos <- p; state <- s

    if state = Resting then
        grid[fst pos, snd pos] <- Sand

    state = OffGrid

let solveSilver (grid, genx) =
    let mutable filled = false
    let mutable count = 0

    while not filled do
        dropSand grid genx |> ignore
        filled <- grid[0, genx] = Sand
        count <- count + 1

    string (count - 1)

let solveGold (grid: Tile[,], genx) =
    let mutable filled = false
    let mutable count = 0

    let () =
        let height = Array2D.length1 grid - 1
        for c in 0 .. Array2D.length2 grid - 1 do
            grid[height, c] <- Wall

    while not filled do
        dropSand grid genx |> ignore
        filled <- grid[0, genx] = Sand
        count <- count + 1

        // if count % 1 = 0 then
        //     Console.Clear()
        //     Console.SetCursorPosition (0, 0)
        //     drawGrid grid (Console.WindowHeight - 6)

    let outside =
        let sumTo x = 
            grid[*, x]
            |> Seq.tryFindIndex ((=) Sand)
            |> function
            | Some n -> seq {1 .. Array2D.length1 grid - n - 2} |> Seq.sum
            | None -> 0
        sumTo 0 + sumTo (Array2D.length2 grid - 1)

    // drawGrid grid 0

    count + outside |> string

let Solver = chainSolver parse solveSilver solveGold
