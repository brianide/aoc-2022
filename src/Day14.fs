module Day14

open System.IO
open Util.Extensions
open Util.Plumbing

type Tile = Air | Wall | Sand
type SandState = Falling | Resting | OffGrid | Blocked

let parse file =
    let connect pts =
        match pts |> Array.toList with
        | [[x; y]; [i; j]] when x = i -> [for t in y .. sign (j - y) .. j do yield (x, t)]
        | [[x; y]; [i; j]] when y = j -> [for t in x .. sign (i - x) .. i do yield (t, y)]
        | x -> failwithf "Invalid input: %A" x

    let parseLine =
        String.split " -> "
        >> Seq.map (String.split "," >> Seq.map int >> Seq.toList)
        >> Seq.windowed 2
        >> Seq.collect connect

    let points = File.ReadAllLines file |> Seq.collect parseLine |> Set.ofSeq
    let minx = (Seq.map fst points |> Seq.min) - 2
    let maxx = (Seq.map fst points |> Seq.max) + 2
    let height = (Seq.map snd points |> Seq.max) + 2
    let width = maxx - minx + 1
    let grid = Array2D.init height width (fun r c -> if Set.contains (c + minx, r) points then Wall else Air)

    (grid, 500 - minx)

let dropSand hasFloor (grid: Tile[,]) x =
    let prio = [(1, 0); (1, -1); (1, 1)]
    let mutable pos = (0, x)
    let mutable state = if grid[fst pos, snd pos] = Sand then Blocked else Falling
    
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
        | Some (_, OffGrid) -> state <- if hasFloor then Resting else OffGrid
        | Some (p, s) -> pos <- p; state <- s
        | None -> state <- Resting

    if state = Resting then
        grid[fst pos, snd pos] <- Sand

    state

let solveSilver (grid, genx) =
    // Run until sand falls off the grid
    let rec run n =
        if dropSand false grid genx <> OffGrid then
            run (n + 1)
        else
            n

    run 0 |> string

let solveGold (grid: Tile[,], genx) =
    // Run until sand is blocked
    let inside = 
        let rec run n =
            if dropSand true grid genx <> Blocked then
                run (n + 1)
            else
                n
        run 0

    // Sum up the sand that would have fallen to the left and right of the simulation area
    let outside =
        let total x = 
            grid[*, x]
            |> Seq.tryFindIndex ((=) Sand)
            |> function
            | Some n -> seq {1 .. Array2D.length1 grid - n - 1} |> Seq.sum
            | None -> 0
        total 0 + total (Array2D.length2 grid - 1)

    inside + outside |> string

let withCopy fn (grid, genx) =
    let copy = Array2D.copy grid
    fn (copy, genx)

let Solver = chainSolver parse (withCopy solveSilver) (withCopy solveGold)
