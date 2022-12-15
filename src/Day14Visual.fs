module Day14Visual

open System.IO
open Util.Extensions
open Util.Plumbing

type Tile = Air | Wall | Sand
type SandState = Falling | Resting | OffGrid | Blocked

let mutable visited = Set.empty

let draw n grid =
    let every = 100

    if n % every = 0 then
        let fmt c r v =
            if visited.Contains (r, c) then
                byte 250
            else
                match v with 
                | Sand -> byte 250
                | Wall -> byte 180
                | Air -> byte 20
        Image.saveToPGM fmt (sprintf "mt/image%06d.pgm" (n / every)) grid

let parse file =
    let connect =
        function
        | [x; y], [i; j] when x = i -> [for t in y .. sign (j - y) .. j do yield (x, t)]
        | [x; y], [i; j] when y = j -> [for t in x .. sign (i - x) .. i do yield (t, y)]
        | x -> failwithf "Invalid input: %A" x

    let parseLine =
        String.split " -> "
        >> Seq.map (String.split "," >> Seq.map int >> Seq.toList)
        >> Seq.pairwise
        >> Seq.collect connect

    let points = File.ReadAllLines file |> Seq.collect parseLine |> Set.ofSeq
    let minx = (Seq.map fst points |> Seq.min) - 2
    let maxx = (Seq.map fst points |> Seq.max) + 2
    let height = (Seq.map snd points |> Seq.max) + 2
    let width = maxx - minx + 1
    let grid = Array2D.init height width (fun r c -> if Set.contains (c + minx, r) points then Wall else Air)

    draw 0 grid
    (grid, 500 - minx)

let dropSand hasFloor (grid: Tile[,]) x =
    let prio = seq {(1, 0); (1, -1); (1, 1)}
    let mutable pos = (0, x)
    let mutable state = if grid[fst pos, snd pos] = Sand then Blocked else Falling
    
    while state = Falling do
        visited <- visited.Add pos
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
    let rec run n =
        // draw n grid
        if dropSand true grid genx <> Blocked then
            run (n + 1)
        else
            n
    run 0

let renderSand infile dir prefix args =
    parse infile

let Solver = renderer renderSand
