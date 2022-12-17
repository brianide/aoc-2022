module Day17

open System
open System.IO
open Util.Extensions
open Util.Plumbing

let ChamberWidth = 7
let GridMax = 2000
let Threshold = 0

[<Struct>]
type Shape = {
    Points: struct(int * int)[]
    Width: int
    Height: int
}

let Shapes =
    let conv shp =
        let points =
            (shp: string).Replace(" ", "").Split('\n')
            |> Seq.rev
            |> Seq.mapi (fun r row -> row |> Seq.mapi (fun c n -> if n = '#' then [struct(r, c)] else []) |> Array.ofSeq)
            |> Seq.collect(Seq.collect id)
            |> Seq.toArray
        let width = points |> Seq.map (fun struct(_, c) -> c) |> Seq.max
        let height = points |> Seq.map (fun struct(r, _) -> r) |> Seq.max
        {Points = points; Width = width; Height = height}

    [|
        "####";

        ".#.
         ###
         .#.";

        "..#
         ..#
         ###";

        "#
         #
         #
         #";
        
        "##
         ##";
    |]
    |> Array.map conv

let parse file =
    File.ReadAllText file
    |> Seq.map (function '<' -> -1 | '>' -> 1 | _ -> failwith "Invalid input")
    |> Seq.toArray

type Tower = {
    mutable Grid: bool[,]
    mutable LocalHeight: int
    mutable TotalHeight: uint64
}

module Fast = // ...in a relative sense

    let inline (.+) struct(ax, ay) struct(bx, by) = struct(ax + bx, ay + by)
    let inline (.*) struct(ax, ay) n = struct(ax * n, ay * n)
    let inline modIndex (arr: ^a[]) n = arr[n % arr.Length]
    let inline bounds lo hi n = lo <= n && n <= hi

    let jetShift (shape: Shape) (jets: int[]) jetIndex struct(r, c) =
        let mutable left = 0
        let mutable right = 0
        for i in 0 .. 2 do
            if jets[(jetIndex + i) % 3] > 0 then
                left <- left + 1
            else
                right <- right + 1

        let newC = min (c + right) (ChamberWidth - 1 - shape.Width)
        let newC = max 0 newC
        struct(r, newC)

    let checkHorizontal (shape: Shape) (grid: bool[,]) pos =
        shape.Points
        |> Array.map (fun off -> pos .+ off)
        |> Array.exists (fun struct(r, c) -> not (bounds 0 (ChamberWidth - 1) c) || grid[r, c])

    let checkVertical (shape: Shape) (grid: bool[,]) pos =
        shape.Points
        |> Array.map (fun off -> pos .+ off)
        |> Array.exists (fun struct(r, c) -> r < 0 || grid[r, c])

    let blit (shape: Shape) (grid: bool[,]) pos =
        let points = shape.Points |> Array.map (fun off -> pos .+ off)
        points |> Array.iter (fun struct(r, c) -> grid[r, c] <- true)
        points |> Array.map (fun struct(r, _) -> r) |> Array.max

    let solve dropCount (jets: int[]) =

        let tower = {
            Grid = Array2D.zeroCreate<bool> GridMax ChamberWidth
            LocalHeight = 0
            TotalHeight = 0UL
        }

        let mutable jetIndex = 0
        let mutable shapeIndex = 0

        for _ in 1UL .. dropCount do
            let shape = modIndex Shapes shapeIndex
            shapeIndex <- shapeIndex + 1

            let mutable pos = struct (tower.LocalHeight + 3, 2)
            let mutable falling = true

            while falling do
                let shift = modIndex jets jetIndex
                jetIndex <- jetIndex + 1

                let pos' = pos .+ struct(0, shift)
                if not <| checkHorizontal shape tower.Grid pos' then
                    pos <- pos'

                let pos' = pos .+ struct(-1, 0)
                if checkVertical shape tower.Grid pos' then
                    falling <- false
                    let blockHeight = blit shape tower.Grid pos
                    let oldLocalHeight = tower.LocalHeight
                    tower.LocalHeight <- 1 + max tower.LocalHeight blockHeight
                    tower.TotalHeight <- tower.TotalHeight + uint64 (tower.LocalHeight - oldLocalHeight)
                    
                    {blockHeight .. -1 .. blockHeight - 1}
                    |> Seq.tryFind (fun r -> r > 0 && tower.Grid[r, *] |> Array.forall id)
                    |> Option.filter (fun r -> r > Threshold)
                    |> function
                    | Some tet ->
                        // printfn "Tetris at %A" tet
                        let temp = tower.Grid
                        tower.Grid <- Array2D.zeroCreate<bool> GridMax ChamberWidth

                        let linesAbove = tower.LocalHeight - tet
                        let width = ChamberWidth - 1

                        tower.Grid[0 .. linesAbove, 0 .. width] <- temp[tet .. tet + linesAbove, 0 .. width]
                        tower.LocalHeight <- linesAbove
                    | None -> ()
                else
                    pos <- pos'

        tower.TotalHeight |> string


let solveSilver = Fast.solve 2022UL

let solveGold = Fast.solve 1000000000000UL

let Solver = chainSolver parse solveSilver solveGold
