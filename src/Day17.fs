module Day17

open System
open System.IO
open Util.Extensions
open Util.Plumbing

let ChamberWidth = 7

let Shapes =
    let conv shp =
        (shp: string).Replace(" ", "").Split('\n')
        |> Array.map (fun row -> row |> Seq.map (fun n -> if n = '#' then 1 else 0) |> Array.ofSeq)
        |> array2D

    [|
        "....
         ....
         ....
         ####";

        "....
         .#..
         ###.
         .#..";

        "....
         ..#.
         ..#.
         ###.";

        "#...
         #...
         #...
         #...";
        
        "....
         ....
         ##..
         ##..";
    |]
    |> Array.map conv

let parse file =
    File.ReadAllText file
    |> Seq.map (function '<' -> -1 | '>' -> 1 | _ -> failwith "Invalid input")
    |> Seq.toArray

type Tower = {
    Grid: byte[]
    mutable LocalHeight: int
    mutable TotalHeight: uint64
}

module Fast = // ...in a relative sense

    let inline (.+) struct(ax, ay) struct(bx, by) = struct(ax + bx, ay + by)
    let inline modIndex (arr: ^a[]) n = arr[n % arr.Length]

    let checkCollision (shape: int[,]) (grid: byte[]) struct(pr, pc) =
        let rec check n =
            if pr < 0 || pc < 0 || pc >= ChamberWidth then
                true
            elif n >= 4 * 4 then
                false
            else
                let r = n / 4
                let c = n % 4
                let pr = pr + r
                let pc = pc + c
                shape[r, c] = 1 && pr < 0
                    || pc >= ChamberWidth
                    || (grid[pr] &&& (1uy >>> c) > 0uy)
                    || check (n + 1)
        check 0

    let blit (shape: int[,]) (grid: byte[]) struct(pr, pc) =
        for r in 0 .. 3 do
            for c in 0 .. 3 do
                if c + pc < ChamberWidth && shape[r, c] = 1 then
                    grid[r + pr] <- grid[r + pr] ||| (1uy >>> c)

    let solve dropCount (jets: int[]) =

        let tower = {
            Grid = Array.zeroCreate<byte> 8000
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

                let newPos = pos .+ struct(0, shift)
                if not <| checkCollision shape tower.Grid newPos then
                    pos <- newPos
                
                let newPos = pos .+ struct(-1, 0)
                if checkCollision shape tower.Grid newPos then
                    blit shape tower.Grid pos
                    tower.LocalHeight <- Array.findIndex (fun n -> n <> 0uy) tower.Grid
                    falling <- false

                    printfn "Placed at %A; new height %A" pos tower.LocalHeight 
                else
                    pos <- newPos

        for i in 0 .. 10 do
            printfn "%07B" tower.Grid[i]

        ""


let solveSilver = Fast.solve 5UL

let solveGold = Fast.solve 1000000000000UL

let Solver = chainSolver parse solveSilver solveGold
