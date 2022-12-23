module Day22

open System.IO
open Util.Collections
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

type TileType = Floor | Wall
type Move = Forward of int | TurnLeft | TurnRight

type Scenario = {
    Walls: Set<int * int>
    Floors: Set<int * int>
    Valid: Set<int * int>
    Moves: Move list
}

type Direction = North | South | East | West

type Face = A | B | C | D | E | F //| Space

let FaceSize = 50
let FaceBound = FaceSize - 1

let getArrivalFace fromFace dir =
    match fromFace, dir with
    | A, North -> B, South
    | A, South -> F, North
    | A, East -> C, South
    | A, West -> E, North
    | B, North -> D, West
    | B, South -> A, North
    | B, East -> C, West
    | B, West -> E, West
    | C, North -> D, South
    | C, South -> A, East
    | C, East -> F, East
    | C, West -> B, East
    | D, North -> E, South
    | D, South -> C, North
    | D, East -> F, South
    | D, West -> B, North
    | E, North -> A, West
    | E, South -> D, North
    | E, East -> F, West
    | E, West -> B, West
    | F, North -> A, South
    | F, South -> D, East
    | F, East -> C, East
    | F, West -> E, East

let inline (.+) (x, y) (a, b) = (x + a, y + b)
let inline (.*) (x, y) n = (x * n, y * n)
let inline (./) (x, y) n = (x / n, y / n)
let inline (.%) (x, y) n = ((x % n + n) % n, (y % n + n) % n)
let row = fst
let col = snd

let parse =
    let rec handleMoves moves = function
    | "L" :: more -> handleMoves (TurnLeft :: moves) more
    | "R" :: more -> handleMoves (TurnRight :: moves) more
    | Int32 n :: more -> handleMoves (Forward n :: moves) more
    | [] -> List.rev moves
    | ws :: more when ws.Trim().Length = 0 -> handleMoves moves more
    | _ -> failwith "Invalid input format"

    let handleMapChar r c = function
    | '#' -> Some (Wall, (r, c) .+ (FaceSize, FaceSize))
    | '.' -> Some (Floor, (r, c) .+ (FaceSize, FaceSize))
    | _ -> None

    let rec handleLines r points = function
    | "" :: more ->
        let moves = String.regMatches @"\d+|L|R|\s+" more.Head |> handleMoves []
        let walls, floors =
            List.collect (Seq.choose id >> Seq.toList) points
            |> List.partition (fst >> (=) Wall)
            |> Tuple2.map (List.map snd >> Set.ofList)
        { Walls = walls; Floors = floors; Valid = Set.union walls floors; Moves = moves }
    | line :: more ->
        let ps = Seq.mapi (handleMapChar r) line
        handleLines (r + 1) (ps :: points) more
    | _ ->
        failwith "Invalid input format"

    File.ReadAllLines
    >> Array.toList
    >> handleLines 0 []

let findStartState scen =
    let pos =
        scen.Floors
        |> Seq.groupBy fst
        |> Seq.minBy fst
        |> snd
        |> Seq.minBy snd
    pos, East

let getDirectionOffset = function
| North -> (-1, 0)
| South -> (1, 0)
| West -> (0, -1)
| East -> (0, 1)

let flip = function
| North -> South | South -> North
| East -> West | West -> East

let getFace (r, c) =
    match r / FaceSize, c / FaceSize with
    | 4, 1 -> A | 3, 1 -> B | 3, 2 -> C
    | 2, 2 -> D | 1, 2 -> E | 1, 3 -> F
    | _ -> failwith "Invalid face"
    // | _ -> Space

let getFaceOrigin face =
    let coord =
        match face with
        | A -> 4, 1 | B -> 3, 1 | C -> 3, 2
        | D -> 2, 2 | E -> 1, 2 | F -> 1, 3
        // | _ -> failwith "Invalid face"
    coord .* FaceSize

// 122332

let translate (r, c) fromDir fromFace =
    let (toFace, toDir) = getArrivalFace fromFace fromDir
    let i =
        match fromDir with
        | North -> c % FaceSize
        | South -> FaceBound - c % FaceSize
        | East -> r % FaceSize
        | West -> FaceBound - r % FaceSize
    let realArrivalPoint =
        match toDir with
        | North -> 0, FaceBound - i
        | South -> FaceBound, i
        | East -> FaceBound - i, FaceBound
        | West -> i, 0
    
    let origin = getFaceOrigin toFace
    let point = origin .+ realArrivalPoint, flip toDir
    printfn "%A" {| toFace = toFace; org = origin; i = i; loc = realArrivalPoint; glob = point |}
    point
    

let calcNextState scen (pos, dir) move =
    let foreTile (pos, dir) =
        let next = pos .+ getDirectionOffset dir

        let fromFace = getFace pos

        let next, nextDir =
            if next ./ FaceSize <> pos ./ FaceSize then
                translate pos dir fromFace
            else
                // printfn "%A" next
                next, dir

        if scen.Floors.Contains next then
            next, nextDir
        elif scen.Walls.Contains next then
            pos, dir
        else
            failwith "what the fuck"
            // let next =
            //     match dir with
            //     | North -> scen.Valid |> Seq.filter (col >> (=) c) |> Seq.maxBy row
            //     | South -> scen.Valid |> Seq.filter (col >> (=) c) |> Seq.minBy row
            //     | East -> scen.Valid |> Seq.filter (row >> (=) r) |> Seq.minBy col
            //     | West -> scen.Valid |> Seq.filter (row >> (=) r) |> Seq.maxBy col
            // if scen.Walls.Contains next then
            //     pos, dir
            // else
            //     next, dir


    match move with
    | TurnLeft ->
        let dir = match dir with North -> West | West -> South | South -> East | East -> North
        pos, dir
    | TurnRight ->
        let dir = match dir with North -> East | East -> South | South -> West | West -> North
        pos, dir
    | Forward n ->
        let st = [ 1 .. n ] |> List.fold (fun st _ -> foreTile st) (pos, dir)
        st

let scoreState ((r, c), dir) = 
    let posScore = 1000 * (r - FaceSize + 1) + 4 * (c - FaceSize + 1)
    let dirScore = match dir with East -> 0 | South -> 1 | West -> 2 | North -> 3
    posScore + dirScore

let solveSilver input =
    List.fold (calcNextState input) (findStartState input) input.Moves
    |> scoreState
    |> sprintf "%i"

let solveGold input =
    // findStartState input
    List.fold (calcNextState input) (findStartState input) input.Moves
    |> fun (pos, dir) -> (pos, pos .% FaceSize, dir, getFace pos, scoreState (pos, dir))
    // |> scoreState
    |> sprintf "%A"

let Solver = chainSolver parse solveSilver solveGold
