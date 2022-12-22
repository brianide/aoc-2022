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

let inline (.+) (x, y) (a, b) = (x + a, y + b)
let inline maxGroupBy f = Seq.groupBy f >> Seq.maxBy fst >> snd
let inline minGroupBy f = Seq.groupBy f >> Seq.minBy fst >> snd
let row = fst
let col = snd

let parse =
    let rec handleMoves moves = function
    | "L" :: more -> handleMoves (TurnLeft :: moves) more
    | "R" :: more -> handleMoves (TurnRight :: moves) more
    | Int32 n :: more -> handleMoves (Forward n :: moves) more
    | [] -> List.rev moves
    | _ -> failwith "Invalid input format"

    let handleMapChar r c = function
    | '#' -> Some (Wall, (r, c))
    | '.' -> Some (Floor, (r, c))
    | _ -> None

    let rec handleLines r points = function
    | "" :: more ->
        let moves = String.regMatches @"\d+|L|R" more.Head |> handleMoves []
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

let calcNextState scen (pos, dir) move =
    let foreTile (pos, dir) =
        let next = pos .+ getDirectionOffset dir
        if scen.Floors.Contains next then
            next, dir
        elif scen.Walls.Contains next then
            pos, dir
        else
            let r, c = pos
            let next =
                match dir with
                | North -> scen.Valid |> Seq.filter (col >> (=) c) |> Seq.maxBy row
                | South -> scen.Valid |> Seq.filter (col >> (=) c) |> Seq.minBy row
                | East -> scen.Valid |> Seq.filter (row >> (=) r) |> Seq.minBy col
                | West -> scen.Valid |> Seq.filter (row >> (=) r) |> Seq.maxBy col
            if scen.Walls.Contains next then
                pos, dir
            else
                next, dir


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
    let posScore = 1000 * (r + 1) + 4 * (c + 1)
    let dirScore = match dir with East -> 0 | South -> 1 | West -> 2 | North -> 3
    posScore + dirScore

let solveSilver input =
    List.fold (calcNextState input) (findStartState input) input.Moves
    |> scoreState
    |> sprintf "%i"

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
