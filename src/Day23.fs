module Day23

open System.IO
open Util.Debug
open Util.Extensions
open Util.Extensions.Tuple2.Infix
open Util.Plumbing

let parse file =
    File.ReadAllLines file
    |> Array.rev
    |> Array.mapi (fun r -> Seq.mapi (fun c -> function '#' -> Some (r, c) | _ -> None) >> Seq.choose id)
    |> Seq.collect id
    |> Set.ofSeq

let getBounds elves =
    let elves = Set.toList elves
    let ll = List.map fst elves |> List.min, List.map snd elves |> List.min
    let ur = List.map fst elves |> List.max, List.map snd elves |> List.max
    ll, ur

let printGrid elves =
    let (r0, c0), (r1, c1) = getBounds elves
    for i in r1 .. -1 .. r0 do
        for j in c0 .. c1 do
            printf "%c" (if Set.contains (i, j) elves then '#' else '.')
        printfn ""
    printfn ""

let checkLive elves pos =
    [ for i in -1 .. 1 do for j in -1 .. 1 do if not (i = 0 && j = 0) then yield (i, j) ]
    |> List.map ((.+) pos)
    |> List.exists (elves: Set<_>).Contains

let proposeMove elves index pos =
    let checkEmpty offs =
        List.map ((.+) pos) offs
        |> List.forall (fun n -> Set.contains n elves |> not)

    let proposals = [
        [ 1, -1;  1,  0;  1,  1], ( 1,  0) // North
        [-1, -1; -1,  0; -1,  1], (-1,  0) // South
        [-1, -1;  0, -1;  1, -1], ( 0, -1) // West 
        [-1,  1;  0,  1;  1,  1], ( 0,  1) // East
    ]

    [ index .. index + 3 ]
    |> List.map (fun i -> List.item (i % 4) proposals)
    |> List.tryFind (fst >> checkEmpty)
    |> Option.map (fun (_, prop) -> pos, pos .+ prop)

let countEmpty elves =
    let (r0, c0), (r1, c1) = getBounds elves
    let mutable count = 0
    for i in r0 .. r1 do
        for j in c0 .. c1 do
            if not <| Set.contains (i, j) elves then
                count <- count + 1
    count

let doRound n elves =
    let getProposals elves =
        elves
        |> Set.toList
        |> List.filter (fun e -> checkLive elves e)
        |> List.choose (fun e -> proposeMove elves n e)

    let selectMoves =
        List.groupBy snd
        >> List.filter (fun (_, n) -> List.length n = 1)
        >> List.collect snd
        >> List.groupBy fst
        >> List.map (fun (_, n) -> List.head n)

    let performMoves elves moves =
        List.fold (fun acc (rem, add) -> acc |> Set.remove rem |> Set.add add) elves moves

    let moves = getProposals elves |> selectMoves
    performMoves elves moves, List.length moves

let doRounds numRounds elves =
    let rec recur n elves =
        if n = numRounds then
            elves
        else
            let (elves, _) = doRound n elves
            recur (n + 1) elves
    recur 0 elves

let solveSilver =
    doRounds 10
    >> countEmpty
    >> string

let solveGold =
    let rec recur n elves =
        match doRound n elves with
        | _, 0 -> (n + 1)
        | elves, _ -> recur (n + 1) elves

    recur 0 >> string

let Solver = chainSolver parse solveSilver solveGold
