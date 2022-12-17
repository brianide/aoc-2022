module Day16

open System.IO
open Util.Collections
open Util.Extensions
open Util.Patterns
open Util.Plumbing

// Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE

type ValveGraph = Map<string, int * Map<string, int>>

let parse file =
    let parseLine line =
        let [rateString; tunnelString] = String.split "; " line
        let [valve; Int32 rate] = String.regGroups @"Valve ([A-Z]{2}) has flow rate=(\d+)" rateString

        let outlets =
            String.regMatches @"[A-Z]{2}" tunnelString
            |> Seq.map (fun n -> (n, 1))
            |> Map.ofSeq

        valve, (rate, outlets)

    File.ReadAllLines file
    |> Seq.map parseLine
    |> Map.ofSeq

let calcNodeCosts (graph: ValveGraph) start =
    let unvisited = Map.keys graph |> Set.ofSeq
    let prev = Map.empty
    let dists =
        Map.keys graph
        |> Seq.map (fun k -> k, if k = start then 0 else System.Int32.MaxValue)
        |> Map.ofSeq

    let rec recur unvisited (dists: Map<string,int>) prev curr =
        let dist = dists[curr]

        let newDists = snd graph[curr] |> Map.toSeq |> Seq.map (fun (k, cost) -> k, min dists[k] (cost + dist))
        let dists = Seq.fold (fun acc (k, v) -> Map.add k v acc) dists newDists
        let unvisited = Set.remove curr unvisited

        let prev = Map.add curr (newDists |> Seq.minBy snd |> fst) prev

        if Set.isEmpty unvisited then
            prev, dists |> Map.filter (fun k _ -> k <> start)
        else
            let next = unvisited |> seq |> Seq.minBy (fun k -> dists[k])
            recur unvisited dists prev next

    recur unvisited dists prev start |> snd

let flattenGraph graph =
    Map.keys graph
    |> Seq.fold (fun acc k -> Map.add k (fst graph[k], calcNodeCosts graph k) acc) Map.empty

let solveGraph time previs (graph: ValveGraph) =
    let flow = Map.map (fun _ v -> fst v) graph
    let paths = Map.map (fun _ v -> snd v |> Map.toSeq) graph

    let rec recur curr path nodesLeft timeLeft =
        let path = (curr, (flow[curr], time - timeLeft)) :: path
        if Set.isEmpty nodesLeft then
            Seq.singleton (List.rev path)
        else
            let more =
                paths[curr]
                |> Seq.filter (fun (k, c) -> Set.contains k nodesLeft && timeLeft - c - 1 > 0)
            
            if Seq.isEmpty more then
                Seq.singleton(List.rev path)
            else
                more
                |> Seq.collect (fun (k, cost) ->
                    let timeLeft = timeLeft - cost - 1
                    if timeLeft >= 0 then
                        recur k path (Set.remove k nodesLeft) timeLeft
                    else
                        [])

    recur "AA" [] (flow |> Map.filter (fun _ v -> v > 0) |> Map.keys |> Set.ofSeq |> fun n -> Set.difference n previs) time

let solveSilver input =
    // Map.iter (fun k (flow, outs) -> printfn "%s: %2d >> %s" k flow (outs |> Map.toSeq |> Seq.map (fun (k, v) -> sprintf "%s %d" k v) |> String.concat ", ")) input

    solveGraph 30 Set.empty input
    |> Seq.map (fun n -> n, Seq.sumBy (fun (_, (flow, time)) -> flow * (30 - time)) n)
    |> Seq.maxBy snd
    |> string

let solveGold input =
    let foo =
        solveGraph 26 Set.empty input
        |> Seq.map (fun n -> n |> Seq.map fst |> Set.ofSeq, Seq.sumBy (fun (_, (flow, time)) -> flow * (26 - time)) n)
        |> Seq.sortByDescending snd

    printfn "%A" (Seq.head foo)

    solveGraph 26 (foo |> Seq.head |> fst) input
    |> Seq.map (fun n -> n |> Seq.map fst |> Set.ofSeq, Seq.sumBy (fun (_, (flow, time)) -> flow * (26 - time)) n)
    |> Seq.maxBy snd
    |> string

let Solver = chainSolver (parse >> flattenGraph) solveSilver solveGold
