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

let pruneZeroNodes graph =
    let rec prune tab =
        tab
        |> Map.tryFindKey (fun k (flow, _) -> k <> "AA" && flow = 0)
        |> function
        | None -> tab
        | Some kz ->
            tab
            |> Map.toSeq
            |> Seq.filter (fun (_, (_, outs)) -> Map.containsKey kz outs)
            |> Seq.map (fun (ks, (flow, outs)) ->
                let repl =
                    tab[kz] |> snd |> Map.toSeq
                    |> Seq.filter (fun (kd, _) -> kd <> ks)
                    |> Seq.map (fun (k, v) -> k, v + 1)
                let outs =
                    outs |> Map.toSeq
                    |> Seq.filter (fun (dest, _) -> dest <> kz)
                    |> Seq.append repl
                    |> Map.ofSeq
                ks, (flow, outs))
            |> Seq.fold (fun acc (k, v) -> Map.add k v acc) (Map.filter (fun k _ -> k <> kz) tab)
            |> prune 
    prune graph

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

let branchSolve (graph: ValveGraph) =
    let flow = Map.map (fun _ v -> fst v) graph
    let paths = Map.map (fun _ v -> snd v |> Map.toSeq) graph

    let rec recur curr path nodesLeft timeLeft =
        if timeLeft <= 0 then
            Seq.singleton (List.rev path)
        elif Set.isEmpty nodesLeft then
            let path = (curr, (flow[curr], 30 - timeLeft)) :: path
            Seq.singleton (List.rev path)
        else
            let path = (curr, (flow[curr], 30 - timeLeft)) :: path
            paths[curr]
            |> Seq.filter (fun (k, _) -> Set.contains k nodesLeft)
            |> Seq.collect (fun (k, cost) -> recur k path (Set.remove k nodesLeft) (timeLeft - cost - 1))

    recur "AA" [] (flow |> Map.filter (fun _ v -> v > 0) |> Map.keys |> Set.ofSeq) 30

let solveSilver input =
    let perms = Map.keys input |> Seq.filter ((<>) "AA") |> Seq.toList |> List.permutations
    
    // Map.iter (fun k (flow, outs) -> printfn "%s: %2d >> %s" k flow (outs |> Map.toSeq |> Seq.map (fun (k, v) -> sprintf "%s %d" k v) |> String.concat ", ")) input

    branchSolve input
    |> Seq.map (fun n -> n, Seq.sumBy (fun (_, (flow, time)) -> flow * (30 - time)) n)
    |> Seq.maxBy snd
    |> printfn "%A"
    
    ""

let solveGold input =
    ""

let Solver = chainSolver (parse >> flattenGraph) solveSilver solveGold
