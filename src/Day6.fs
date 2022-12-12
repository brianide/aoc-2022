module Day6

open System.IO
open Util.Plumbing

let findMarker length =
    Seq.windowed length
    >> Seq.findIndex (Seq.distinct >> Seq.length >> (=) length)
    >> (+) length

let solve length = findMarker length >> string

let Solver = chainSolver File.ReadAllText (solve 4) (solve 14)