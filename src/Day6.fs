module Day6

open System.IO

let findMarker length =
    Seq.windowed length
    >> Seq.findIndex (Seq.distinct >> Seq.length >> (=) length)
    >> (+) length

let solve length file =
    File.ReadAllText file
    |> findMarker length
    |> string

let Solvers = (solve 4, solve 14)