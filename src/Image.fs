module Image

open System
open System.IO
open System.Text

let saveToPGM byteFn path grid =
    let width, height = Array2D.length2 grid, Array2D.length1 grid
    let header = $"P5 {width} {height} 255 "
    
    let body = Array.zeroCreate<byte> (ASCIIEncoding.ASCII.GetByteCount header + width * height)
    let offset = ASCIIEncoding.ASCII.GetBytes(header, body)

    for i in 0 .. width * height - 1 do
        let r, c = i / width, i % width
        body[i + offset] <- byteFn c r grid[r, c]

    File.WriteAllBytes(path, body)