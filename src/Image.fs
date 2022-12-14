module Image

open System
open System.IO
open System.Text

let saveToPGM byteFn path grid =
    let width, height = Array2D.length2 grid, Array2D.length1 grid
    
    let header = ASCIIEncoding.ASCII.GetBytes $"P5 {width} {height} 255 "

    let body = Array.zeroCreate<byte> (width * height)
    for i in 0 .. body.Length - 1 do
        let r, c = i / width, i % width
        body[i] <- byteFn c r grid[r, c]

    let payload = Array.zeroCreate<byte> (header.Length + body.Length)
    Buffer.BlockCopy(header, 0, payload, 0, header.Length)
    Buffer.BlockCopy(body, 0, payload, header.Length, body.Length)
    File.WriteAllBytes(path, payload)