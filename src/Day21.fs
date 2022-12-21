module Day21

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

type Token =
| Constant of int64
| Operation of string * string * string

type Expr =
| Const of int64
| Query
| Add of Expr * Expr
| Sub of Expr * Expr
| Mul of Expr * Expr
| Div of Expr * Expr
| Equal of Expr * Expr

let (|BinaryArithmetic|_|) = function
| Add (a, b) -> Some (Add, (+), a, b)
| Sub (a, b) -> Some (Sub, (-), a, b)
| Mul (a, b) -> Some (Mul, (*), a, b)
| Div (a, b) -> Some (Div, (/), a, b)
| _ -> None


let tokenize file =
    let tokenizeLine = function
    | RegGroups @"([a-z]{4}): ([a-z]{4}) ([*/+-]) ([a-z]{4})" [label; key1; op; key2] -> label, Operation (op, key1, key2)
    | RegGroups @"([a-z]{4}): (\d+)" [label; Int64 value] -> label, Constant value
    | s -> failwithf "Invalid input: %s" s

    File.ReadAllLines file |> Array.map tokenizeLine |> Map.ofArray

let rec parseExpression (table: Map<string, Token>) xform root =
    let (|Operator|) = function
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | op -> failwithf "Invalid operator: %s" op

    match table[root] with
    | Operation (Operator op, key1, key2) -> op (parseExpression table xform key1, parseExpression table xform key2)
    | Constant value -> Const value
    |> fun n -> xform (root, n)

let rec simplify expr =
    let reduce c = function
    | Const _ -> failwith "Can't solve equality between constants"
    | Add (Const n, b) -> Equal (Const (c - n), b) //  c = n + b  ==>  c - n = b
    | Add (a, Const n) -> Equal (Const (c - n), a) //  c = b + n  ==>  c - n = b
    | Sub (Const n, b) -> Equal (Const (n - c), b) //  c = n - b  ==>  n - c = b
    | Sub (a, Const n) -> Equal (Const (c + n), a) //  c = b - n  ==>  c + n = b
    | Mul (Const n, b) -> Equal (Const (c / n), b) //  c = n * b  ==>  c / n = b
    | Mul (a, Const n) -> Equal (Const (c / n), a) //  c = b * n  ==>  c / n = b
    | Div (Const n, b) -> Equal (Const (n / c), b) //  c = n / b  ==>  n / c = b
    | Div (a, Const n) -> Equal (Const (c * n), a) //  c = b / n  ==>  c * n = b
    | Query -> Const c
    | x -> Equal (Const c, x)

    match expr with
    | Const n -> Const n
    | Query -> Query
    | BinaryArithmetic (_, fn, Const a, Const b) -> Const (fn a b)
    | BinaryArithmetic (case, _, a, b) -> case (simplify a, simplify b)
    | Equal (Const a, b) -> reduce a (simplify b)
    | Equal (a, Const b) -> reduce b (simplify a)
    | Equal (a, b) -> Equal (simplify a, simplify b)
    | ex -> ex


let rec simplifyToConstant expr =
    let next = simplify expr
    if next <> expr then
        simplifyToConstant next
    else
        match next with
        | Const n -> n
        | _ -> failwithf "Expression did not reduce to a constant value: %A" expr

let solveSilver input =
    parseExpression input snd "root"
    |> simplifyToConstant
    |> string

let solveGold input =
    let xform = function
    | "root", BinaryArithmetic (_, _, a, b) -> Equal (a, b)
    | "humn", _ -> Query
    | _, n -> n

    parseExpression input xform "root"
    |> simplifyToConstant
    |> string

let Solver = chainSolver tokenize solveSilver solveGold
