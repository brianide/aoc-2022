module Day21

open System.IO
open Util.Collections
open Util.Extensions
open Util.Math
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

let parse file =
    let tokenizeLine = function
    | RegGroups @"([a-z]{4}): ([a-z]{4}) ([*/+-]) ([a-z]{4})" [label; key1; op; key2] -> label, Operation (op, key1, key2)
    | RegGroups @"([a-z]{4}): (\d+)" [label; Int64 value] -> label, Constant value
    | s -> failwithf "Invalid input: %s" s

    File.ReadAllLines file |> Array.map tokenizeLine |> Map.ofArray

let solveSilver input =
    ""

let rec parseExpression (table: Map<string, Token>) root =
    let (|Operator|) = function
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | op -> failwithf "Invalid operator: %s" op

    match root, table[root] with
    | "root", Operation (_, key1, key2) -> Equal (parseExpression table key1, parseExpression table key2)
    | "humn", _ -> Query
    | _, Constant value -> Const value
    | _, Operation (Operator op, key1, key2) -> op (parseExpression table key1, parseExpression table key2)

let rec simplify expr =
    let reduce e = function
    | Const _ -> failwith "Can't solve constant equality!"
    | Add (Const n, b) -> Equal (Const (e - n), b)
    | Add (a, Const n) -> Equal (Const (e - n), a)
    | Sub (Const n, b) -> Equal (Const (n - e), b)
    | Sub (a, Const n) -> Equal (Const (e + n), a)
    | Mul (Const n, b) -> Equal (Const (e / n), b)
    | Mul (a, Const n) -> Equal (Const (e / n), a)
    | Div (Const n, b) -> Equal (Const (n / e), b)
    | Div (a, Const n) -> Equal (Const (e * n), a)
    | Query -> Const e
    | x -> printfn "Unreduced: %A" (e, x); x

    match expr with
    | Const n -> Const n
    | Query -> Query
    | Add (Const a, Const b) -> Const (a + b)
    | Add (a, b) -> Add (simplify a, simplify b)
    | Sub (Const a, Const b) -> Const (a - b)
    | Sub (a, b) -> Sub (simplify a, simplify b)
    | Mul (Const a, Const b) -> Const (a * b)
    | Mul (a, b) -> Mul (simplify a, simplify b)
    | Div (Const a, Const b) -> Const (a / b)
    | Div (a, b) -> Div (simplify a, simplify b)
    | Equal (Const a, b) -> reduce a (simplify b)
    | Equal (a, Const b) -> reduce b (simplify a)
    | Equal (a, b) -> Equal (simplify a, simplify b)


let solveGold input =
    let rec simp expr =
        let next = simplify expr
        if next <> expr then
            simp next
        else
            next

    parseExpression input "root"
    |> simp
    |> printfn "%A"
    ""

let Solver = chainSolver parse solveSilver solveGold
