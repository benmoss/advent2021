open Stdio
open Base

type position = { x : int; y : int }

let apply pos move =
    { x = pos.x + move.x; y = pos.y + move.y }

let parse move =
    let to_move direction distance =
        match direction with
        | "forward" -> { x = distance; y = 0; }
        | "up" -> { x = 0; y = -distance }
        | "down" -> { x = 0; y = distance }
        | _ -> raise (Invalid_argument "invalid direction") in
    let split = String.split ~on:' ' move in
    let direction = match List.nth split 0 with
    | Some x -> x
    | None -> raise (Invalid_argument "direction not found") in
    let distance = match List.nth split 1 with
    | Some x -> Int.of_string x
    | None -> raise (Invalid_argument "distance not found") in
    to_move direction distance


let rec main pos =
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> pos
    | Some line ->
            let move = parse line in
            let applied = apply pos move in
            main applied

let () =
    let final = main { x = 0; y = 0; } in
    printf "Total: %d\n" (final.x * final.y)
