open Stdio
open Base
(* - read all moves
- assemble boards
- move by move, update each board
- if bingo, total that board *)

type number = { int : int; marked : bool; }
type game = { moves : int list; boards : number list list list }

let read_game (l : string list) : game =
    let read_moves line =
        let split = String.split line ~on:',' in
        if List.length split = 0 then raise (Invalid_argument "invalid value") else
            List.map split ~f:Int.of_string in
    let rec read_boards l (acc : number list list list) =
        match l with
        | [] -> acc
        | hd :: tl ->
                let stripped = String.strip hd in
                if String.is_empty stripped then
                    read_boards tl ([] :: acc)
                else
                    let split = String.split hd ~on:' ' in
                    let stripped = List.filter split ~f:(Fn.compose not String.is_empty) in
                    let ints = List.map stripped ~f:(fun x -> { int = Int.of_string x; marked = false }) in
                    match List.hd acc with
                    | None -> read_boards tl ([ints] :: acc)
                    | Some l -> read_boards tl ((ints :: l) :: (List.drop acc 1))
    in
    match l with
    | [] -> raise (Invalid_argument "no lines")
    | hd :: tl ->
            let moves = read_moves hd in
            let boards = read_boards (List.rev (List.drop tl 1)) [] in
            { moves = moves; boards = boards}

let rec transpose = function
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map xss ~f:List.hd_exn) :: transpose (xs :: List.map xss ~f:List.tl_exn)

let rec find_winner { moves; boards } =
    let update_board move board =
        let update_row row =
            List.map row ~f:(fun x -> if x.int = move then { x with marked = true } else x) in
        List.map board ~f:update_row in
    let winner numbers =
        let all_marked l = List.find l ~f:(fun row -> List.for_all row ~f:(fun x -> x.marked)) in
        let across = all_marked numbers in
        let down = all_marked (transpose numbers) in
        Option.is_some across || Option.is_some down in
    match moves with
    | [] -> raise (Invalid_argument "ran out of moves")
    | hd :: tl ->
            let updated = List.map boards ~f:(update_board hd) in
            let winning_board = List.find updated ~f:winner in
            match winning_board with
            | None -> find_winner { moves = tl; boards = updated }
            | Some w -> (w, hd)

let calculate_score (board, winning_number) =
    let sum_row row = List.fold row ~init:0 ~f:(fun acc n -> if n.marked then acc else acc + n.int) in
    let sum = List.fold board ~init:0 ~f:(fun acc row -> acc + sum_row row) in
    sum * winning_number

let rec readlines acc =
    match In_channel.input_line In_channel.stdin with
    | None -> acc
    | Some line -> line :: readlines acc

let () =
    let game = read_game (readlines []) in
    let winner = find_winner game in
    Stdio.printf "score: %d" (calculate_score winner)
