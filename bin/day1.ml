open Base
open Stdio

let windowed_prepend l x len =
    let l = x :: l in
    let reversed = List.rev l in
    let to_drop = List.length l - len in
    let dropped = List.drop reversed to_drop in
    List.rev dropped

let rec count total acc =
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> total
    | Some line ->
        let int = Int.of_string line in
        let current = windowed_prepend acc int 3 in
        let total = if List.length acc = 3 then
            let sum x = match List.reduce x ~f:(+) with
            | None -> 0
            | Some x -> x in
            let previous_sum = sum acc in
            let new_sum = sum current in
            if new_sum > previous_sum then total+1 else total
        else
            total in
        count total current

let () =
    printf "Total: %d\n" (count 0 [])
