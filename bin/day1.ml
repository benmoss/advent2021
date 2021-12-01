open Base
open Stdio


let rec count_incrementing total previous =
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> total
    | Some x ->
        let current = Int.of_string x in
        let total = if current > previous then total+1 else total in
        count_incrementing total current

let count =
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> 0
    | Some x ->
        let current = Int.of_string x in
        count_incrementing 0 current

let () =
    printf "Total: %d\n" count
