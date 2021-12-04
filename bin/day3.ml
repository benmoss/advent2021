open Stdio

let most_frequent = function
    | [] -> 0
    | x ->
            let sum acc = function
            | 0 -> acc-1
            | 1 -> acc+1
            | _ -> raise (Invalid_argument "invalid value") in
            let result = List.fold_left sum 0 x in
            if result == 0 then raise (Invalid_argument "ambiguous") else if result > 0 then 1 else 0

let rec gamma = function
    | [] -> []
    | hd :: tl ->
            most_frequent hd :: gamma tl

let rec transpose = function
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let rec to_list = function
    | "" -> []
    | x ->
            let hd = String.sub x 0 1 in
            let tl = String.sub x 1 (String.length x - 1) in
            int_of_string hd :: to_list tl

let readlines =
    let rec read acc =
        let line = In_channel.input_line In_channel.stdin in
        match line with
        | None -> acc
        | Some line ->
                let acc = to_list line :: acc in
                read acc in
    read []

let rec invert = function
    | [] -> []
    | 0 :: tl -> 1 :: invert tl
    | 1 :: tl -> 0 :: invert tl
    | _ -> raise (Invalid_argument "invalid value")

let main =
    let lines = readlines in
    let rotated = transpose lines in
    let g = gamma rotated in
    let e = invert g in
    let to_binary x = List.fold_left (fun acc x -> acc ^ (Int.to_string x)) "" x in
    let to_int x = int_of_string ("0b" ^ to_binary x) in
    let g = to_int g in
    let e = to_int e in
    g * e

let () =
    printf "wtf: %d\n" main
