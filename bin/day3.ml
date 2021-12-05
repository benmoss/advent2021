open Stdio

let most_frequent = function
    | [] -> None
    | x ->
            let sum acc = function
            | 0 -> acc-1
            | 1 -> acc+1
            | _ -> raise (Invalid_argument "invalid value") in
            let result = List.fold_left sum 0 x in
            if result == 0 then None else if result > 0 then Some 1 else Some 0

let least_frequent l =
  match most_frequent l with
  | None -> Some 0
  | Some x -> if x = 0 then Some 1 else Some 0

let rec gamma = function
    | [] -> []
    | hd :: tl ->
        let hd = most_frequent hd in
        match hd with
        | Some hd -> hd :: gamma tl
        | None -> raise (Invalid_argument "ambiguous")

let rec transpose = function
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let rec gas_level f l =
  match l with
  | [] -> []
  | hd :: _ ->
      let mf = match f hd with
      | Some x -> x
      | None -> 1 in
      let rotated = transpose l in
      if (List.length rotated) = 1 then (List.hd rotated) else
      let filtered = List.filter (fun x -> (List.hd x) = mf) rotated in
      let rotated = transpose filtered in
      mf :: gas_level f (List.tl rotated)

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

let to_int x =
    let to_binary x = List.fold_left (fun acc x -> acc ^ (Int.to_string x)) "" x in
    let to_int x = int_of_string ("0b" ^ to_binary x) in
    to_int x

let main =
    let lines = readlines in
    let rotated = transpose lines in
    let g = gamma rotated in
    let e = to_int (invert g) in
    let g = to_int g in
    let o = to_int (gas_level most_frequent rotated) in
    let co2 = to_int (gas_level least_frequent rotated) in
    (g * e, o, co2)

let () =
  let (power, oxygen, co2) = main in
    printf "power: %d\noxygen: %d\nco2: %d\n" power oxygen co2
