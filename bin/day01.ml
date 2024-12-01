open Advent_of_code_2024

let input = Advent.read_lines "input.txt"

let parse_lists input =
  input
  |> List.filter_map (fun line ->
         match
           line |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
         with
         | [ a; b ] -> Some (int_of_string a, int_of_string b)
         | _ -> None)
  |> List.split

(* Part 1 *)
let total_min_distance input =
  input |> parse_lists |> fun (list1, list2) ->
  List.combine (List.sort compare list1) (List.sort compare list2)
  |> List.fold_left (fun acc (a, b) -> acc + Int.abs (a - b)) 0

let () = input |> total_min_distance |> string_of_int |> print_endline

(* Part 2 *)
module IntMap = Map.Make (Int)

let calculate_similarity input =
  let get_freq map x = Option.value (IntMap.find_opt x map) ~default:0 in
  let count_freqs list =
    List.fold_left
      (fun acc x -> IntMap.add x (succ (get_freq acc x)) acc)
      IntMap.empty list
  in
  let list1, list2 = parse_lists input in
  let freq = count_freqs list2 in
  List.fold_left (fun acc x -> acc + (get_freq freq x * x)) 0 list1

let () = input |> calculate_similarity |> string_of_int |> print_endline
