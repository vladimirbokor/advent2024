let parse_line line = 
  match String.split_on_char ' ' line with
    | hd :: tail -> 
      let lst = List.hd (List.rev tail) in
      (int_of_string hd, int_of_string lst)
    | [] -> failwith "Invalid line: Empty or improperly formatted"


let parce_lines lines = 
  List.fold_left
    (fun (list1, list2) line ->
      let hd, lst = parse_line line in
      (hd :: list1, lst :: list2))
    ([], [])
    lines

let find_difference col1 col2 = 
  let s_col1, s_col2 = List.sort compare col1, List.sort compare col2 in
  List.fold_left2 
    (
      fun acc val_1 val_2 ->
        acc + Int.abs (val_1 - val_2)
    ) 
    0
    s_col1
    s_col2

let () = 
  let filename = "assets/day01/input.txt" in
  let lines = Core.In_channel.read_lines filename in
  let col1, col2 = parce_lines lines in
  Printf.printf "%d" (find_difference col1 col2)