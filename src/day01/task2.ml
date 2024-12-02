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

let count_frequencies lst =
  let table = Hashtbl.create 1000 in
  List.iter (fun x ->
    let count = Hashtbl.find_opt table x |> Option.value ~default:0 in
    Hashtbl.replace table x (count + 1)
  ) lst;
  table

let calculate_total_score col1 freq_table = 
  List.fold_left
    (
      fun acc value ->
        let count = Hashtbl.find_opt freq_table value |> Option.value ~default:0 in
        acc + count * value
    )
    0
    col1
  
let () = 
  let filename = "assets/day01/input.txt" in
  let lines = Core.In_channel.read_lines filename in
  let col1, col2 = parce_lines lines in
  let freq_table = count_frequencies col2 in
  Printf.printf "%d" (calculate_total_score col1 freq_table)