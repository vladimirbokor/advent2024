
let parse_rule line = 
  match String.split_on_char '|' line with
    | [a ; b] -> int_of_string a, int_of_string b
    | _ -> failwith "Error"

let parse_rules lines =
  let table = Hashtbl.create 100 in

  let add_pair (key, value) =
    match Hashtbl.find_opt table key with
    | Some values -> Hashtbl.replace table key (value :: values)
    | None -> Hashtbl.add table key [value]
  in

  List.iter (fun line ->
    line
    |> parse_rule
    |> add_pair
  ) lines;

  table

let parse_pages lines =
  lines
  |> List.map (fun line ->
    line 
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list
  )

let parse_file filename =
  let all_lines = Core.In_channel.read_all filename in
  let idx = Core.String.substr_index_exn all_lines ~pattern:"\n\n" in
  let rules = Core.String.slice all_lines 0 idx in
  let pages = Core.String.slice all_lines (idx + 2) 0 in

  parse_rules (String.split_on_char '\n' rules), 
  parse_pages (String.split_on_char '\n' pages)

let find_with_default tbl key default =
  match Hashtbl.find_opt tbl key with
  | Some value -> value
  | None -> default

let is_correct rules pages =
  let result = ref true in
  for i = 1 to (Array.length pages - 1) do
    let current_value = pages.(i) in
    let current_value_rules = find_with_default rules current_value [] in
    for j = 0 to (i - 1) do
      let checked_value = pages.(j) in
      if List.mem checked_value current_value_rules then 
        result := false
    done
  done;
  !result

let get_middle arr =
  let n = Array.length arr in
  arr.(n / 2)

let () =
  let rules, pages = parse_file "assets/day05/input.txt" in
  pages
  |> List.fold_left (
    fun acc page ->
      if is_correct rules page then
        acc + get_middle page
      else
        acc
  ) 0 
  |> Printf.printf "%d"
