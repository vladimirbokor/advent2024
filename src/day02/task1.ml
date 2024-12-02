let parse_line line = 
  String.split_on_char ' ' line
  |> List.map int_of_string
  
let rec is_safe line is_increasing = 
  match line with
    | [] | [_] -> true
    | hd :: (second :: _ as tail) -> 
      let diff = hd - second in
      if (Int.abs diff >= 1) && (Int.abs diff <= 3) && (diff < 0 == is_increasing) then
        is_safe tail is_increasing
      else
        false

let is_line_safe line = 
  let is_increasing = List.hd line < List.nth line 1 in
  is_safe line is_increasing

let parce_lines lines = 
  lines
  |> List.filter (fun line -> is_line_safe (parse_line line))
  |> List.length

let () = 
  let filename = "assets/day02/input.txt" in
  let lines = Core.In_channel.read_lines filename in
  Printf.printf "%d" (parce_lines lines)