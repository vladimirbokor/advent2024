let get_equation line = 
  match String.split_on_char ':' line with
    | result :: [values] -> 
      let raw_values = String.split_on_char ' ' (String.trim values) in
      int_of_string result, List.map int_of_string raw_values
    | _ -> failwith "Invalid input format"
  
let rec get_possible_results values acc =
  match values with
    | [] -> [acc]
    | hd :: tail -> 
      let add_results = get_possible_results tail (acc + hd) in
      let mul_results = get_possible_results tail (acc * hd) in
      add_results @ mul_results
    
let get_res_for_equation line = 
  let expected, values = get_equation line in
  match values with
    | [] -> 0
    | hd :: tail ->
      get_possible_results tail hd
      |> List.filter (fun res -> res = expected)
      |> List.length
      |> (fun len -> if len > 0 then expected else 0)

let () = 
  "assets/day07/input.txt"
  |> Core.In_channel.read_lines
  |> List.fold_left (fun acc line-> acc + get_res_for_equation line) 0
  |> Printf.printf "%d"