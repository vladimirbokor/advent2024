let sum_mul_products str =
  let regexp = Re.compile (Re.Perl.re {|mul\((\d+),(\d+)\)|}) in
  let matches = Re.all regexp str in
  List.fold_left (fun acc group ->
    let first_num = int_of_string (Re.Group.get group 1) in
    let second_num = int_of_string (Re.Group.get group 2) in
    let product = first_num * second_num in
    acc + product
  ) 0 matches

let () = 
  "assets/day03/input.txt"
  |> open_in
  |> input_line
  |> sum_mul_products
  |> Printf.printf "Final result: %d\n"