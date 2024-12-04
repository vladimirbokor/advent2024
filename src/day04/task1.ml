let read_grid filename =
  Core.In_channel.read_lines filename
  |> List.map (
    fun line -> Array.init (String.length line) (String.get line))
  |> Array.of_list


let get_word grid coords =
  let length = Array.length grid in
  coords 
  |> List.map (
    fun (x, y) -> 
      if x < 0 || y < 0 || x >= length || y >= length then
        '.'
      else
        grid.(x).(y)
    )
  |> Core.String.of_char_list

let get_valid_combinations grid x y =
  [
    get_word grid [(x, y); (x+1, y); (x+2, y); (x+3, y)]; 
    get_word grid [(x, y); (x-1, y); (x-2, y); (x-3, y)];
    get_word grid [(x, y); (x, y+1); (x, y+2); (x, y+3)];
    get_word grid [(x, y); (x, y-1); (x, y-2); (x, y-3)];
    get_word grid [(x, y); (x+1, y+1); (x+2, y+2); (x+3, y+3)]; 
    get_word grid [(x, y); (x-1, y+1); (x-2, y+2); (x-3, y+3)]; 
    get_word grid [(x, y); (x+1, y-1); (x+2, y-2); (x+3, y-3)]; 
    get_word grid [(x, y); (x-1, y-1); (x-2, y-2); (x-3, y-3)]; 
  ]
  |> List.filter (fun word -> word = "XMAS")
  |> List.length

let sum_valid_combinations grid = 
  let result = ref 0 in
  let n = Array.length grid and m = Array.length grid.(0) in
  for i = 0 to n do
    for j = 0 to m do
      result := !result + get_valid_combinations grid i j
    done
  done;
  !result

let () =
  let grid = read_grid "assets/day04/input.txt" in
  Printf.printf "%d" (sum_valid_combinations grid)