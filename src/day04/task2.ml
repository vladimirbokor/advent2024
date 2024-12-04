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
    get_word grid [(x-1, y-1); (x, y); (x+1, y+1)]; 
    get_word grid [(x-1, y+1); (x, y); (x+1, y-1)]; 
  ]
  |> List.filter (fun word -> word = "MAS" || word = "SAM")
  |> List.length
  |> (fun len -> if len = 2 then 1 else 0)


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