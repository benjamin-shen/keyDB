type t = (string * string) list

let empty = []

let value r c = List.assoc c r

let add_column r c v = r @ [(c,v)]

let build_row cs vs = 
  assert (List.length vs = List.length cs);
  let rec row_builder acc vals = function 
    | [] -> acc
    | h::t -> row_builder ((h, List.hd vals)::acc) (List.tl vals) t
  in row_builder empty (List.rev vs) (List.rev cs)

let rec update r c v = match r with
  | [] -> []
  | h::t -> if (fst h)=c then (c,v)::update t c v else h::update t c v

let rec to_csv r = 
  match r with
  | [] -> ""
  | (_,h)::t -> let tail = to_csv t in 
    h ^ (if tail="" then "" else ",") ^ tail