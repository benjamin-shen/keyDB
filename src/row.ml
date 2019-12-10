exception InvalidColumn of string
exception ValueMismatch of int

(** An association list associating column names to values. *)
type t = (Command.column * Command.value) list

let empty = []

let value r c = List.assoc c r

let add_column r c v = r @ [(c,v)]

let delete_column r c = List.filter (fun (col, _) -> col<>c) r

let rec update r c v = match r with
  | [] -> []
  | h::t -> if (fst h)=c then (c,v)::update t c v else h::update t c v

(** [compare_bool rv v op] is the result of comparing [rv] and [v]
    with [op]. *)
let compare_bool rv v = function
  | Command.LT -> rv < v
  | Command.LTE -> rv <= v
  | Command.EQ -> rv = v
  | Command.NE -> rv <> v
  | Command.GT -> rv > v
  | Command.GTE -> rv >= v

(** [condition_cols r c cd acc] goes through the columns in [c], 
    checks if the column from the row satisfies all conditions from [cd], 
    adds the column to the new row [acc] if so,
    otherwise stops the function and returns [None]. *)
let rec condition_cols r c cd acc = 
  match c with
  | [] -> Some acc
  | hcol::tcol -> 
    try
      (** [column_check acc cd] goes through the conditions for a column,
          checks if the column is a part of the condition,
          if not adds the column, if it is manages the condition
          if condition satisfied, add column, otherwise stop and 
          return [None] *)
      let rec column_check acc' = function
        | [] -> condition_cols r tcol cd (add_column acc' hcol (value r hcol))
        | (col, op, v)::t -> 
          if List.mem_assoc col r then
            let rv = value r col in 
            let compare = compare_bool rv v op
            in if compare then column_check acc' t else None
          else raise (InvalidColumn col)
      in if (List.length cd) = 0 then
        condition_cols r tcol cd (add_column acc hcol (value r hcol))
      else column_check acc cd
    with
    | Not_found -> raise (InvalidColumn hcol)

let condition r c cd = condition_cols r c cd empty

let build_row cs vs = 
  let len = List.length cs in
  if List.length vs = len then
    let rec row_builder acc vals = function 
      | [] -> acc
      | h::t -> row_builder ((h, List.hd vals)::acc) (List.tl vals) t
    in row_builder empty (List.rev vs) (List.rev cs)
  else raise (ValueMismatch len)

let rec to_csv r = 
  match r with
  | [] -> ""
  | (_,h)::t -> let tail = to_csv t in 
    h ^ (if tail="" then "" else ",") ^ tail 