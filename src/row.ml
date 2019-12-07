type t = (string * string) list

exception InvalidCol of string

let empty = []

let value r c = List.assoc c r

let add_column r c v = r @ [(c,v)]

let delete_column r c = List.filter (fun (col, _) -> not (col = c)) r

let rec update r c v = match r with
  | [] -> []
  | h::t -> if (fst h)=c then (c,v)::update t c v else h::update t c v

let condition (r : t) (c : string list) (cd : Command.conditions) = 
  (* [condition_cols acc c] goes through the columns in [c], 
     checks if the column from the row satisfies all conditions from [cd], 
     adds the column to the new row [acc] if so,
     otherwise stops the function and returns none. *)
  let rec condition_cols acc = function
    | [] -> Some acc
    | hcol::tcol -> 
      try
        (* [column_check acc cd] goes through the conditions for a column,
           checks if the column is a part of the condition,
           if not adds the column, if it is manages the condition
           if condition satisfied, add column, otherwise stop and return none *)
        let rec column_check acc' = function
          | [] -> condition_cols (add_column acc' hcol (value r hcol)) tcol
          | (col, op, v)::t ->
            if hcol = col then
              let rv = value r col in
              (* Check cond, return none if not satisfied or check more cond if so*)
              match op with 
              | Command.LT  -> if rv < v then column_check acc' t else None
              | Command.LTE -> if rv <= v then column_check acc' t else None
              | Command.EQ  -> if rv = v then column_check acc' t else None
              | Command.NE  -> if rv <> v then column_check acc' t else None
              | Command.GT  -> print_endline "gt"; if rv > v then column_check acc' t else None
              | Command.GTE -> if rv >= v then column_check acc' t else None
            else 
              (* add column and value if not part of condition *)
              column_check acc' t
        in if (List.length cd) = 0 then
          condition_cols (add_column acc hcol (value r hcol)) tcol
        else column_check (acc) cd
      with
      | Not_found -> raise (InvalidCol hcol)
  in match condition_cols (empty) c with
  | None -> None
  | Some list -> print_string "\n"; Some (List.rev list)

let build_row cs vs = 
  assert (List.length vs = List.length cs);
  let rec row_builder acc vals = function 
    | [] -> acc
    | h::t -> row_builder ((h, List.hd vals)::acc) (List.tl vals) t
  in row_builder empty (List.rev vs) (List.rev cs)



let rec to_csv r = 
  match r with
  | [] -> ""
  | (_,h)::t -> let tail = to_csv t in 
    h ^ (if tail="" then "" else ",") ^ tail 