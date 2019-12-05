type t = (string * string) list

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
      (* [column_check acc cd] goes through the conditions  *)
      let rec column_check acc' = function
        | [] -> Some acc'
        | (col, op, v)::t ->
          if hcol = col then 
            let row_val = value r col in
            (* Check cond, return none if not satisfied or add col if so*)
            match op with 
            | Command.LT -> if (row_val) < v then 
                column_check (add_column acc' col row_val) t else None
            | Command.LTE -> if (row_val) <= v then 
                column_check (add_column acc' col row_val) t else None
            | Command.EQ -> if (row_val) = v then 
                column_check (add_column acc' col row_val) t else None
            | Command.NE -> if (row_val) <> v then 
                column_check (add_column acc' col row_val) t else None
            | Command.GT -> if (row_val) > v then 
                column_check (add_column acc' col row_val) t else None
            | Command.GTE -> if (row_val) >= v then 
                column_check (add_column acc' col row_val) t else None
          else 
            (* add column and value if not part of condition *)
            column_check (((hcol, value r hcol)::(acc'))) t
      in column_check (empty) cd
  in condition_cols (empty) c 

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