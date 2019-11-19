type t = (string, string) Hashtbl.t

let empty = Hashtbl.create 10

let value r c = Hashtbl.find r c

let add_column r c v = Hashtbl.add r c v; r

let update r c v = Hashtbl.replace r c v; r

let to_hasht r = r