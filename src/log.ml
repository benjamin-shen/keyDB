let log = "log.txt"

let clear () = 
  ignore (Sys.command ("rm " ^ log)); ()

let rec get_log () = 
  (* from https://rosettacode.org/wiki/Read_entire_file#OCaml *)
  let ic = open_in log in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  Bytes.unsafe_to_string b

let write_log line = 
  let empty = get_log () = "" in
  (* from http://www.codecodex.com/wiki/Append_a_string_to_a_file#OCaml *)
  let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 
      log in
  output_string out (if empty then line else "\n" ^ line);
  close_out out

let undo () = "undo not implemented"