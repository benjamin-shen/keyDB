(**
   Parsing of user commands.
*)

(** [filename] represents the name of a file representing a database table. *)
type filename = string

(** [key] represents the last row key of a table. *)
type key = int

(** [column] represents a table column. *)
type column = string

(** [value] represents a row value. *)
type value = string

(** [operator] represents a comparison operator. *)
type operator = LT | LTE | EQ | NE | GT | GTE

(** [condition] represents a select condition. *)
type condition = (column * operator * value)

(** The type [table_command] represents a user command that is 
    operated on a particular table. *)
type table_command =
  | Select of column list*condition list (* select cols where conditions *)
  | SelectStar of condition list (* select * where conditions *)
  | Insert of value list   (* insert vals *)
  | Remove of key list     (* remove keyval  *)
  | Add    of column list  (* add col *)
  | Delete of column list  (* del col *)
  | Update of {key:key;    (* update keyval col newval *)
               col:column;
               value:string}
  | Sum    of column       (* sum col *)
  | Count  of column       (* count col *)
  | CountNull of column    (* count_null col *)

(** The type [command] represents a user command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Log
  | Undo
  | Quit
  | Help
  | Create of {file:filename;        (* create table cols*)
               cols:column list}
  | Drop   of filename               (* drop table *)
  | In     of filename*table_command (* in filename command *)

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed of string

(** [parse str] parses a user's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.

    Requires: [str] matches regex "\[a-zA-Z0-9* _.<>!=&\]+".

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command cannot be parsed. *)
val parse : string -> command

(** [help] returns the help message string. *)
val help : unit -> string