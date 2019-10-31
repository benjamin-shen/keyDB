(**
   Parsing of user commands.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    user command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original user command.

    An [object_phrase] is not permitted to be the empty list.

    TODO: edit this specification *)
type filename = string
type key = int
type column = string
type value = string
type conditions = string list

(** The type [table_command] represents a user command that is 
    operated on a particular table. *)
type table_command =
  | Select of column list*conditions (* select cols where conditions *)
  | Insert of value list         (* insert vals *)
  | Remove of key list           (* remove keyval  *)
  | Add    of column list        (* add col *)
  | Delete of column list        (* del col *)
  | Update of {key:key;          (* update keyval col newval *)
               col:column;
               value:string}
  | Sum    of column             (* sum col *)
  | Count  of column             (* count col *)
  | Count_Null of column         (* count_null col *)
(** The type [command] represents a user command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Log
  | Undo
  | Quit
  | Help
  | Create of {file:filename;    (* create table cols*)
               cols:column list}
  | Drop   of filename           (* drop table *)
  | In     of filename*table_command   (* in filename command *)

(** Raised when an empty command is parsed. *)
exception Empty

(** TODO *)
type err = string
(** Raised when a malformed command is encountered. *)
exception Malformed of err

(** [parse str] parses a user's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val parse : string -> command

(*TODO*)
val help : unit -> string