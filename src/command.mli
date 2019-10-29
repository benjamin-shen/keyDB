(**
   Parsing of user commands.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    user command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original user command.  For example:
    - If the player command is ["CREATE table col1 col2 col3"], then the object 
      phrase is [["table"; "col1"; "col2"; "col3"]].
    - If the player command is ["DROP table"], then the object phrase is
      again [["table"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a user command that is decomposed
    into a verb and possibly an object phrase. The object phrase *)
type command = 
  | In of object_phrase         (* command  keyword *)
  | Where of object_phrase      (* command  keyword *)
  | Select of object_phrase     (* display  data *)
  | Create of object_phrase     (* create table *)
  | Drop of object_phrase       (* drop table *)
  | Insert of object_phrase     (* insert row *)
  | Remove of object_phrase     (* remove row  *)
  | Add of object_phrase        (* add col *)
  | Delete of object_phrase     (* del col *)
  | Update of object_phrase     (* update cell *)
  | Sum of object_phrase        (* sum values *)
  | Count of object_phrase      (* count values *)
  | Count_Null of object_phrase (* count null values *)
  | Log    (* show log *)
  | Undo   (* reverse last command *)
  | Quit   (* quit the DBMS *)

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a user's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    Create  customers   name  "] is [Create ["customers"; "name"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val parse : string -> command