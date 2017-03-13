(********************************************************************)
(* ocaml-scribble - Syntax file                                     *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

(* Positionning *)

type pos = int * int
type info = pos * pos

(* Errors *)
exception Syntax_error of string*info
exception Parse_error of string*info

(************************************)
(* Abstract syntax for source files *)
(************************************)

type role_name = string

type message_op = string

type message_payload = ( string * string ) list

type message_sig = message_op * message_payload

type as_role = role_name

type role = role_name
type roles = role_name list

type parameters= message_op list

type as_global_protocol_body =
  | GASEnd
  | GASMsg of (info * message_sig * role_name * role_name)
  | GASSeq of (as_global_protocol_body * as_global_protocol_body)
  | GASChoice of (info * role_name * (as_global_protocol_body list))
  | GASPar of (info * (as_global_protocol_body list))
  | GASRec of (info * string * as_global_protocol_body)
  | GASCont of (info * string)
  | GASInterrupt of (info * as_global_protocol_body * ((role_name * message_sig) list))

type as_global =
    string * parameters * roles * as_global_protocol_body

type as_local_protocol_body =
  | LASEnd
  | LASSend of (info * message_sig * role_name)
  | LASRecv of (info * message_sig * role_name)
  | LASSeq of (as_local_protocol_body * as_local_protocol_body)
  | LASChoice of (info * role_name * (as_local_protocol_body list))
  | LASPar of (info * (as_local_protocol_body list))
  | LASRec of (info * string * as_local_protocol_body)
  | LASCont of info * string
  | LASInterrupt of (info * as_local_protocol_body * ((role_name * message_sig) list))

type as_local =
    string * role_name * parameters * roles * as_local_protocol_body

type as_protocol =
    Localast of as_local
  | Globalast of as_global

type as_import =
    (string * ((string * string) option) * (string option))

type ast =
    FileAS of (as_import list * as_protocol list)
    
