(** General parameters handling. *)

type loquacity =
  | Quiet
  | Usual
  | Verbose
  | Debug

(* Global parameters *)
let loquacity = ref Usual
let output = ref "truc"
let latex = ref false

(* Description *)
let descr = "Possible options are:"

(* Parameters specification *)
let output_spec =
  (
    "-o",
    Arg.Set_string output,
    "<file> Specify an output filename"
  )

let latex_spec =
  (
    "-latex",
    Arg.Set latex,
    " Print the sdp problem in a latex file"
  )

let quiet_spec =
  (
    "-q",
    Arg.Unit (fun () -> loquacity := Quiet),
    " Write nothing"
  )

let verbose_spec =
  (
    "-v",
    Arg.Unit (fun () -> loquacity := Verbose),
    " Say more"
  )

let debug_spec =
  (
    "-debug",
    Arg.Unit (fun () -> loquacity := Debug),
    " Say even more"
  )

(* *)
let spec = Arg.align
  [ output_spec; latex_spec; quiet_spec; verbose_spec; debug_spec ]
 
let empty_anon_fun s =
  raise (Arg.Bad ( Printf.sprintf "Don't know what to do with %s" s ))

let _ = Arg.parse spec empty_anon_fun descr


