open Core

open Angstrom

let (|>>) p f =
  p >>= fun x -> return (f x)

let between left right p =
  left *> p <* right

let zero =
  fail ""

let any_char_except_parser p =
  let rewind = ref false in
  let set_rewind v = rewind := v in
  let get_rewind () = !rewind in
  choice
    [ (p >>= fun _ -> (return (set_rewind true)) >>= fun _ -> fail "bad")
    ; (return () >>= fun _ -> if get_rewind () then fail "rewind" else any_char)
      (* TODO this may need some kind of EOF condition to work for both template and match parsing *)
    ]

let any_allowed_except_parser allowed p =
  let rewind = ref false in
  let set_rewind v = rewind := v in
  let get_rewind () = !rewind in
  choice
    [ (p >>= fun _ -> (return (set_rewind true)) >>= fun _ -> fail "bad")
    ; (return () >>= fun _ -> if get_rewind () then fail "rewind" else allowed)
      (* TODO this needs some kind of EOF condition to work for both template and match parsing *)
    ]

let alphanum =
  satisfy (function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' -> true
      | _ -> false)

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let blank =
  choice
    [ char ' '
    ; char '\t'
    ]

let cons x xs = x :: xs

let many_till p t =
  fix (fun m -> (t *> return []) <|> (lift2 cons p m))

let many1_till p t =
  lift2 cons p (many_till p t)

let skip_unit p =
  p |>> ignore

(* XXX can shortcircuit *)
(* what if you hit a reserved
   seqence "{" and then attempt
   ":[[" and then say "end of
   input" and then move ahead any_char. not good.
   going from longest to shortest works though *)
let any_char_except ~reserved =
  List.fold reserved
    ~init:(return `OK)
    ~f:(fun acc reserved_sequence ->
        option `End_of_input
          (peek_string (String.length reserved_sequence)
           >>= fun s ->
           if s = reserved_sequence then
             return `Reserved_sequence
           else
             acc))
  >>= function
  | `OK -> any_char
  | `End_of_input -> any_char
  | `Reserved_sequence -> fail "reserved sequence hit"

(** must have at least one, otherwise spins on
    the empty string *)
let spaces1 =
  satisfy is_whitespace >>= fun c ->
  (* XXX use skip_while once everything works.
     we don't need the string *)
  take_while is_whitespace >>= fun s ->
  return (Format.sprintf "%c%s" c s)
