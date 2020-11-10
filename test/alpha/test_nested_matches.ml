open Core

open Matchers
open Rewriter

open Matchers.Alpha

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run ?(configuration = configuration) (module M : Matchers.Matcher) source match_template ?rule rewrite_template =
  let open Language in
  let rule =
    match rule with
    | Some rule -> Rule.Alpha.create rule |> Or_error.ok_exn
    | None -> Rule.Alpha.create "where true" |> Or_error.ok_exn
  in
  M.all ~configuration ~template:match_template ~source
  |> List.filter ~f:(fun { Match.environment; _ } -> Rule.Alpha.(sat @@ apply rule environment))
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

(* segfault:
   echo 'foo(bar(baz(qux)))' | ./comby ':[[x]]' 'yo' -stdin -match-only
   echo 'foo(bar(baz(qux)))' | ./comby ':[x~\w+]' 'yo' -stdin -match-only
   broken:
   echo 'foo(bar(baz(qux)))' | ./comby '...(...)' 'yo' -stdin -match-only
*)
let%expect_test "nested_matches" =
  let source = {|
a(
   b(
      c(
        d(e)
       )
    )
 )
|} in
  let match_template = {|:[[f]](:[x])|} in
  let rewrite_template = {|hi|} in

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|(foo)|}]
