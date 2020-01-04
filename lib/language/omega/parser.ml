open Core

open Angstrom

open Ast

let (|>>) p f =
  p >>= fun x -> return (f x)

let alphanum =
  satisfy (function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' -> true
      | _ -> false)

let variable_parser =
  (string Syntax.variable_left_delimiter
   *> (many (alphanum <|> char '_') |>> String.of_char_list)
   <* string Syntax.variable_right_delimiter)

module M =
  Parsers.String_literals.Omega.Escapable.Make(struct
    let delimiter = {|"|}
    let escape = '\\'
  end)

let value_parser =
  M.base_string_literal

let operator_parser =
  choice
    [ string Syntax.equal
    ; string Syntax.not_equal
    ]

let atom_parser =
  choice
    [ (variable_parser >>= fun variable -> return (Variable variable))
    ; (value_parser >>= fun value -> return (String value))
    ]

let rewrite_template_parser =
  value_parser >>= fun value -> return (RewriteTemplate value)
