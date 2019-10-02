type match_kind =
  | Exact
  | Fuzzy

type t =
  { match_kind : match_kind
  ; significant_whitespace: bool
  ; tight_matching : bool
  }

let create ?(match_kind = Fuzzy) ?(tight_matching = false) ?(significant_whitespace = false) () =
  { match_kind
  ; significant_whitespace
  ; tight_matching
  }
