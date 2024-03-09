(** This module provides the lexer for time policies *)


{
  open Policy_tokens
}


let number = ['0'-'9']+


rule token = parse
  | number as t 's' { TIME (int_of_string t) }
  | number as t 'm' { TIME (int_of_string t * 60) }
  | number as t 'h' { TIME (int_of_string t * 3600) }
  | number as t 'd' { TIME (int_of_string t * 86400) }
  | number as t 'w' { TIME (int_of_string t * 604800) }
  | number as n { NUMBER (int_of_string n) }
  | "now" { NUMBER 0 }
  | '^' { MAX }
  | '_' { MIN }
  | '+' { PLUS }
  | '-' { MINUS }
  | ':' { COLON }
  | eof { EOF }
