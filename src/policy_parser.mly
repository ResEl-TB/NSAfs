(** This module provides the parser for policies *)


%{
  open Types
%}

%start<policy> start

%%

(** A policy encoded in a file name may be suffixed *)
start: policy suffix? EOF { $1 }

(** A policy is a number, a period, or a combination of them *)
policy:
  | number { Only $1 }
  | period { Only $1 }
  | number MIN period { First_of ($1, $3) }
  | number MAX period { Last_of ($1, $3) }
  | period MIN number { First_of ($3, $1) }
  | period MAX number { Last_of ($3, $1) }
  | number MAX period MIN number { Bounded ($1, $3, $5) }

number: NUMBER { At_least $1 }

time: TIME+ { List.fold_left (+) 0 $1 }

(** A period may be shifted by a constant *)
period:
  | time { Every ($1, 0) }
  | time PLUS time { Every ($1, $3) }
  | time MINUS time { Every ($1, -$3) }

suffix: COLON NUMBER {}
