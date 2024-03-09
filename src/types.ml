(** This module provides the types used in this project *)


(** Policy rule *)
type rule = At_least of int | Every of int * int

(** Policy *)
type policy = Only of rule | First_of of rule * rule | Last_of of rule * rule | Bounded of rule * rule * rule

(** Module profile *)
type profile = { name: string; token: string; policy: policy; evict: bool; sub: profile list }

(** Future schedule *)
type future = Future of int * int | Planned of int

(** Schedule *)
type schedule = At of int | Require of int | Any of int * int | All of int * int | Threshold of int * future * int
