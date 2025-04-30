open! Core

type aexp =
  | AVal of int (*this int will always be non-negative*)
  | AId of string
  | APlus of (aexp * aexp)
  | AMinus of (aexp * aexp)
  | AMult of (aexp * aexp)
[@@deriving eq, ord, show, sexp]

type bexp =
  | BVal of bool
  | BEq of (aexp * aexp)
  | BLe of (aexp * aexp)
  | BNot of bexp
  | BAnd of (bexp * bexp)
[@@deriving eq, ord, show, sexp]

type statement =
  | Skip
  | Assign of (string * aexp)
  | Seq of (statement * statement)
  | If of (bexp * statement * statement)
  | While of (bexp * statement)
[@@deriving eq, ord, show, sexp]

type store = string -> int (*this int will always be non-negative*)

let rec aexp_semantics
    (a:aexp)
    (s:store)
  : int =
  begin match a with
    | AVal i -> i
    | AId i -> s i
    | APlus (a1,a2) ->
      aexp_semantics a1 s + aexp_semantics a2 s
    | AMinus (a1,a2) ->
      aexp_semantics a1 s - aexp_semantics a2 s
    | AMult (a1,a2) ->
      aexp_semantics a1 s * aexp_semantics a2 s
  end

let rec bexp_semantics
    (b:bexp)
    (s:store)
  : bool =
  begin match b with
  | BVal b -> b
  | BEq (a1,a2) -> Int.equal (aexp_semantics a1 s) (aexp_semantics a2 s)
  | BLe (a1,a2) -> (aexp_semantics a1 s) <= (aexp_semantics a2 s)
  | BNot b -> not (bexp_semantics b s)
  | BAnd (b1,b2) -> bexp_semantics b1 s && bexp_semantics b2 s
  end

let update_store
    (store:store)
    (v:string)
    (i:int)
  : store =
  fun v' ->
  if String.equal v v' then i else store v'

let rec statement_semantics
    (s:statement)
    (store:store)
  : store =
  begin match s with
  | Skip -> store
  | Assign (v,a) ->
    let i = aexp_semantics a store in
    update_store store v i
  | Seq (s1,s2) ->
    let store' = statement_semantics s1 store in
    statement_semantics s2 store'
  | If (b,s1,s2) ->
    if bexp_semantics b store then
      statement_semantics s1 store
    else
      statement_semantics s2 store
  | While (b,s') ->
    if bexp_semantics b store then
      statement_semantics (Seq (s',s)) store
    else
      store
  end
