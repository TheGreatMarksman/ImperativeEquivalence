open! Core
open! Imp

let rec unroll_1
    (s:statement)
  : statement =
  match s with
  | Skip -> Skip
  | Assign (str, a) -> Assign (str, a)
  | If (b, s1, s2) ->
    if bexp_semantics b (fun _ -> 0) then
      unroll_1 s1
    else
      unroll_1 s2
  | Seq (s1, s2) ->
    Seq (unroll_1 s1, unroll_1 s2)
  | While (b, st) ->
    if bexp_semantics b (fun _ -> 0) then
      let st' = unroll_1 st in
      Seq (st', (While (b, st')))
    else
      Skip


let btrue : bexp = BVal true
let bfalse : bexp = BVal false
let bless_or_equal : bexp = BLe ((AId "a"), (AVal 8))
let less_or_equal_6 : bexp = BLe ((AId "a"), (AVal 5))

let a5 : statement = Assign("a", (AVal 5))
let b4 : statement = Assign("b", (AVal 4))

let simplif : statement = If (btrue, Skip, Skip)
let complif : statement = If(btrue, simplif, Skip)

let a_plus_equals_1 : statement = (Assign ("a", (APlus((AId "a"), (AVal 1)))))

let while_loop : statement = Seq (a5, (While(bless_or_equal, a_plus_equals_1)))
let simple_while : statement = Seq (a5, (While(less_or_equal_6, a_plus_equals_1)))


let a_b_store : store = 
  fun x ->
    match x with
    | "a" -> 1
    | "b" -> 2
    | _ -> 0

(*
let b_c_store : store = 
  fun x ->
    match x with
    | "b" -> 2
    | "c" -> 3
    | _ -> 0
*)

let a_b_diff_store : store = 
  fun x ->
    match x with
    | "a" -> 10
    | "b" -> 12
    | _ -> 0

let%test_unit "unroll_1 skip" =
      [%test_eq: statement]
        (unroll_1 Skip)
        Skip

let%test_unit "unroll_1 assign" =
        [%test_eq: statement]
          (unroll_1 a5)
          a5

let%test_unit "unroll_1 if 1" =
        [%test_eq: statement]
          (unroll_1 (If ((BVal true), Skip, Skip)))
          Skip
          
let%test_unit "unroll_1 if 2" =
            [%test_eq: statement]
              (unroll_1 (If (btrue, complif, Skip)))
              Skip

let%test_unit "unroll_1 simple seq" =
              [%test_eq: statement]
                (unroll_1 (Seq(Skip, Skip)))
                (Seq (Skip, Skip))

let%test_unit "unroll_1 seq 1" =
              [%test_eq: statement]
                (unroll_1 (Seq(complif, Seq(simplif, Skip))))
                (Seq(Skip, (Seq(Skip, Skip))))

let%test_unit "unroll_1 seq 2" =
                [%test_eq: statement]
                  (unroll_1 (Seq(complif, Seq(simplif, Skip))))
                  (Seq(Skip, (Seq(Skip, Skip))))

let%test_unit "unroll_1 while" =
                  [%test_eq: statement]
                    (unroll_1 (While(btrue, complif)))
                    (Seq(Skip, While((BVal true), Skip)))

let rec replace_whiles
    (s:statement)
  : statement =
  match s with
  | Skip -> Skip
  | Assign (str, a) -> Assign (str, a)
  | If (b, s1, s2) ->
    if bexp_semantics b (fun _ -> 0) then
      replace_whiles s1
    else
      replace_whiles s2
  | Seq (s1, s2) ->
    Seq (replace_whiles s1, replace_whiles s2)
  | While _ ->
    Assign ("PANIC", AVal 0)

let rec unroll
    (n:int)
    (s:statement)
  : statement =
  if n > 0 then
    unroll (n-1) (unroll_1 s)
  else
    if n = 0 then
      (* What assignment description says *)
      (* Assign ("PANIC", AVal 0) *)

      (* this replaces all whiles with panic *)
      replace_whiles s
    else
      failwith "ERROR: unroll: n cannot be negative"

let%test_unit "unroll skip" =
      [%test_eq: statement]
        (unroll 1 Skip)
        Skip

let%test_unit "unroll assign" =
        [%test_eq: statement]
          (unroll 1 a5)
          a5

let%test_unit "unroll if 1" =
        [%test_eq: statement]
          (unroll 1 (If ((BVal true), Skip, Skip)))
          Skip
          
let%test_unit "unroll if 2" =
            [%test_eq: statement]
              (unroll 1 (If (btrue, complif, Skip)))
              Skip

let%test_unit "unroll simple seq" =
              [%test_eq: statement]
                (unroll 1 (Seq(Skip, Skip)))
                (Seq (Skip, Skip))

let%test_unit "unroll seq 1" =
              [%test_eq: statement]
                (unroll 1 (Seq(complif, Seq(simplif, Skip))))
                (Seq(Skip, (Seq(Skip, Skip))))

let%test_unit "unroll seq 2" =
                [%test_eq: statement]
                  (unroll 1 (Seq(complif, Seq(simplif, Skip))))
                  (Seq(Skip, (Seq(Skip, Skip))))

let%test_unit "unroll while" =
                  [%test_eq: statement]
                    (unroll 1 (While(btrue, complif)))
                    (Seq(Skip, Assign ("PANIC", AVal 0)))
                    
let%test_unit "unroll 2 while" =
                    [%test_eq: statement]
                      (unroll 2 (While(btrue, (While(btrue, Skip)))))
                      (
                        Seq(
                          (Seq(Skip, Seq(Skip, Assign ("PANIC", AVal 0)))),
                          Seq (
                            (Seq(Skip, Seq(Skip, Assign ("PANIC", AVal 0)))),
                            Assign ("PANIC", AVal 0)
                          )
                        )
                      )



let rec panics
    (s:statement)
  : bool =
  match s with
  | Skip -> false
  | Assign (str, _) ->
    if String.compare str "PANIC" = 0 then
      true
    else
      false
  | If (b, s1, s2) ->
    if bexp_semantics b (fun _ -> 0) then
      panics s1
    else
      panics s2
  | Seq (s1, s2) ->
    if panics s1 || panics s2 then
      true
    else
      false
  | While _ ->
    failwith "ERROR: panics: should not have any while statements"

let%test_unit "panics assign" =
    [%test_eq: bool]
      (panics (unroll 1 a5))
      false
      
let%test_unit "panics if" =
        [%test_eq: bool]
          (panics (unroll 1 (If (btrue, complif, Skip))))
          false

let%test_unit "panics simple seq" =
          [%test_eq: bool]
            (panics (unroll 1 (Seq(Skip, Skip))))
            false

let%test_unit "panics seq 1" =
          [%test_eq: bool]
            (panics (unroll 1 (Seq(complif, Seq(simplif, Skip)))))
            false

let%test_unit "panics unroll while" =
              [%test_eq: bool]
                (panics (unroll 1 (While(btrue, complif))))
                true
                
let%test_unit "panics unroll 2 while" =
                [%test_eq: bool]
                  (panics (unroll 2 (While(btrue, (While(btrue, Skip))))))
                  true
 
let%test_unit "panics unroll 1 simple_while" =
  [%test_eq: bool]
    (panics (unroll 1 simple_while))
    true   

let rec string_is_in_list
    (s: string)
    (l: string list)
  : bool =
  match l with
  | head::tail ->
    if String.equal s head then
      true
    else
      string_is_in_list s tail
  | [] -> false

let rec is_subset_of
    (l1: string list)
    (l2: string list)
  : bool =
  match l1 with
  | head::tail ->
    if string_is_in_list head l2 then
      is_subset_of tail l2
    else
      false
  | [] ->
    true

let%test_unit "is_subset_of true" =
    [%test_eq: bool]
      (is_subset_of ["a"; "b"] ["b"; "a"; "c"])
      true

let%test_unit "is_subset_of false" =
      [%test_eq: bool]
        (is_subset_of ["b"; "a"; "c"] ["a"; "b"])
        false

let rec concat_unique
    (s1: string list)
    (s2: string list)
  : string list =
  match s2 with
  | head::tail ->
    if (string_is_in_list head s1) then
      concat_unique s1 tail
    else
      concat_unique (s1 @ [head]) tail 
  | [] -> s1

let%test_unit "concat_unique" =
  [%test_eq: string list]
    (concat_unique ["a"; "b"] ["a"; "c"])
    ["a"; "b"; "c"]

let%test_unit "concat_unique 2" =
    [%test_eq: string list]
      (concat_unique ["a"; "b"] [])
      ["a"; "b"]

(* Like statement_semantics but also returns list of variables modified/added*)
let rec descriptive_statement_semantics
    (s:statement)
    (store:store)
  : store * string list =
  begin match s with
  | Skip -> (store, [])
  | Assign (v,a) ->
    let i = aexp_semantics a store in
    (update_store store v i, [v])
  | Seq (s1,s2) ->
    (* get new store and list from s1 *)
    let result1 = descriptive_statement_semantics s1 store in
    (* pass along the modified store and evaluate s2 *)
    let result2 = descriptive_statement_semantics s2 (fst result1) in
    (* append the modified variables from result1 to the new list *)
    let (a, b) = result2 in
    (a, concat_unique (snd result1) b)

  | If (b,s1,s2) ->
    if bexp_semantics b store then
      descriptive_statement_semantics s1 store
    else
      descriptive_statement_semantics s2 store
  | While (b,s') ->
    if bexp_semantics b store then
      descriptive_statement_semantics (Seq (s',s)) store
    else
      (store, [])
  end
  
let%test_unit "descriptive_statement_semantics assign" =
  [%test_eq: string list]
    (snd (descriptive_statement_semantics a5 (fun _ -> 0)))
    (["a"])

let%test_unit "descriptive_statement_semantics seq" =
    [%test_eq: string list]
      (snd (descriptive_statement_semantics (Seq(a5, b4)) (fun _ -> 0)))
      (["a"; "b"])

let%test_unit "descriptive_statement_semantics if" =
      [%test_eq: string list]
        (snd (descriptive_statement_semantics (If(btrue, a5, b4)) (fun _ -> 0)))
        (["a"])

let%test_unit "descriptive_statement_semantics else" =
          [%test_eq: string list]
            (snd (descriptive_statement_semantics (If(bfalse, a5, b4)) (fun _ -> 0)))
            (["b"])

let%test_unit "descriptive_statement_semantics while" =
            [%test_eq: string list]
              (snd (descriptive_statement_semantics while_loop (fun _ -> 0)))
              (["a"])
            

let eval_on_a
    (s:statement)
    (store:store)
  : int =
  let store' = statement_semantics s store in
  store' "a"

let%test_unit "eval_on_a" =
  [%test_eq: int]
    (eval_on_a while_loop (fun _ -> 0))
    (9)

let rec input_output_equiv
    (st1: store)
    (st2: store)
    (vars: string list)
  : bool =
  match vars with
  | head::tail ->
    if (st1 head) = (st2 head) then
      input_output_equiv st1 st2 tail
    else
      false
  | [] -> true

(* l1 and l2 are the lists of variables modified/added from descriptive_statement_semantics *)
let store_equiv
    (st1:store)
    (l1: string list)
    (st2:store)
    (l2: string list)
  : bool =
  if (is_subset_of l1 l2) && (is_subset_of l2 l1) then
    input_output_equiv st1 st2 l1
  else
    false

let%test_unit "store_equiv true" =
    [%test_eq: bool]
      (store_equiv a_b_store ["a"; "b"] a_b_store ["a"; "b"])
      true

let%test_unit "store_equiv false" =
      [%test_eq: bool]
        (store_equiv a_b_store ["a"; "b"] a_b_diff_store ["a"; "b"])
        false


let bounded_equiv_on
    (n:int) (*this int will always be non-negative*)
    (s1:statement)
    (s2:statement)
    (st:store)
  : bool =
  let s1' = unroll n s1 in
  let s2' = unroll n s2 in
  if (panics s1' || panics s2') then
    true
  else
    let (st1, l1) = descriptive_statement_semantics s1' st in
    let (st2, l2) = descriptive_statement_semantics s2' st in
    store_equiv st1 l1 st2 l2

let%test_unit "bounded_equiv_on panic" =
    [%test_eq: bool]
    (bounded_equiv_on 1 (Assign ("PANIC", AVal 0)) (Assign ("PANIC", AVal 0)) (fun _ -> 0))
    true

let%test_unit "bounded_equiv_on if" =
    [%test_eq: bool]
    (bounded_equiv_on 2 simplif complif (fun _ -> 0))
    true

let%test_unit "bounded_equiv_on assign" =
    [%test_eq: bool]
    (bounded_equiv_on 1 (If(btrue, a5, b4)) a5 (fun _ -> 0))
    true

let%test_unit "bounded_equiv_on seq" =
    [%test_eq: bool]
    (bounded_equiv_on 1 (Seq(a5, b4)) (Seq(b4, a5)) (fun _ -> 0))
    true

let%test_unit "bounded_equiv_on seq false" =
    [%test_eq: bool]
    (bounded_equiv_on 1 (Seq(a5, b4)) b4 (fun _ -> 0))
    false


(* Double check this, since it panics resulting in being true when compared with anything *)
let%test_unit "bounded_equiv_on while" =
    [%test_eq: bool]
    (bounded_equiv_on 5 while_loop (Assign("a", (AVal 9))) (fun _ -> 0))
    true

let first_value_of_store_list
    (l: store list)
    (s: string)
  : int = 
  match l with
  | head::_ ->
      head s
  | [] -> 0

let rec last_value_of_store_list
    (l: store list)
    (s: string)
  : int = 
  match l with
  | [a] -> a s
  | _::tail ->
    last_value_of_store_list tail s
  | [] -> -1

let rec make_combos
    (vars: string list)
    (n: int)
  : ((string * int) list list) =
  match vars with
  | [] -> if n >= 0 then [ [] ] else []
  | v :: rest ->
      let rec loop i acc =
        if i > n then acc
        else
          let tails = make_combos rest (n - i) in
          let with_v = List.map ~f: (fun t -> (v, i) :: t) tails in
          loop (i + 1) (with_v @ acc)
      in
      loop 0 []

let%test_unit "make_combos" =
      [%test_eq: ((string * int) list list)]
      (make_combos ["a"; "b"]  1)
      [[("a", 1); ("b", 0)]; [("a", 0); ("b", 1)]; [("a", 0); ("b", 0)]]

let rec value_from_list
    (s: string)
    (l:(string * int) list)
  : int =
    match l with
    | (s', i)::tail ->
      if String.equal s s' then
        i
      else
        value_from_list s tail
    | [] -> 0

let%test_unit "value_from_list" =
    [%test_eq: int]
    (value_from_list "b" [("a", 0); ("b", 1)])
    1

let list_to_store
    (l:(string * int) list)
    (st: store)
  : store =
  fun x ->
    if st x = -1 then
      value_from_list x l
    else
      st x

let%test_unit "list_to_store" =
      [%test_eq: int]
      ((list_to_store [("a", 1); ("b", 0)] (fun _ -> -1)) "a")
      1

let rec list_to_store_list
      (l:((string * int) list list))
    : store list =
    match l with
    | head::tail ->
      (list_to_store head (fun _ -> -1)) :: (list_to_store_list tail)
    | [] -> []

let%test_unit "list_to_store_list" =
    [%test_eq: int]
    (last_value_of_store_list (list_to_store_list [[("a", 0); ("b", 0)]; [("a", 0); ("b", 1)]; [("a", 1); ("b", 0)]]) "a")
    1



let generate_stores
    (n:int) (*this int will always be non-negative*)
    (vars:string list)
  : store list =
  if n > 0 then
    list_to_store_list (make_combos vars n)
  else
    []

let%test_unit "generate_stores" =
    [%test_eq: int]
    (first_value_of_store_list (generate_stores 1 ["a"; "b"]) "a")
    1



let rec bounded_equiv_on_all
    (m:int)
    (s1: statement)
    (s2: statement)
    (stores: store list)
  : bool =
  match stores with
  | head::tail ->
    if bounded_equiv_on m s1 s2 head then
      bounded_equiv_on_all m s1 s2 tail
    else
      false
  | [] -> bounded_equiv_on m s1 s2 (fun _ -> 0)

let unsound_equiv
    (n:int) (*this int will always be non-negative*)
    (m:int) (*this int will always be non-negative*)
    (s1:statement)
    (s2:statement)
  : bool =
    let s1' = unroll n s1 in
    let s2' = unroll n s2 in
    let (_, ss1) = descriptive_statement_semantics s1' (fun _ -> 0) in
    let (_, ss2) = descriptive_statement_semantics s2' (fun _ -> 0) in
    let ss = concat_unique ss1 ss2 in
    let stores = generate_stores n ss in
    bounded_equiv_on_all m s1 s2 stores



let%test_unit "unsound_equiv" =
  [%test_eq: bool]
    (unsound_equiv
       2
       2
       (While(BVal true, Skip))
       (Assign("X",AVal 2)))
    true

let%test_unit "unsound_equiv" =
  [%test_eq: bool]
    (unsound_equiv
       2
       2
       (Assign("Y",AVal 2))
       (Assign("X",AVal 2)))
    false
