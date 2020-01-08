(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b, c) = (c, b, a)

let max_tup (a, b, c) = max a (max b c)

let abs x = if (x < 0) then -x else x

let area (a, b) (x, y) = abs ( (x - a) * (y - b) )

let volume (a, b, c) (x, y, z) = abs ( (x - a) * (y - b) * (z - c) )

let equiv_frac (a, b) (x, y) = (b <> 0) && (y <> 0) && (a * y = b * x)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x =
  match x with
  | 0 -> 1
  | _ -> x * (factorial (x - 1))

let rec pow x = function
  | 0 -> 1
  | p -> x * (pow x (p - 1))

let rec tail x num = x mod (pow 10 num)

let rec log x y = if (x > y) then (0) else (1 + log x (y/x))

let rec len x = if (x = 0) then (1) else ((log 10 (abs x)) + 1)

let rec contains sub x =
  if ((tail x (len sub)) = sub) then (true)
  else (
    if (len sub >= len x) then (false) else (contains sub (x / 10))
  )

(*********************)
(* Part 3: Variables *)
(*********************)

type lookup_table = (string * int) list list

let empty_table () : lookup_table = []

let push_scope (table:lookup_table) : lookup_table = []::table

let pop_scope (table:lookup_table) : lookup_table =
  match table with
  | [] -> failwith "No scopes remain!"
  | h::t -> t

let add_var name value (table:lookup_table) : lookup_table =
  match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | h::t -> ((name, value)::h)::t

let rec lookup name (table:lookup_table) =
  let rec search_scope scope =
    match scope with
    | [] -> (false, 0)
    | ((n, v)::t) when n = name -> (true, v)
    | _::t  -> (search_scope t)
  in match table with
  | [] -> failwith "Variable not found!"
  | h::t ->
    let (found, value) = search_scope h in
    if (found) then (value) else (lookup name t)
