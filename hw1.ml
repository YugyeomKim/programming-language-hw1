exception Not_implemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec sum n = if n == 1 then 1 else n + sum (n - 1)
let rec power x n = if n == 0 then 1 else x * power x (n - 1)
let rec gcd m n =
  if m == 0 then n
  else if n == 0 then m
  else if m > n then gcd (m - n) n
  else gcd m (n - m)
let rec combi n k =
  if k == 0 || k == n then 1 else combi (n - 1) (k - 1) + combi (n - 1) k
let rec sum_tree t =
  match t with
  | Leaf value -> value
  | Node (left, value, right) -> sum_tree left + value + sum_tree right
let rec depth t =
  match t with
  | Leaf _ -> 0
  | Node (left, _, right) ->
      let left_depth = depth left in
      let right_depth = depth right in
      1 + if left_depth > right_depth then left_depth else right_depth

let rec bin_search t x =
  match t with
  | Leaf value -> value == x
  | Node (left, value, right) ->
      if value == x then true
      else if value > x then bin_search left x
      else bin_search right x
let rec postorder t =
  match t with
  | Leaf value -> [ value ]
  | Node (left, value, right) -> postorder left @ postorder right @ [ value ]
let rec max l =
  match l with
  | [] -> 0
  | first :: rest -> if first > max rest then first else max rest
let rec list_add l1 l2 =
  match (l1, l2) with
  | [], l2 -> l2
  | l1, [] -> l1
  | first1 :: rest1, first2 :: rest2 ->
      (first1 + first2) :: list_add rest1 rest2
let rec insert m l =
  match l with
  | [] -> [ m ]
  | first :: rest -> if m < first then m :: l else first :: insert m rest
let rec insort l =
  match l with [] -> [] | first :: rest -> insert first (insort rest)

let rec compose f g =
  let h x = g (f x) in
  h
let rec curry f m n = f (m, n)
let rec uncurry f p = match p with m, n -> f m n
let rec multifun f n = if n == 1 then f else compose f (multifun f (n - 1))

let rec ltake l n =
  if n == 0 then []
  else match l with [] -> [] | first :: rest -> first :: ltake rest (n - 1)
let rec lall f l =
  match l with [] -> true | first :: rest -> f first && lall f rest
let rec lmap f l =
  match l with [] -> [] | first :: rest -> f first :: lmap f rest
let rec lrev l =
  match l with [] -> [] | first :: rest -> lrev rest @ [ first ]
let rec lflat l = match l with [] -> [] | first :: rest -> first @ lflat rest
let rec lzip l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> []
  | first1 :: rest1, first2 :: rest2 -> (first1, first2) :: lzip rest1 rest2
let rec split l =
  match l with
  | [] -> ([], [])
  | first :: [] -> ([ first ], [])
  | first :: second :: rest -> (
      match split rest with l1, l2 -> (first :: l1, second :: l2))
let rec cartprod l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> []
  | first1 :: rest1, first2 :: rest2 ->
      ((first1, first2) :: cartprod [ first1 ] rest2) @ cartprod rest1 l2
let rec powerset l =
  match l with
  | [] -> [ [] ]
  | first :: rest -> lmap (fun l -> first :: l) (powerset rest) @ powerset rest
