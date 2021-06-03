(********************************************************************)
(* Copyright (C) 2020-2021 Alain Giorgetti, Clotilde Erard and      *)
(*                                          Jérome Ricciardi        *)
(* FEMTO-ST institute                                               *)
(********************************************************************)

(********************************************************************)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1                              *)
(********************************************************************)

(** Extracted contents of ENUM 1.3 adapted to a single file 
Differences are:
  - file.ml -> module File, for several files

  - array__Array.ml -> module Array__Array
  - array__ArraySwap.ml -> module Array__ArraySwap
  - array__ToList.ml -> module Array__ToList

  - arrayExtension__ArrayInjection.ml -> module ArrayExtension__ArrayInjection
  - arrayExtension__Array.ml -> module ArrayExtension__Array
  - arrayExtension__ToList.ml -> module ArrayExtension__ToList
  - arrayExtension__Inc.ml -> module ArrayExtension__Inc

  - list__List.ml -> module List__List
  - list__Nth.ml -> module List__Nth
  
  - listExtension__ListExtension.ml -> module ListExtension__ListExtension
  - listExtension__ArrayList.ml -> module ListExtension__ArrayList
  - listExtension__ToArray.ml -> module ListExtension__ToArray

  - lexgen__Cursor.ml -> module Lexgen__Cursor
  - lexgenList__Cursor.ml -> module LexgenList__Cursor

  - map__Map.ml -> module Map__Map
  - option__Option.ml -> module Option__Option

  - Xxx__Xxx.ml -> module Xxx__Xxx for Endo, Barray, ...
  - Xxx__Enum.ml -> module Xxx__Enum for Endo, Barray, ...
*)


(* This file gathers files in
   https://github.com/alaingiorgetti/enum/tree/1.3/OCaml/generator/**. *)

module Array__Array = struct

exception OutOfBounds

let self_blit : type a. (a array) -> (Z.t) -> (Z.t) -> (Z.t) ->  unit =
  fun a ofs1 ofs2 len -> if Z.leq ofs1 ofs2
                         then
                           let o = Z.zero in
                           let o1 = Z.sub len Z.one in
                           let rec for_loop_to k =
                             if Z.geq k o
                             then begin
                               a.(Z.to_int (Z.add ofs2 k)) <- a.(Z.to_int 
                               (Z.add ofs1 k));
                               for_loop_to (Z.pred k)
                             end
                           in for_loop_to o1
                         else
                           begin
                             let o = Z.sub len Z.one in let o1 = Z.zero in
                             let rec for_loop_to1 k1 =
                               if Z.leq k1 o
                               then begin
                                 a.(Z.to_int (Z.add ofs2 k1)) <- a.(Z.to_int 
                                 (Z.add ofs1 k1));
                                 for_loop_to1 (Z.succ k1)
                               end
                             in for_loop_to1 o1 end

end


module Array__ArraySwap = struct

let swap : type a. (a array) -> (Z.t) -> (Z.t) ->  unit =
  fun a i j -> let v = a.(Z.to_int i) in
               a.(Z.to_int i) <- a.(Z.to_int j); a.(Z.to_int j) <- v

end


module Array__ToList = struct

let rec to_list : type a. (a array) -> (Z.t) -> (Z.t) ->  (a list) =
  fun a l u -> if Z.leq u l
               then [] 
               else a.(Z.to_int l) :: to_list a (Z.add l Z.one) u

end


module List__List = struct

let is_nil : type a. (a list) ->  (bool) =
  fun l -> match l with
    | [] -> true
    | _ :: _ -> false

end


module List__Nth = struct

let rec nth : type a. (Z.t) -> (a list) ->  (a option) =
  fun n l -> match l with
    | [] -> None 
    | x :: r -> if Z.equal n Z.zero then Some x else nth (Z.sub n Z.one) r

end


module ArrayExtension__ArrayInjection = struct

let in_interval (x: Z.t) (l: Z.t) (u: Z.t) : bool = Z.leq l x && Z.lt x u

let b_diff (a: (Z.t) array) (i: Z.t) : bool =
  let exception QtReturn of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to2 j =
       if Z.leq j o
       then begin
         if Z.equal a.(Z.to_int j) a.(Z.to_int i) && not (Z.equal j i)
         then raise (QtReturn false);
         for_loop_to2 (Z.succ j)
       end
     in for_loop_to2 o1);
    true
  with
  | QtReturn r -> r

let b_injective (a: (Z.t) array) : bool =
  let exception QtReturn1 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to3 j1 =
       if Z.leq j1 o
       then begin
         if not (b_diff a j1) then raise (QtReturn1 false);
         for_loop_to3 (Z.succ j1)
       end
     in for_loop_to3 o1);
    true
  with
  | QtReturn1 r1 -> r1

let b_range (a: (Z.t) array) : bool =
  let exception QtReturn2 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to4 j2 =
       if Z.leq j2 o
       then begin
         if not (in_interval a.(Z.to_int j2) Z.zero n)
         then raise (QtReturn2 false);
         for_loop_to4 (Z.succ j2)
       end
     in for_loop_to4 o1);
    true
  with
  | QtReturn2 r2 -> r2

end


module ArrayExtension__Array = struct

let copy (a: (Z.t) array) (b: (Z.t) array) : unit =
  let o = Z.sub (Z.of_int (Array.length a)) Z.one in
  let o1 = Z.zero in
  let rec for_loop_to2 i =
    if Z.leq i o
    then begin b.(Z.to_int i) <- a.(Z.to_int i); for_loop_to2 (Z.succ i) end
  in for_loop_to2 o1

end


module ArrayExtension__ToList = struct

let rec to_list (a: (Z.t) array) (l: Z.t) (u: Z.t) : (Z.t) list =
  if Z.leq u l then []  else a.(Z.to_int l) :: to_list a (Z.add l Z.one) u

end


module ArrayExtension__Inc = struct
  
let b_inc (a: (Z.t) array) : bool =
  let exception QtReturn4 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.one in
     let rec for_loop_to6 i1 =
       if Z.leq i1 o
       then begin
         if not (Z.lt a.(Z.to_int (Z.sub i1 Z.one)) a.(Z.to_int i1))
         then raise (QtReturn4 false);
         for_loop_to6 (Z.succ i1)
       end
     in for_loop_to6 o1);
    true
  with
  | QtReturn4 r4 -> r4

end


module ListExtension__ListExtension = struct

let rec id_aux (n: Z.t) (i: Z.t) : (Z.t) list =
  if Z.leq n Z.zero then []  else i :: id_aux (Z.sub n Z.one) (Z.add i Z.one)

let id (n: Z.t) : (Z.t) list = id_aux n Z.zero

let rec rm_nth (x: Z.t) (l: ((Z.t) list) ref) : Z.t =
  match !l with
  | [] -> l := [] ; Z.zero
  | y :: m ->
    if Z.equal x Z.zero
    then begin l := m; y end
    else
      begin
        let r = ref m in let z = rm_nth (Z.sub x Z.one) r in l := y :: !r; z end

let rec make (n: Z.t) (v: Z.t) : (Z.t) list =
  if Z.equal n Z.zero then []  else v :: make (Z.sub n Z.one) v

let rec nth_func_rec (i: Z.t) (l: (Z.t) list) : Z.t =
  match l with
  | x :: r -> if Z.equal i Z.zero then x else nth_func_rec (Z.sub i Z.one) r
  | _ -> assert false (* absurd *)

let in_interval (x: Z.t) (l: Z.t) (u: Z.t) : bool = Z.leq l x && Z.lt x u

end


module ListExtension__ArrayList = struct

let rec to_array_rec (l: (Z.t) list) (a: (Z.t) array) : unit =
  match l with
  | [] -> ()
  | y :: m ->
    a.(Z.to_int (Z.sub (Z.of_int (Array.length a))
                 (Z.of_int (List.length l)))) <- y;
    to_array_rec m a

let to_array (l: (Z.t) list) : (Z.t) array =
  let a = Array.make (Z.to_int (Z.of_int (List.length l))) Z.zero in
  to_array_rec l a; a

end


module ListExtension__ToArray = struct

let rec to_array_rec (l: (Z.t) list) (a: (Z.t) array) : unit =
  match l with
  | [] -> ()
  | y :: m ->
    a.(Z.to_int (Z.sub (Z.of_int (Array.length a))
                 (Z.of_int (List.length l)))) <- y;
    to_array_rec m a

let to_array (l: (Z.t) list) : (Z.t) array =
  let a = Array.make (Z.to_int (Z.of_int (List.length l))) Z.zero in
  to_array_rec l a; a

end


module Lexgen__Cursor = struct

  type cursor = {
    current: (Z.t) array;
    mutable new1: bool;
  }

end


module LexgenList__Cursor = struct

  type cursor = {
    mutable current: (Z.t) list;
    mutable new1: bool;
  }
  
end


module Map__Map = struct

type ('a, 'b) map = 'a -> 'b

let get : type a b. (a -> b) -> a ->  b = fun f x -> f x

let mixfix_lbrb : type a b. (a -> b) -> a ->  b = fun f x -> f x

end

module Option__Option = struct
  
let is_none : type a. (a option) ->  (bool) =
  fun o -> match o with
    | None -> true
    | Some _ -> false

end


(* From Barray__Barray.ml *)

module Barray__Barray = struct

let b_barray (a: (Z.t) array) (b: Z.t) : bool =
  let exception QtReturn3 of (bool) in
  try
    (let o = Z.sub (Z.of_int (Array.length a)) Z.one in let o1 = Z.zero in
     let rec for_loop_to5 i =
       if Z.leq i o
       then begin
         if not (let q1_ = a.(Z.to_int i) in Z.leq Z.zero q1_ && Z.lt q1_ b)
         then raise (QtReturn3 false);
         for_loop_to5 (Z.succ i)
       end
     in for_loop_to5 o1);
    true
  with
  | QtReturn3 r3 -> r3
 end


(* From Barray__Enum.ml *)

module Barray__Enum = struct

let create_cursor (n: Z.t) (b: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  if Z.leq b Z.zero
  then { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = false }
  else { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) (b: Z.t) : unit =
  if Z.leq b Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      let a = c.Lexgen__Cursor.current in
      let n = Z.of_int (Array.length a) in let r4 = ref (Z.sub n Z.one) in
      while Z.geq !r4 Z.zero && Z.equal a.(Z.to_int !r4) (Z.sub b Z.one) do
        r4 := Z.sub !r4 Z.one
      done;
      if Z.lt !r4 Z.zero
      then c.Lexgen__Cursor.new1 <- false
      else
        begin
          a.(Z.to_int !r4) <- Z.add a.(Z.to_int !r4) Z.one;
          (let o = Z.sub n Z.one in let o1 = Z.add !r4 Z.one in
           let rec for_loop_to6 i1 =
             if Z.leq i1 o
             then begin
               a.(Z.to_int i1) <- Z.zero;
               for_loop_to6 (Z.succ i1)
             end
           in for_loop_to6 o1);
          c.Lexgen__Cursor.new1 <- true
        end end

end


module Blist__Blist = struct

let rec is_blist (l: (Z.t) list) (b: Z.t) : bool =
  match l with
  | [] -> true
  | x :: r -> (Z.leq Z.zero x && Z.lt x b) && is_blist r b

end


module Blist__Enum = struct

let create_cursor (n: Z.t) (b: Z.t) : LexgenList__Cursor.cursor =
  let l = ListExtension__ListExtension.make n Z.zero in
  if Z.lt b Z.zero
  then { LexgenList__Cursor.current = l; LexgenList__Cursor.new1 = false }
  else
    begin
      if Blist__Blist.is_blist l b
      then { LexgenList__Cursor.current = l; LexgenList__Cursor.new1 = true }
      else
        { LexgenList__Cursor.current = l; LexgenList__Cursor.new1 = false } end

let rec next_blist_rec (l: (Z.t) list) (b: Z.t) : ((Z.t) list) option =
  match l with
  | [] -> None 
  | x :: r ->
    if Z.lt x (Z.sub b Z.one)
    then Some (Z.add x Z.one :: r)
    else
      begin match next_blist_rec r b with
      | None -> None 
      | Some m -> Some (x :: m)
      end

let next (c: LexgenList__Cursor.cursor) (b: Z.t) : unit =
  if Z.lt b Z.zero
  then c.LexgenList__Cursor.new1 <- false
  else
    begin match next_blist_rec c.LexgenList__Cursor.current b with
    | None -> c.LexgenList__Cursor.new1 <- false
    | Some l ->
      c.LexgenList__Cursor.current <- l; c.LexgenList__Cursor.new1 <- true
    end

end


(* From Endo__Endo.ml *)

module Endo = struct

let b_endo (a: (Z.t) array) : bool =
  let exception QtReturn of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to2 i =
       if Z.leq i o
       then begin
         if not (let q1_ = a.(Z.to_int i) in Z.leq Z.zero q1_ && Z.lt q1_ n)
         then raise (QtReturn false);
         for_loop_to2 (Z.succ i)
       end
     in for_loop_to2 o1);
    true
  with
  | QtReturn r -> r

end


(* From Endo__Enum.ml *)

module Endo__Enum = struct

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  if Z.lt n Z.zero
  then
    { Lexgen__Cursor.current = Array.make (Z.to_int Z.zero) Z.zero;
      Lexgen__Cursor.new1 = false }
  else
    begin
      let a = Array.make (Z.to_int n) Z.zero in
      { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true } end

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r2 = ref (Z.sub n Z.one) in
  while Z.geq !r2 Z.zero && Z.equal a.(Z.to_int !r2) (Z.sub n Z.one) do
    r2 := Z.sub !r2 Z.one
  done;
  if Z.lt !r2 Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int !r2) <- Z.add a.(Z.to_int !r2) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add !r2 Z.one in
       let rec for_loop_to4 i2 =
         if Z.leq i2 o
         then begin a.(Z.to_int i2) <- Z.zero; for_loop_to4 (Z.succ i2) end
       in for_loop_to4 o1);
      c.Lexgen__Cursor.new1 <- true
    end

end

(* From Fact__Fact (incomplete) *)

module Fact__Fact = struct

let b_fact (a: (Z.t) array) : bool =
  let exception QtReturn of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to2 i =
       if Z.leq i o
       then begin
         if not (let q1_ = a.(Z.to_int i) in Z.leq Z.zero q1_ && Z.leq q1_ i)
         then raise (QtReturn false);
         for_loop_to2 (Z.succ i)
       end
     in for_loop_to2 o1);
    true
  with
  | QtReturn r -> r

end


module Fact__Enum = struct

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r1 = ref (Z.sub n Z.one) in
  while Z.geq !r1 Z.zero && Z.equal a.(Z.to_int !r1) !r1 do
    r1 := Z.sub !r1 Z.one
  done;
  if Z.lt !r1 Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int !r1) <- Z.add a.(Z.to_int !r1) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add !r1 Z.one in
       let rec for_loop_to3 i1 =
         if Z.leq i1 o
         then begin a.(Z.to_int i1) <- Z.zero; for_loop_to3 (Z.succ i1) end
       in for_loop_to3 o1);
      c.Lexgen__Cursor.new1 <- true
    end


end


(* Fact__FactEndo *)

module Fact__FactEndo = struct

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let c = Endo__Enum.create_cursor n in
  while c.Lexgen__Cursor.new1 && not (Fact__Fact.b_fact c.Lexgen__Cursor.current) do
    Endo__Enum.next c
  done;
  c

let next (c: Lexgen__Cursor.cursor) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length c.Lexgen__Cursor.current))) Z.zero in
  ArrayExtension__Array.copy c.Lexgen__Cursor.current a;
  if c.Lexgen__Cursor.new1 then Endo__Enum.next c;
  while c.Lexgen__Cursor.new1 && not (Fact__Fact.b_fact c.Lexgen__Cursor.current) do
    Endo__Enum.next c
  done;
  if not (Fact__Fact.b_fact c.Lexgen__Cursor.current)
  then begin
         ArrayExtension__Array.copy a c.Lexgen__Cursor.current;
         c.Lexgen__Cursor.new1 <- false
       end

end


(* From Permutation__Permutation.ml *)

module Permutation = struct

let b_permut (a: (Z.t) array) : bool =
  ArrayExtension__ArrayInjection.b_range a && ArrayExtension__ArrayInjection.b_injective a

end


(* From Permutation__Enum.ml *)

module Permutation__Enum =struct

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  (let o = Z.sub n Z.one in let o1 = Z.zero in
   let rec for_loop_to6 i1 =
     if Z.leq i1 o
     then begin a.(Z.to_int i1) <- i1; for_loop_to6 (Z.succ i1) end
   in for_loop_to6 o1);
  { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let reverse (a: (Z.t) array) (l: Z.t) (u: Z.t) : unit =
  let m = Z.ediv (Z.sub (Z.add l u) Z.one) (Z.of_string "2") in
  let rec for_loop_to7 i2 =
    if Z.leq i2 m
    then begin
      Array__ArraySwap.swap a i2 (Z.sub (Z.sub (Z.add u l) Z.one) i2);
      for_loop_to7 (Z.succ i2)
    end
  in for_loop_to7 l

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  if Z.leq n Z.one
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      let r4 = ref (Z.sub n (Z.of_string "2")) in
      while Z.geq !r4 Z.zero && Z.gt a.(Z.to_int !r4)
                                a.(Z.to_int (Z.add !r4 Z.one)) do
        r4 := Z.sub !r4 Z.one
      done;
      if Z.lt !r4 Z.zero
      then c.Lexgen__Cursor.new1 <- false
      else
        begin
          let j3 = ref (Z.sub n Z.one) in
          while Z.gt a.(Z.to_int !r4) a.(Z.to_int !j3) do
            j3 := Z.sub !j3 Z.one
          done;
          Array__ArraySwap.swap a !r4 !j3;
          reverse a (Z.add !r4 Z.one) n;
          c.Lexgen__Cursor.new1 <- true end end

end


(* From Rgf__Rgf.ml *)

module Rgf__Rgf = struct

let b_rgf (a: (Z.t) array) : bool =
  let exception QtReturn1 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    if Z.equal n Z.zero
    then raise (QtReturn1 true)
    else
      begin
        if not (Z.equal a.(Z.to_int Z.zero) Z.zero)
        then raise (QtReturn1 false)
        else
          begin
            let o = Z.sub n Z.one in let o1 = Z.one in
            let rec for_loop_to3 i1 =
              if Z.leq i1 o
              then begin
                if
                  not (let q1_ = a.(Z.to_int i1) in
                       Z.leq Z.zero q1_ && Z.leq q1_
                                           (Z.add a.(Z.to_int (Z.sub i1
                                                               Z.one))
                                            Z.one))
                then raise (QtReturn1 false);
                for_loop_to3 (Z.succ i1)
              end
            in for_loop_to3 o1 end end;
    true
  with
  | QtReturn1 r1 -> r1

end


(* From Rgf__Enum.ml *)

module Rgf__Enum = struct

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  if Z.leq n Z.one
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      let r2 = ref (Z.sub n Z.one) in
      while Z.geq !r2 Z.one && Z.equal a.(Z.to_int !r2)
                               (Z.add a.(Z.to_int (Z.sub !r2 Z.one)) Z.one) do
        r2 := Z.sub !r2 Z.one
      done;
      if Z.equal !r2 Z.zero
      then c.Lexgen__Cursor.new1 <- false
      else
        begin
          a.(Z.to_int !r2) <- Z.add a.(Z.to_int !r2) Z.one;
          (let o = Z.sub n Z.one in let o1 = Z.add !r2 Z.one in
           let rec for_loop_to4 i2 =
             if Z.leq i2 o
             then begin
               a.(Z.to_int i2) <- Z.zero;
               for_loop_to4 (Z.succ i2)
             end
           in for_loop_to4 o1);
          c.Lexgen__Cursor.new1 <- true
        end end

end

(* From Sorted__Sorted.ml *)

module Sorted__Sorted = struct

let b_inc1 (a: (Z.t) array) : bool =
  let exception QtReturn4 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.one in
     let rec for_loop_to6 i1 =
       if Z.leq i1 o
       then begin
         if not (Z.leq a.(Z.to_int (Z.sub i1 Z.one)) a.(Z.to_int i1))
         then raise (QtReturn4 false);
         for_loop_to6 (Z.succ i1)
       end
     in for_loop_to6 o1);
    true
  with
  | QtReturn4 r4 -> r4

let b_sorted (a: (Z.t) array) (b: Z.t) : bool =
  Barray__Barray.b_barray a b && b_inc1 a

end


(* From Sorted__Enum.ml *)

module Sorted__Enum = struct

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  if Z.leq k2 Z.zero
  then { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = false }
  else { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r5 = ref (Z.sub n Z.one) in
  while Z.geq !r5 Z.zero && Z.equal a.(Z.to_int !r5) (Z.sub k2 Z.one) do
    r5 := Z.sub !r5 Z.one
  done;
  if Z.lt !r5 Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int !r5) <- Z.add a.(Z.to_int !r5) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add !r5 Z.one in
       let rec for_loop_to7 i2 =
         if Z.leq i2 o
         then begin
           a.(Z.to_int i2) <- a.(Z.to_int !r5);
           for_loop_to7 (Z.succ i2)
         end
       in for_loop_to7 o1);
      c.Lexgen__Cursor.new1 <- true
    end

end


(* From Sorted__SortedBarray.ml *)

module Sorted__SortedBarray = struct

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Sorted__Sorted.b_sorted c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length c.Lexgen__Cursor.current))) Z.zero in
  ArrayExtension__Array.copy c.Lexgen__Cursor.current a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Sorted__Sorted.b_sorted c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  if not (Sorted__Sorted.b_sorted c.Lexgen__Cursor.current k2)
  then begin
         ArrayExtension__Array.copy a c.Lexgen__Cursor.current;
         c.Lexgen__Cursor.new1 <- false
       end

end


(* From Filtering__inj.ml *)

module Filtering__Inj = struct

let b_linear (a: (Z.t) array) : bool =
  let exception QtReturn5 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to9 i4 =
       if Z.leq i4 o
       then begin
         if not (ArrayExtension__ArrayInjection.b_diff a i4)
         then raise (QtReturn5 false);
         for_loop_to9 (Z.succ i4)
       end
     in for_loop_to9 o1);
    true
  with
  | QtReturn5 r5 -> r5

let b_injective (a: (Z.t) array) (k2: Z.t) : bool =
  if Z.lt k2 (Z.of_int (Array.length a))
  then false
  else Barray__Barray.b_barray a k2 && b_linear a

end


(* From Filtering__InjBarray.ml *)

module Filtering__InjBarray = struct

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Filtering__Inj.b_injective c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length c.Lexgen__Cursor.current))) Z.zero in
  ArrayExtension__Array.copy c.Lexgen__Cursor.current a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Filtering__Inj.b_injective c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  if not (Filtering__Inj.b_injective c.Lexgen__Cursor.current k2)
  then begin
         ArrayExtension__Array.copy a c.Lexgen__Cursor.current;
         c.Lexgen__Cursor.new1 <- false
       end

end


(* From Surj__Surj.ml *)

module Surj__Surj = struct

let b_pre_img (a: (Z.t) array) (j3: Z.t) : bool =
  let exception QtReturn4 of (bool) in
  try
    (let o = Z.sub (Z.of_int (Array.length a)) Z.one in let o1 = Z.zero in
     let rec for_loop_to7 i2 =
       if Z.leq i2 o
       then begin
         if Z.equal a.(Z.to_int i2) j3 then raise (QtReturn4 true);
         for_loop_to7 (Z.succ i2)
       end
     in for_loop_to7 o1);
    false
  with
  | QtReturn4 r4 -> r4

let b_init_pre_img (a: (Z.t) array) (k2: Z.t) : bool =
  let exception QtReturn5 of (bool) in
  try
    (let o = Z.sub k2 Z.one in let o1 = Z.zero in
     let rec for_loop_to8 j3 =
       if Z.leq j3 o
       then begin
         if not (b_pre_img a j3) then raise (QtReturn5 false);
         for_loop_to8 (Z.succ j3)
       end
     in for_loop_to8 o1);
    true
  with
  | QtReturn5 r5 -> r5

end


(* From Surj__SurjBarray.ml *)

module Surj__SurjBarray = struct

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Surj__Surj.b_init_pre_img c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length c.Lexgen__Cursor.current))) Z.zero in
  ArrayExtension__Array.copy c.Lexgen__Cursor.current a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Surj__Surj.b_init_pre_img c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  if not (Surj__Surj.b_init_pre_img c.Lexgen__Cursor.current k2)
  then begin
         ArrayExtension__Array.copy a c.Lexgen__Cursor.current;
         c.Lexgen__Cursor.new1 <- false
       end

end


(* From Filtering__Comb.ml *)

module Filtering__Comb = struct

let b_comb (a: (Z.t) array) (k2: Z.t) : bool =
  if Z.leq k2 Z.zero
  then false
  else Barray__Barray.b_barray a k2 && ArrayExtension__Inc.b_inc a

end


(* From Filtering__CombBarray.ml *)

module Filtering__CombBarray = struct

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Filtering__Comb.b_comb c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length c.Lexgen__Cursor.current))) Z.zero in
  ArrayExtension__Array.copy c.Lexgen__Cursor.current a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Filtering__Comb.b_comb c.Lexgen__Cursor.current
                                      k2) do
    Barray__Enum.next c k2
  done;
  if not (Filtering__Comb.b_comb c.Lexgen__Cursor.current k2)
  then begin
         ArrayExtension__Array.copy a c.Lexgen__Cursor.current;
         c.Lexgen__Cursor.new1 <- false
       end

end

