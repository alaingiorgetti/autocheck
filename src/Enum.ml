(********************************************************************)
(* Copyright (C) 2020 Alain Giorgetti and Clotilde Erard            *)
(* FEMTO-ST institute                                               *)
(********************************************************************)

(********************************************************************)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1                              *)
(********************************************************************)

(* Supplementary code (should come from some release of ENUM > 1.2.0): *)

module Array__Quant = struct

let for_all_sub :
  type a. (a -> (bool)) -> (a array) -> (Z.t) -> (Z.t) ->  (bool) =
  fun p a l u -> let exception QtReturn of (bool) in
                 try
                   begin
                     let rec for_loop_to3 i1 =
                       if Z.leq i1 u
                       then begin
                         if not (p (a.(Z.to_int i1)))
                         then raise (QtReturn false);
                         for_loop_to3 (Z.succ i1)
                       end
                     in for_loop_to3 l end;
                   true
                 with
                 | QtReturn r -> r

let for_all : type a. (a -> (bool)) -> (a array) ->  (bool) =
  fun p a -> let exception QtReturn1 of (bool) in
             try
               (let o = Z.sub (Z.of_int (Array.length a)) Z.one in
                let o1 = Z.zero in
                let rec for_loop_to4 i2 =
                  if Z.leq i2 o
                  then begin
                    if not (p (a.(Z.to_int i2))) then raise (QtReturn1 false);
                    for_loop_to4 (Z.succ i2)
                  end
                in for_loop_to4 o1);
               true
             with
             | QtReturn1 r1 -> r1

let for_some_sub :
  type a. (a -> (bool)) -> (a array) -> (Z.t) -> (Z.t) ->  (bool) =
  fun p a l u -> let exception QtReturn2 of (bool) in
                 try
                   begin
                     let rec for_loop_to5 i3 =
                       if Z.leq i3 u
                       then begin
                         if p (a.(Z.to_int i3)) then raise (QtReturn2 true);
                         for_loop_to5 (Z.succ i3)
                       end
                     in for_loop_to5 l end;
                   false
                 with
                 | QtReturn2 r2 -> r2

let for_some : type a. (a -> (bool)) -> (a array) ->  (bool) =
  fun p a -> let exception QtReturn3 of (bool) in
             try
               (let o = Z.sub (Z.of_int (Array.length a)) Z.one in
                let o1 = Z.zero in
                let rec for_loop_to6 i4 =
                  if Z.leq i4 o
                  then begin
                    if p (a.(Z.to_int i4)) then raise (QtReturn3 true);
                    for_loop_to6 (Z.succ i4)
                  end
                in for_loop_to6 o1);
               false
             with
             | QtReturn3 r3 -> r3

let mem_sub :
  type a. (a -> (a -> (bool))) -> a -> (a array) -> (Z.t) -> (Z.t) ->  (bool) =
  fun eq x a l u -> for_some_sub (eq x) a l u

let mem : type a. (a -> (a -> (bool))) -> a -> (a array) ->  (bool) =
  fun eq x a -> for_some (eq x) a

end


(** Extracted contents of ENUM 1.2.0 adapted to a single file *)

(* This file gathers files in
   https://github.com/alaingiorgetti/enum/tree/1.2/OCaml/generator/**. *)

module Array__ArraySwap = struct

let swap : type a. (a array) -> (Z.t) -> (Z.t) ->  unit =
  fun a i j -> let v = a.(Z.to_int i) in
               a.(Z.to_int i) <- a.(Z.to_int j); a.(Z.to_int j) <- v

end

module ArrayExtension = struct

let in_interval (x: Z.t) (l: Z.t) (u: Z.t) : bool = Z.leq l x && Z.lt x u

let b_diff (a: (Z.t) array) (i: Z.t) : bool =
  let exception QtReturn of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to2 j =
       if Z.leq j o
       then begin
         if Z.equal (a.(Z.to_int j)) (a.(Z.to_int i)) && not (Z.equal j i)
         then raise (QtReturn false);
         for_loop_to2 (Z.succ j)
       end
     in for_loop_to2 o1);
    true
  with
  | QtReturn r -> r

let b_inj (a: (Z.t) array) : bool =
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
         if not (in_interval (a.(Z.to_int j2)) Z.zero n)
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
  let rec for_loop_to9 i3 =
    if Z.leq i3 o
    then begin
      b.(Z.to_int i3) <- a.(Z.to_int i3);
      for_loop_to9 (Z.succ i3)
    end
  in for_loop_to9 o1
end

module Lexgen__Cursor = struct

  type cursor = {
    current: (Z.t) array;
    mutable new1: bool;
  }
end

module Barray = struct

(* From Barray__Barray.ml *)

let b_barray (a: (Z.t) array) (b: Z.t) : bool =
  let exception QtReturn3 of (bool) in
  try
    if Z.leq b Z.zero
    then false
    else
      begin
        (let o = Z.sub (Z.of_int (Array.length a)) Z.one in
         let o1 = Z.zero in
         let rec for_loop_to5 i =
           if Z.leq i o
           then begin
             if
               not (let q1_ = a.(Z.to_int i) in
                    Z.leq Z.zero q1_ && Z.lt q1_ b)
             then raise (QtReturn3 false);
             for_loop_to5 (Z.succ i)
           end
         in for_loop_to5 o1);
        true
      end
  with
  | QtReturn3 r3 -> r3

(* From Barray__Enum.ml *)

let create_cursor (n: Z.t) (b: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  if Z.leq b Z.zero
  then { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = false }
  else { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) (b: Z.t) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r5 = ref (Z.sub n Z.one) in
  while Z.geq (!r5) Z.zero && Z.equal (a.(Z.to_int (!r5))) (Z.sub b Z.one) do
    r5 := Z.sub (!r5) Z.one
  done;
  if Z.lt (!r5) Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int (!r5)) <- Z.add (a.(Z.to_int (!r5))) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add (!r5) Z.one in
       let rec for_loop_to8 i3 =
         if Z.leq i3 o
         then begin a.(Z.to_int i3) <- Z.zero; for_loop_to8 (Z.succ i3) end
       in for_loop_to8 o1);
      c.Lexgen__Cursor.new1 <- true
    end
end

module Fact = struct

(* From Fact__Fact (incomplete) *)

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

(* Fact__Enum *)

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r1 = ref (Z.sub n Z.one) in
  while Z.geq (!r1) Z.zero && Z.equal (a.(Z.to_int (!r1))) (!r1) do
    r1 := Z.sub (!r1) Z.one
  done;
  if Z.lt (!r1) Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int (!r1)) <- Z.add (a.(Z.to_int (!r1))) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add (!r1) Z.one in
       let rec for_loop_to3 i1 =
         if Z.leq i1 o
         then begin a.(Z.to_int i1) <- Z.zero; for_loop_to3 (Z.succ i1) end
       in for_loop_to3 o1);
      c.Lexgen__Cursor.new1 <- true
    end
end


module Permutation = struct

(* From Permutation__Permutation.ml *)

let b_permut (a: (Z.t) array) : bool =
  ArrayExtension.b_range a && ArrayExtension.b_inj a

(* From Permutation__Enum.ml *)

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  (let o = Z.sub n Z.one in let o1 = Z.zero in
   let rec for_loop_to6 i1 =
     if Z.leq i1 o
     then begin a.(Z.to_int i1) <- i1; for_loop_to6 (Z.succ i1) end
   in for_loop_to6 o1);
  { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let reverse (a: (Z.t) array) (l: Z.t) (u: Z.t) : unit =
  let x = ref l in
  let y = ref (Z.sub u Z.one) in
  while Z.lt (!x) (!y) do
    Array__ArraySwap.swap a (!x) (!y);
    y := Z.sub (!y) Z.one;
    x := Z.add (!x) Z.one
  done

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  if Z.leq n Z.one
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      let r4 = ref (Z.sub n (Z.of_string "2")) in
      while Z.geq (!r4) Z.zero && Z.gt (a.(Z.to_int (!r4))) (a.(Z.to_int 
                                                             (Z.add (!r4) Z.one))) do
        r4 := Z.sub (!r4) Z.one
      done;
      if Z.lt (!r4) Z.zero
      then c.Lexgen__Cursor.new1 <- false
      else
        begin
          let j3 = ref (Z.sub n Z.one) in
          while Z.gt (a.(Z.to_int (!r4))) (a.(Z.to_int (!j3))) do
            j3 := Z.sub (!j3) Z.one
          done;
          Array__ArraySwap.swap a (!r4) (!j3);
          reverse a (Z.add (!r4) Z.one) n;
          c.Lexgen__Cursor.new1 <- true end end
end

module Rgf = struct

(* From Rgf__Rgf.ml *)

let b_rgf (a: (Z.t) array) : bool =
  let exception QtReturn1 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    if Z.equal n Z.zero
    then raise (QtReturn1 true)
    else
      begin
        if not (Z.equal (a.(Z.to_int Z.zero)) Z.zero)
        then raise (QtReturn1 false)
        else
          begin
            let o = Z.sub n Z.one in let o1 = Z.one in
            let rec for_loop_to3 i1 =
              if Z.leq i1 o
              then begin
                if
                  not (let q1_ = a.(Z.to_int i1) in
                       Z.leq Z.zero q1_ && Z.leq q1_ (Z.add (a.(Z.to_int 
                                                             (Z.sub i1 Z.one))) Z.one))
                then raise (QtReturn1 false);
                for_loop_to3 (Z.succ i1)
              end
            in for_loop_to3 o1 end end;
    true
  with
  | QtReturn1 r1 -> r1

(* From Rgf__Enum.ml *)

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
      while Z.geq (!r2) Z.one && Z.equal (a.(Z.to_int (!r2))) (Z.add 
                                                               (a.(Z.to_int 
                                                                (Z.sub 
                                                                 (!r2) Z.one))) Z.one) do
        r2 := Z.sub (!r2) Z.one
      done;
      if Z.equal (!r2) Z.zero
      then c.Lexgen__Cursor.new1 <- false
      else
        begin
          a.(Z.to_int (!r2)) <- Z.add (a.(Z.to_int (!r2))) Z.one;
          (let o = Z.sub n Z.one in let o1 = Z.add (!r2) Z.one in
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

module Barray__Barray = Barray

module Sorted = struct

(* From Sorted__Sorted.ml *)

let b_inc1 (a: (Z.t) array) : bool =
  let exception QtReturn4 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.one in
     let rec for_loop_to6 i1 =
       if Z.leq i1 o
       then begin
         if not (Z.leq (a.(Z.to_int (Z.sub i1 Z.one))) (a.(Z.to_int i1)))
         then raise (QtReturn4 false);
         for_loop_to6 (Z.succ i1)
       end
     in for_loop_to6 o1);
    true
  with
  | QtReturn4 r4 -> r4

let b_sorted (a: (Z.t) array) (b: Z.t) : bool =
  Barray__Barray.b_barray a b && b_inc1 a

(* From Sorted__Enum.ml *)

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  if Z.leq k2 Z.zero
  then { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = false }
  else { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r5 = ref (Z.sub n Z.one) in
  while Z.geq (!r5) Z.zero && Z.equal (a.(Z.to_int (!r5))) (Z.sub k2 Z.one) do
    r5 := Z.sub (!r5) Z.one
  done;
  if Z.lt (!r5) Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int (!r5)) <- Z.add (a.(Z.to_int (!r5))) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add (!r5) Z.one in
       let rec for_loop_to7 i2 =
         if Z.leq i2 o
         then begin
           a.(Z.to_int i2) <- a.(Z.to_int (!r5));
           for_loop_to7 (Z.succ i2)
         end
       in for_loop_to7 o1);
      c.Lexgen__Cursor.new1 <- true
    end
end

module Endo = struct

(* From Endo__Endo.ml *)

let b_endo (a: (Z.t) array) : bool =
  let exception QtReturn of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.zero in
     let rec for_loop_to2 i =
       if Z.leq i o
       then begin
         if
           not (let q1_ = a.(Z.to_int i) in
                Z.leq Z.zero q1_ && Z.lt q1_ (Z.of_int (Array.length a)))
         then raise (QtReturn false);
         for_loop_to2 (Z.succ i)
       end
     in for_loop_to2 o1);
    true
  with
  | QtReturn r -> r

(* From Endo__Enum.ml *)

let create_cursor (n: Z.t) : Lexgen__Cursor.cursor =
  let a = Array.make (Z.to_int n) Z.zero in
  { Lexgen__Cursor.current = a; Lexgen__Cursor.new1 = true }

let next (c: Lexgen__Cursor.cursor) : unit =
  let a = c.Lexgen__Cursor.current in
  let n = Z.of_int (Array.length a) in
  let r1 = ref (Z.sub n Z.one) in
  while Z.geq (!r1) Z.zero && Z.equal (a.(Z.to_int (!r1))) (Z.sub n Z.one) do
    r1 := Z.sub (!r1) Z.one
  done;
  if Z.lt (!r1) Z.zero
  then c.Lexgen__Cursor.new1 <- false
  else
    begin
      a.(Z.to_int (!r1)) <- Z.add (a.(Z.to_int (!r1))) Z.one;
      (let o = Z.sub n Z.one in let o1 = Z.add (!r1) Z.one in
       let rec for_loop_to3 i1 =
         if Z.leq i1 o
         then begin a.(Z.to_int i1) <- Z.zero; for_loop_to3 (Z.succ i1) end
       in for_loop_to3 o1);
      c.Lexgen__Cursor.new1 <- true
    end
end

module Barray__Enum = Barray
module Sorted__Sorted = Sorted

module SortedBarray = struct

(* From Sorted__SortedBarray.ml *)

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Sorted__Sorted.b_sorted (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length (c.Lexgen__Cursor.current)))) Z.zero in
  ArrayExtension__Array.copy (c.Lexgen__Cursor.current) a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Sorted__Sorted.b_sorted (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  if not (Sorted__Sorted.b_sorted (c.Lexgen__Cursor.current) k2)
  then begin
         ArrayExtension__Array.copy a (c.Lexgen__Cursor.current);
         c.Lexgen__Cursor.new1 <- false
       end

end

module ArrayExtension__ArrayInjection = ArrayExtension

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

let b_inj (a: (Z.t) array) (k2: Z.t) : bool =
  if Z.lt k2 (Z.of_int (Array.length a))
  then false
  else Barray__Barray.b_barray a k2 && b_linear a
end

module InjBarray = struct

(* From Filtering__InjBarray.ml *)

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Filtering__Inj.b_inj (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length (c.Lexgen__Cursor.current)))) Z.zero in
  ArrayExtension__Array.copy (c.Lexgen__Cursor.current) a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Filtering__Inj.b_inj (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  if not (Filtering__Inj.b_inj (c.Lexgen__Cursor.current) k2)
  then begin
         ArrayExtension__Array.copy a (c.Lexgen__Cursor.current);
         c.Lexgen__Cursor.new1 <- false
       end
end

module Surj__Surj = struct

(* From Surj__Surj.ml *)

let b_pre_img (a: (Z.t) array) (j3: Z.t) : bool =
  let exception QtReturn4 of (bool) in
  try
    (let o = Z.sub (Z.of_int (Array.length a)) Z.one in let o1 = Z.zero in
     let rec for_loop_to7 i2 =
       if Z.leq i2 o
       then begin
         if Z.equal (a.(Z.to_int i2)) j3 then raise (QtReturn4 true);
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

module SurjBarray = struct
(* From Surj__SurjBarray.ml *)

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Surj__Surj.b_init_pre_img (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length (c.Lexgen__Cursor.current)))) Z.zero in
  ArrayExtension__Array.copy (c.Lexgen__Cursor.current) a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Surj__Surj.b_init_pre_img (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  if not (Surj__Surj.b_init_pre_img (c.Lexgen__Cursor.current) k2)
  then begin
         ArrayExtension__Array.copy a (c.Lexgen__Cursor.current);
         c.Lexgen__Cursor.new1 <- false
       end


end

(* From arrayExtension__Inc.ml *)
module ArrayExtension__Inc = struct

let b_inc (a: (Z.t) array) : bool =
  let exception QtReturn4 of (bool) in
  try
    let n = Z.of_int (Array.length a) in
    (let o = Z.sub n Z.one in let o1 = Z.one in
     let rec for_loop_to6 i1 =
       if Z.leq i1 o
       then begin
         if not (Z.lt (a.(Z.to_int (Z.sub i1 Z.one))) (a.(Z.to_int i1)))
         then raise (QtReturn4 false);
         for_loop_to6 (Z.succ i1)
       end
     in for_loop_to6 o1);
    true
  with
  | QtReturn4 r4 -> r4
end

module Filtering__Comb = struct (* From Filtering__Comb.ml *)
let b_comb (a: (Z.t) array) (k2: Z.t) : bool =
  if Z.leq k2 Z.zero
  then false
  else Barray__Barray.b_barray a k2 && ArrayExtension__Inc.b_inc a


end

module CombBarray = struct

(* From Filtering__CombBarray.ml *)

let create_cursor (n: Z.t) (k2: Z.t) : Lexgen__Cursor.cursor =
  let c = Barray__Enum.create_cursor n k2 in
  while c.Lexgen__Cursor.new1 && not (Filtering__Comb.b_comb (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  c

let next (c: Lexgen__Cursor.cursor) (k2: Z.t) : unit =
  let a =
    Array.make (Z.to_int (Z.of_int (Array.length (c.Lexgen__Cursor.current)))) Z.zero in
  ArrayExtension__Array.copy (c.Lexgen__Cursor.current) a;
  if c.Lexgen__Cursor.new1 then Barray__Enum.next c k2;
  while c.Lexgen__Cursor.new1 && not (Filtering__Comb.b_comb (c.Lexgen__Cursor.current)
                                        k2) do
    Barray__Enum.next c k2
  done;
  if not (Filtering__Comb.b_comb (c.Lexgen__Cursor.current) k2)
  then begin
         ArrayExtension__Array.copy a (c.Lexgen__Cursor.current);
         c.Lexgen__Cursor.new1 <- false
       end
end
