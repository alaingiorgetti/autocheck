(********************************************************************)
(* Copyright (C) 2020 Alain Giorgetti and Clotilde Erard            *)
(* FEMTO-ST institute                                               *)
(********************************************************************)

(********************************************************************)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1                              *)
(********************************************************************)

(* SCheck.ml: Enumerative testing for OCaml

   Inspired by QCheck for OCaml and SmallCheck for Haskell
*)

let poly_compare=compare
open Printf

open QCheck  (* for printing feature *)

open Enum    (* Library of OCaml enumerators extracted from certified
                WhyML enumerators *)

module EnumState = struct
  type 'a t = {
    item: 'a;
    rk: int;
    is_new: bool
  }
end

module ES = EnumState

(* Turns an ENUM generator into a generator for SCheck *)
module EnumAdapter = struct

  (* Conversion of (Z.t) array into int array. *)
  let to_int_array (za: (Z.t) array) : int array = Array.map (Z.to_int) za

  (* Conversion of int array into (Z.t) array. *)
  let of_int_array (a: int array) : (Z.t) array = Array.map (Z.of_int) a

  let es_of_enum (lc: Lexgen__Cursor.cursor) : (int array) ES.t = {
    item = to_int_array lc.current;
    rk = 0; (* not yet implemented *)
    is_new = lc.new1
  }

  let es_to_enum (c: (int array) ES.t) : Lexgen__Cursor.cursor = {
    current =of_int_array  c.item;
    new1 = c.is_new
  }

  let create_of_enum (cc: Z.t -> Lexgen__Cursor.cursor) (n: int) : (int array) ES.t =
    es_of_enum (cc (Z.of_int n))

  let next_of_enum (n: Lexgen__Cursor.cursor -> unit) :
     (int array) ES.t -> (int array) ES.t =
     fun c ->
     let lc = es_to_enum c in begin
       n lc;
       es_of_enum lc
     end
end

module Gen = struct
  type 'a gen = {
    create: int -> 'a ES.t;
    has_next: 'a ES.t -> bool;
    next: 'a ES.t -> 'a ES.t
  }

  let unit = {
    create = (fun _ -> {
      ES.item = ();
      ES.rk = 1;
      ES.is_new = true });
    has_next = (fun c -> c.is_new);
    next = (fun c -> {
      ES.item = c.ES.item;
      ES.rk = 2;
      ES.is_new = false })
   }

  let bool = {
    create = (fun _ -> {
      ES.item = false;
      ES.rk = 1;
      ES.is_new = true });
     has_next = (fun c -> c.is_new);
    next = (fun c ->
      match c.ES.item with
      | false -> {
          ES.item = true;
          ES.rk = 2;
          ES.is_new = true
        }
      | true -> {
          ES.item = true;
          ES.rk = 2;
          ES.is_new = false
        }
    )
  }

  (* TODO:
     - revise ranks and size parameter for create
     - see if opt -> option is correct *)
  let opt (g: 'a gen) : ('a option) gen = {
    create = (fun _ -> { item = None; rk = 1; is_new = true });
    has_next = (fun c -> c.is_new);
    next = fun c -> match c.item with 
      | None ->
          { item = Some ((g.create 0).item); rk = 2; is_new = true }
      | Some x ->
          let d = { ES.item = x; rk = c.rk; is_new = true } in
          if g.has_next d then
            let y = g.next d in
            { item = Some (y.item); rk = y.rk+1; is_new = y.is_new }
          else 
            { item = c.item; rk = c.rk; is_new = false }
  }

  (* Interval [a..b] *)
  let int_range (a: int) (b: int) : int gen = {
    create = (fun _ ->
      { item = a; rk = 1; is_new = if b < a then false else true });
    has_next = (fun c -> c.is_new);
    next = fun c -> if c.item < b then
      { item = c.item+1; rk = c.rk+1; is_new = true }
   else
      { item = c.item; rk = c.rk; is_new = false }
  }

  (* Interval [0..n] *)
  let int_bound (n: int) : int gen = int_range 0 n

  (* Factorial arrays on [0..size-1] *)
  let fact_size (size: int) : (int array) gen = {
    create = (fun n -> 
      EnumAdapter.create_of_enum 
      Fact.create_cursor
      n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum Fact.next
  }

  (* Permutations on [0..size-1]. *)
  let permut_size (size: int) : (int array) gen = {
    create = (fun n -> 
      EnumAdapter.create_of_enum 
      Permutation.create_cursor
      n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum Permutation.next
  }

  (* RGFs on [0..size-1]. *)
  let rgf_size (size: int) : (int array) gen = {
    create = (fun n -> 
      EnumAdapter.create_of_enum 
      Rgf.create_cursor
      n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum Rgf.next
  }

  (* Sorted arrays of a given size on [0..k-1]. *)
  let sorted_size (k: int) (size: int) : (int array) gen = {
    create = (fun n ->
      EnumAdapter.create_of_enum 
        (fun n -> Sorted.create_cursor n (Z.of_int k))
        n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum 
      (fun c -> Sorted.next c (Z.of_int k))
  }

  (* Bounded arrays of a given size on [0..k-1]. *)
  let barray_size (k: int) (size: int) : (int array) gen = {
    create = (fun n -> 
      EnumAdapter.create_of_enum 
        (fun n -> Barray.create_cursor n (Z.of_int k))
        n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum
     (fun c -> Barray.next c (Z.of_int k))
  }

  (* Endofunctions on [0..size-1]. *)
  let endo_size (size: int) : (int array) gen = {
    create = (fun n -> 
      EnumAdapter.create_of_enum 
      Endo.create_cursor
      n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum Endo.next
  }

  (* Sorted arrays of a given size on [0..k-1], by filtering bounded arrays. *)
  let sorted_barray_size (k: int) (size: int) : (int array) gen = {
    create = (fun n ->
      EnumAdapter.create_of_enum 
        (fun n -> SortedBarray.create_cursor n (Z.of_int k))
        n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum 
      (fun c -> SortedBarray.next c (Z.of_int k))
  }

  (* Injections from [0..size-1] to [0..k-1], by filtering. *)
  let inj_barray_size (k: int) (size: int) : (int array) gen = {
    create = (fun n ->
      EnumAdapter.create_of_enum 
        (fun n -> InjBarray.create_cursor n (Z.of_int k))
        n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum 
      (fun c -> InjBarray.next c (Z.of_int k))
  }

  (* Surjections from [0..size-1] to [0..k-1], by filtering. *)
  let surj_barray_size (k: int) (size: int) : (int array) gen = {
    create = (fun n ->
      EnumAdapter.create_of_enum 
        (fun n -> SurjBarray.create_cursor n (Z.of_int k))
        n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum 
      (fun c -> SurjBarray.next c (Z.of_int k))
  }

  (* Combinations of size elements from k in [0..k-1]. *)
  let comb_barray_size (k: int) (size: int) : (int array) gen = {
    create = (fun n ->
      EnumAdapter.create_of_enum 
        (fun n -> CombBarray.create_cursor n (Z.of_int k))
        n);
    has_next = (fun c -> c.is_new);
    next = EnumAdapter.next_of_enum 
      (fun c -> CombBarray.next c (Z.of_int k))
  }

end

type 'a serial = {
  gen: 'a Gen.gen;
  size: int;
  print: ('a -> string) option; (* to print values *)
}

let unit : unit serial = { 
  gen = Gen.unit;
  size = 0;
  print = Some Print.unit
}

let bool : bool serial = {
  gen = Gen.bool;
  size = 0;
  print = Some Print.bool
}

(* As QCheck._opt_map, just a patch for let opt below. *)

let _opt_map ~f = function
  | None -> None
  | Some x -> Some (f x)

(* TODO: replace
    print = _opt_map ~f:Print.option a.print
   by
    print = (QCheck._opt_map ~f:(Print.option a.print))
   after understanding why QCheck._opt_map is not visible here
*)

let option (a: 'a serial) : ('a option) serial = {
  gen = Gen.opt a.gen;
  size = a.size+1;
  print = _opt_map ~f:Print.option a.print
}

let int_range (a: int) (b: int) : int serial = {
  gen = Gen.int_range a b;
  size = 0;
  print = Some Print.int
}

let int_bound (n: int) : int serial = {
  gen = Gen.int_bound n;
  size = 0;
  print = Some Print.int
}

let fact_of_size size : (int array) serial = {
  gen = Gen.fact_size size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let permut_of_size size : (int array) serial = {
  gen = Gen.permut_size size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let rgf_of_size size : (int array) serial = {
  gen = Gen.rgf_size size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let endo_of_size size : (int array) serial = {
  gen = Gen.endo_size size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let sorted_of_size k size : (int array) serial = {
  gen = Gen.sorted_size k size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let barray_of_size k size : (int array) serial = {
  gen = Gen.barray_size k size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let sorted_barray_of_size k size : (int array) serial = {
  gen = Gen.sorted_barray_size k size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let inj_barray_of_size k size : (int array) serial = {
  gen = Gen.inj_barray_size k size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let surj_barray_of_size k size : (int array) serial = {
  gen = Gen.surj_barray_size k size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

let comb_barray_of_size k size : (int array) serial = {
  gen = Gen.comb_barray_size k size;
  size = size;
  print = _opt_map ~f:Print.array int.print
}

module Test = struct
  type 'a cell = {
    law: 'a -> bool; (* the law to check *)
    series: 'a serial;
    mutable name : string; (* name of the law *)
  }

  (* Same function as QCheck.Test.fresh_name but visibility issue.
     TODO: use QCheck.Test.fresh_name instead. *)
  let fresh_name =
    let r = ref 0 in
    (fun () -> incr r; Printf.sprintf "anon_test_%d" !r)

  type t = | EnumTest : 'a cell -> t

  let get_name {name; _} = name
  let set_name c name = c.name <- name
  let get_law {law; _} = law
  let get_series {series; _} = series

  let make_cell ?(name=fresh_name()) s law =
    { law = law; series = s; name = name }

  (* make : type a. (a serial) -> (a -> (bool)) ->  (a cell) *)
  let make ?name s law =
    EnumTest (make_cell ?name s law)

  type 'a verdict = {
    test: 'a cell;
    witness: 'a option;
    rank: int;
    prt: ('a -> string) option;
  }

(* TODO: replace the local management of rank
   by its management in the cursor? *)

  let exhaust_cell : type a. (a cell) -> (a verdict) =
    fun tc ->
    let exception QtReturn of (a verdict) in
    try
      let s = tc.series in
      let n = s.size in
      let oracle = tc.law in
      let g = s.gen in
      let r = ref 0 in
      let cr = g.create in
      let hn = g.has_next in
      let next1 = g.next in
      let x = ref (cr n) in
      while hn (!x) do
      if oracle ((!x).item) then begin
        x.contents <- next1 (!x);
        r := !r+1;
      end
      else
        raise (QtReturn ({ 
          test = tc;
          witness = Some ((!x).item);
          rank = !r;
          prt = s.print
        }))
      done;
      { test = tc;
        witness = None;
        rank =  !r;
        prt = s.print
      }
            with
            | QtReturn r -> r

  let exhaust = exhaust_cell



end

