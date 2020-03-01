(********************************************************************)
(* Copyright (C) 2020 Alain Giorgetti and Clotilde Erard            *)
(* FEMTO-ST institute                                               *)
(********************************************************************)

(********************************************************************)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1                              *)
(********************************************************************)

(* TestExamples.ml: Examples of automated tests (random tests, enumerative tests
   and combinations thereof) on OCaml properties.
*)

open QCheck (* Random testing tool *)
open SCheck (* Enumerative testing tool *)
open SCheck_runner

open Enum

(* Some properties of some functions from the following OCaml modules are tested: *)
open List
open Array
open Inverse_in_place

(** Properties shared between random and enumerative tests *)
module Properties = struct

  let implb (x: bool) (y: bool) : bool =
    match x with
    | true  -> y
    | false -> true

  let equivb (x: bool) (y: bool) : bool =
    match x with
    | true -> y
    | false -> not y
end

(** Random tests *)
module RandomTests = struct

  (* Wrong function on unit type *)
  let is_unit (x: unit) : bool =
    if x = () then false else true

  let is_unit_test = QCheck_runner.run_tests [
    QCheck.Test.make
      ~name:"1: randomly, \"() is not of type unit\""
      QCheck.unit
      is_unit
  ]

  (** bool type *)

  let is_bool (x: bool) : bool = not x

  let is_bool_test = QCheck_runner.run_tests [
    QCheck.Test.make
      ~name:"2: randomly, \"is_bool does not characterize Booleans\""
      QCheck.bool
      (fun b -> not (is_bool b))
  ]

  (** Tests with a pair of Booleans *)

  let equivb_prop (x: bool*bool) : bool =
    let (a,b) = x in
    if (Properties.implb a b) && (Properties.implb b a) then
      Properties.equivb a b
    else
      not (Properties.equivb a b)

  let equivb_test = QCheck_runner.run_tests [
    QCheck.Test.make
      ~name:"3: randomly, \"(equivb a b) is not equivalent to (implb a b && implb b a)\""
      QCheck.(pair bool bool)
      (fun x -> not (equivb_prop x))
  ]

  (** option type *)

  let is_none (x: 'a option) : bool =
    match x with None -> true | _ -> false

  let is_none_test = QCheck_runner.run_tests [
    QCheck.Test.make
      ~name:"4: randomly, \"All inhabitants of (bool option) are None\""
      QCheck.(option QCheck.bool)
      is_none
  ]

  (** list type *)

  let is_nil (l: 'a list) : bool =
    match l with [] -> true | _ -> false

  let test_is_nil =
    QCheck_runner.run_tests [
      QCheck.Test.make
        ~name:"5: randomly, \"All inhabitants of (int list) are []\""
        QCheck.(list QCheck.int)
        is_nil
   ]

  (** array type *)

  (* TODO: length make *)

  (* Read-write axioms *)
  let rw_axioms (a: int array) (i: int) (j: int) (v: int) : bool =
    let b = Array.copy a in
    set b i v;
    if
      (i = j && get b j = v) ||
      (i <> j && get b j = get a j)
    then true else false

  let rw_axioms_quad (x:(int array*int*int*int)) : bool =
    let (a,i,j,v) = x in
    rw_axioms a i j v

  (* (sized_array_gen n) generates an (int array) of length n at random *)

  let (sized_array_gen : int -> Random.State.t -> int array) =
    fun n -> QCheck.Gen.array_size (fun _ -> n) QCheck.Gen.int

  (* rw_axioms_gen generates a quadruple (a,i,j,v) to test rw_axioms. *)

  let (rw_axioms_gen : Random.State.t -> int array * int * int * int) =
    fun st ->
      let n = QCheck.Gen.int_range 1 5 st in
      (QCheck.Gen.quad
        (sized_array_gen n)
        (QCheck.Gen.int_bound (n-1))
        (QCheck.Gen.int_bound (n-1))
        QCheck.Gen.int
      )
      st

let (rw_axioms_arbitrary : (int array * int * int * int) arbitrary) =
  QCheck.make
    ~print:(QCheck.Print.(quad (array int) int int int))
    rw_axioms_gen

  let rw_axioms_test = QCheck_runner.run_tests [
    QCheck.Test.make
      ~name:"\"RW axioms\""
      rw_axioms_arbitrary
      rw_axioms_quad
  ]

  (* Wrong mutation in read-write axioms: get a i instead of 
     get a j *)
  let wrong_rw_axioms_quad (x:(int array*int*int*int)) : bool =
    let (a,i,j,v) = x in
    let b = Array.copy a in
    set b i v;
    if
      (i = j && get b j = v) ||
      (i <> j && get b j = get a i)
    then true else false

  let wrong_rw_axioms_test = QCheck_runner.run_tests [
    QCheck.Test.make
      ~name:"6: randomly, \"Mutation in RW axioms\""
      rw_axioms_arbitrary
      wrong_rw_axioms_quad
  ]
end

(** Enumerative tests *)
module EnumerativeTests = struct

  (* Wrong function on unit type *)
  let is_unit (x: unit) : bool =
    if x = () then false else true

  let unit_test = SCheck_runner.run_tests [
    SCheck.Test.make
      ~name:"1 by enumeration: \"() is not of type unit\""
      SCheck.unit
      is_unit
    ]

  let is_bool (x: bool) : bool =
    match x with
    | true -> true
    | false -> true
    | _ -> false

  let bool_test = SCheck_runner.run_tests [
    SCheck.Test.make
      ~name:"2 by enumeration: \"is_bool does not characterize Booleans\""
      SCheck.bool
      (fun b -> not (is_bool b))
    ]

  let is_none (x: 'a option) : bool =
    match x with None -> true | _ -> false

  let is_none_test = SCheck_runner.run_tests [
    Test.make
    ~name:"3 by enumeration: \"All inhabitants of (bool option) are None\""
     (SCheck.option (SCheck.bool))
     is_none
  ]


  (** Examples of properties tested with ENUM generators *)

  let b_fact_prop (a: int array) : bool =
    Enum.Fact.b_fact (EnumAdapter.of_int_array a)

  (* The generator of factorial arrays is sound: *)
  let b_fact_test = SCheck_runner.run_tests [
    Test.make
      ~name: "4: by certified enumeration, \"The enumerator of factorial arrays of size 5 is sound\""
      (SCheck.fact_of_size 5)
      b_fact_prop
    ]

  let is_inj (a: int array) : bool =
    Enum.ArrayExtension.b_inj (EnumAdapter.of_int_array a)

  (* Some factorial arrays (of size 4) are not injective: *)

  let is_inj_test = SCheck_runner.run_tests [
    Test.make
      ~name:"5: by certified enumeration, \"All factorial arrays of size 4 are injective\""
      (SCheck.fact_of_size 4)
      is_inj
  ]

  let b_permut_prop (a: int array) : bool =
    Enum.Permutation.b_permut (EnumAdapter.of_int_array a)

  (* The generator of permutation arrays is sound: *)
  let b_permut_test = SCheck_runner.run_tests [
    Test.make
      ~name:"6: by certified enumeration, \"The enumerator of permutation arrays of size 5 is sound\""
      (SCheck.permut_of_size 5)
      b_permut_prop
    ]

  (* The function inverse_in_place preserves permutations *)

  let inverse_in_place_permut (a: int array) : bool =
    let b = EnumAdapter.of_int_array a in
    inverse_in_place b;
    b_permut_prop (EnumAdapter.to_int_array b)

  let inverse_in_place_permut_test = SCheck_runner.run_tests [
    Test.make
      ~name:"7: by certified enumeration, \"The function inverse_in_place preserves permutations of size 6\""
      (SCheck.permut_of_size 6)
      inverse_in_place_permut
    ]

  (* The function inverse_in_place computes the inverse permutation. *)

  (* Implementation of the postcondition
       ensures  { forall i. 0 <= i < length a -> (old a)[a[i]] = i }
  *)

  let inverse_values (a: int array) (b: int array) (i: int) : bool = ((get a (get b i)) = i)

  let inverse_in_place_inverse (a: int array) : bool =
    let olda = EnumAdapter.of_int_array a in
    let newa = copy olda in
    inverse_in_place newa;
    let newaz = EnumAdapter.to_int_array newa in
    for_all (inverse_values a newaz) newaz

  let inverse_in_place_inverse_test = SCheck_runner.run_tests [
    Test.make
      ~name:"8: by certified enumeration, \"The function inverse_in_place computes the inverse of all permutations of size 6\""
      (SCheck.permut_of_size 6)
      inverse_in_place_inverse
    ]
end

(* TODO : module MixedTests *)
