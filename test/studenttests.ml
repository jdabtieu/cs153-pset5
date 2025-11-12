open Testlib
open Util.Assert
open X86
open Ll
module Driver = Oat.Driver
module Backend = Oat.Backend
module Typechecker = Oat.Typechecker
module Frontend = Oat.Frontend
module Tctxt = Oat.Tctxt
open Backend
open Driver

(* Use this file to create additional test cases here to help you   *)
(* debug your comiplper                                             *)

let student_local_tests : suite = [
  Test("Student Ed Posts",
    executed_oat_file [
      ("hw4programs/nqueens.oat", " ", "10000000\n00000010\n00001000\n00000001\n01000000\n00010000\n00000100\n001000000");
      ("hw5programs/trie.oat", "", "Wrong Answer on query 110");
    ]
  );
  Test("personal unittests", executed_oat_file [
    ("hw5programs/compile_array_init2.oat", "", "2");
    ("hw5programs/tc_correct_func_cast.oat", "", "12");
  ]);
  Test("Positive Ed tests", typecheck_file_correct [
    "hw5programs/tc_struct_ok.oat";
    "hw5programs/tc_correct_func_cast.oat";
  ]);
  Test("Negative Ed tests", typecheck_file_error [
    "hw5programs/tc_recursive_struct_err.oat";
    "hw5programs/tc_error_func_cast.oat";
  ]);
]
