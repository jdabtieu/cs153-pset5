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

let personal_unittests = [
  ("hw5programs/compile_array_init2.oat", "", "2");
]


let student_local_tests : suite = [
  
] @ [
  GradedTest("personal unittests", 1, executed_oat_file personal_unittests)
]
