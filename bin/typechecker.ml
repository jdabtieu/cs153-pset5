open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) (err : string) = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match (t1, t2) with
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | (TNullRef st1, TNullRef st2) -> subtype_ref c st1 st2
  | (TRef st1, TRef st2) -> subtype_ref c st1 st2
  | (TRef st1, TNullRef st2) -> subtype_ref c st1 st2
  | _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  match (t1, t2) with
  | (RString, RString) -> true
  | (RArray t1_elem, RArray t2_elem) -> t1_elem = t2_elem
  | (RStruct s1, RStruct s2) -> (
    match Tctxt.lookup_struct_option s1 c, Tctxt.lookup_struct_option s2 c with
    | Some fields1, Some fields2 -> List.for_all (fun f -> List.mem f fields1) fields2
    | _ -> false
  )
  | (RFun (args1, rt1), RFun (args2, rt2)) ->
      List.length args1 = List.length args2 &&
      List.for_all2 (fun a1 a2 -> subtype c a2 a1) args1 args2 &&
      subtype_rt c rt1 rt2
  | _ -> false

(* Decides whether H |-rt rt1 <: rt2 *)
  and subtype_rt (c : Tctxt.t) (rt1 : Ast.ret_ty) (rt2 : Ast.ret_ty) : bool =
    match (rt1, rt2) with
    | (RetVoid, RetVoid) -> true
    | (RetVal t1, RetVal t2) -> subtype c t1 t2
    | _ -> false

(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TInt -> ()
  | TBool -> ()
  | TRef r -> typecheck_ty_ref l tc r
  | TNullRef r -> typecheck_ty_ref l tc r

and typecheck_ty_ref (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with
  | RString -> ()
  | RArray elem_ty -> typecheck_ty l tc elem_ty
  | RStruct id -> (
    match Tctxt.lookup_struct_option id tc with
    | Some _ -> ()
    | None -> type_error l ("Undefined struct " ^ id)
  )
  | RFun (arg_tys, ret_ty) ->
    let _ = List.for_all (fun ty -> typecheck_ty l tc ty; true) arg_tys in
    let _ = typecheck_ty_rt l tc ret_ty in
    ()

and typecheck_ty_rt (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty

(* A helper function to determine whether a type allows the null value *)
let is_nullable_ty (t : Ast.ty) : bool =
  match t with
  | TNullRef _ -> true
  | _ -> false

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
(* TODO what's the diff between H |- vs H;G;L |- *)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  match e.elt with
  | Bop (op, e1, e2) ->
    let t1 = typecheck_exp c e1 in
    let t2 = typecheck_exp c e2 in (
    match op with
    | Eq | Neq ->
      if subtype c t1 t2 && subtype c t2 t1 then TBool
      else type_error e "Invalid equality operands"
    | _ ->
      let expect1, expect2, tr = typ_of_binop op in 
      if t1 = expect1 && t2 = expect2 then tr
      else type_error e "Invalid binop operands"
    )
  | Uop (op, e) ->
    let t = typecheck_exp c e in
    let expect, tr = typ_of_unop op in
    if t = expect then tr
    else type_error e "Invalid uop operand"
  | CNull rty -> typecheck_ty e c (TRef rty); TNullRef rty
  | CBool _ -> TBool
  | CInt _ -> TInt
  | CStr _ -> TRef RString
  | Lhs l -> let t, _ = typecheck_lhs c l in t
  | CArr (ty, exps) ->
    typecheck_ty e c ty;
    if List.for_all (fun exp -> subtype c (typecheck_exp c exp) ty) exps
    then TRef (RArray ty)
    else type_error e "Invalid array element type"
  | NewArr (ty, len_exp) ->
    typecheck_ty e c ty;
    (match typecheck_exp c len_exp with
    | TInt -> ()
    | _ -> type_error e "Array length must be an integer expression"
    );
    (match ty with
    | TInt | TBool | TNullRef _ -> TRef (RArray ty)
    | TRef _ -> type_error e "Default-initialized array cannot be of nonnull reference type"
    )
  | NewArrInit (ty, len_exp, id, init_exp) ->
    typecheck_ty e c ty;
    (match typecheck_exp c len_exp with
    | TInt -> ()
    | _ -> type_error e "Array length must be an integer expression"
    );
    (match lookup_local_option id c with
    | Some _ -> type_error e ("Array initializer variable " ^ id ^ " already defined")
    | None -> ()
    );
    let new_ctxt = Tctxt.add_local c id TInt in
    if subtype c (typecheck_exp new_ctxt init_exp) ty
    then TRef (RArray ty)
    else type_error e "Invalid array initializer type"
  | Length arr_exp -> (
    match typecheck_exp c arr_exp with
    | TRef (RArray _) -> TInt
    | _ -> type_error e "Length expression must be an array"
    )
  | CStruct (st_name, fields) -> (
    match Tctxt.lookup_struct_option st_name c with
    | None -> type_error e ("Undefined struct " ^ st_name)
    | Some st_fields ->
      let sorted_decl_fields = List.sort (fun f1 f2 -> String.compare f1.fieldName f2.fieldName) st_fields in
      let sorted_exp_fields = List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) fields in
      if List.length sorted_decl_fields <> List.length sorted_exp_fields then
        type_error e ("Incorrect number of fields in struct " ^ st_name);
      let _ = List.for_all2 (fun decl_field (exp_name, exp_value) ->
        if decl_field.fieldName <> exp_name then
          type_error e ("Undefined field name " ^ exp_name ^ " in struct " ^ st_name)
        else
          let exp_ty = typecheck_exp c exp_value in
          if subtype c exp_ty decl_field.ftyp then
            true
          else
            type_error e ("Incorrect type for field " ^ exp_name ^ " in struct " ^ st_name)
      ) sorted_decl_fields sorted_exp_fields in
      TRef (RStruct st_name)
    )
  | Call (fn_exp, arg_exps) -> (
    match typecheck_exp c fn_exp with
    | TRef (RFun (param_tys, ret_ty)) -> 
      if List.length param_tys <> List.length arg_exps then
        type_error e "Incorrect number of function arguments";
      let _ = List.for_all2 (fun param_ty arg_exp ->
        let arg_ty = typecheck_exp c arg_exp in
        if subtype c arg_ty param_ty then
          true
        else
          type_error e "Function argument type mismatch"
      ) param_tys arg_exps in (
      match ret_ty with 
      | RetVoid -> type_error e "Attempting to use void return value"
      | RetVal ty -> ty
      )
    | _ -> type_error e "Attempting to call a non-function type"
    )

(* Typechecks a lhs expression in the typing context c.  Returns the
   type of result, along with a boolean flag indicating whether
   the lhs is "assignable".
   INVARIANT:
     If the flag is true, we can think of lhs as denoting a reference
     to a value of the returned type.

     If the flag is false, the lhs is a (globally defined) function
     pointer (which cannot be written to).
 *)
and typecheck_lhs (c : Tctxt.t) (l : Ast.lhs node) : Ast.ty * bool =
  match l.elt with
  | Id id -> (
    match Tctxt.lookup_local_option id c with
    | Some ty -> (ty, true)
    | None -> (
      match Tctxt.lookup_global_option id c with
      | None -> type_error l ("Undefined identifier " ^ id)
      | Some (TRef (RFun (arg_tys, ret_ty))) -> (TRef (RFun (arg_tys, ret_ty)), false)
      | Some ty -> (ty, true)
    )
  )
  | Index (arr_id, exp) -> (
    match typecheck_exp c arr_id with
    | TRef (RArray elem_ty) -> (
      match typecheck_exp c exp with
      | TInt -> (elem_ty, true)
      | _ -> type_error l "Array index must be an integer"
    )
    | _ -> type_error l "Indexing a non-array type"
  )
  | Proj (struct_id, field) -> (
    match typecheck_exp c struct_id with
    | TRef (RStruct st_name) -> (
      match Tctxt.lookup_field_option st_name field c with
      | Some f_ty -> (f_ty, true)
      | None -> type_error l ("Undefined field " ^ field ^ " in struct " ^ st_name)
    )
    | _ -> type_error l "Projecting a non-struct type"
  )

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement)

     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, the return behavior of the branching 
        statement is the conjunction of the return behavior of the two 
        branches: both both branches must definitely return in order for 
        the whole statement to definitely return.

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entire conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  failwith "todo: implement typecheck_stmt"


(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups (fs : field list) =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) (id : id) (fs : field list)  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - ensures formal parameters are distinct
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  failwith "todo: typecheck_fdecl"

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'H'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can mention only other global values that were declared earlier
*)

let rec create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  match p with
  | [] -> Tctxt.empty
  | Gtdecl ({elt=(id, fs)} as l) :: rest ->
    let tc_rest = create_struct_ctxt rest in
    typecheck_tdecl tc_rest id fs l;
    Tctxt.add_struct tc_rest id fs
  | _ :: rest -> create_struct_ctxt rest

let rec create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  match p with
  | [] -> tc
  | Gfdecl ({elt=f} as l) :: rest ->
    let tc_rest = create_function_ctxt tc rest in
    (match Tctxt.lookup_global_option f.fname tc_rest with
    | Some _ -> type_error l ("Redefinition of function " ^ f.fname)
    | None -> ());
    Tctxt.add_global tc_rest f.fname (TRef (RFun (List.map fst f.args, f.frtyp)))
  | _ :: rest -> create_function_ctxt tc rest

let rec create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  match p with
  | [] -> tc
  | Gvdecl ({elt=g} as l) :: rest ->
    let tc_rest = create_global_ctxt tc rest in
    let g_ty = typecheck_exp tc_rest g.init in
    typecheck_ty l tc_rest g_ty;
    Tctxt.add_global tc_rest g.name g_ty
  | _ :: rest -> create_global_ctxt tc rest

(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p
