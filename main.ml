(* 
current score:
36/40
*)
(*
Basically, you’ll look at classes, methods and attibutes (but not method bodies).

*)
(*
Some code stolen from westley weimer
https://www.youtube.com/watch?v=Wa9zMygcv_M&ab_channel=WestleyWeimer
*)


(*

we can test out type chceker by running it against good code-
- helps develop completeness

or running it against bad code (the test cases we made)
- helps develop soundness

*)
open Printf


(* static type of cool expression*)
type static_type = 
| Class of string (*Int, or Object etc*)
| SELF_TYPE of string (*implement later*)

let type_to_str t = match t with
| Class(x) -> x
| SELF_TYPE(c) -> "SELF_TYPE"

(* liskov substitution principle*)
(* Int <= Object *)
(* String <= String *)
let rec is_subtype t1 t2 = 
  match t1, t2 with
  | Class(x), Class(y) when x = y -> true
  | Class(x), Class("Object") -> true
  | Class(x), Class(y) -> false (*TODO: check parent map*)
  | _,_ -> false (* TODO: SELF_TYPE shenanigans *)
(*
typechecking will need O, M, C
type environment consists of three parts:
Objects - mapping from object identifiers (names) to types!
    * for example, in a let expression we need the type of the variable so that we can...
        * check if hte initializer expression has the right type
        * typecheck the let body (assuming we use the variable in it.)

Methods - mapping from (Class C, method f) to signature (check page 22 of CRM)
    *note - the signature of the method is the tuple of types.
    *example - (t1,t2,...tn-1,tn)
    * to read, In class C, method f has formal parameter of types (t1,...,tn-1)
        , and a return type of tn
    * signature types to do typechecking on the signatures. 
        
Current class - name of current class, this is needed for type rules involving SELF_TYPE
 *)
type object_environment = (string, static_type) Hashtbl.t
(* we only care about the types *)
type method_environment = ((string * string), (static_type list * static_type)) Hashtbl.t 


let empty_object_environment (): object_environment = Hashtbl.create 255
let m_env:method_environment  = Hashtbl.create 255 

(*
update with current class when encountering SELF_TYPE
now we can get access to current class in the recursive typechecking function.
*)
let current_class : string ref = ref "" 

type cool_program = cool_class list
and loc = string (*really an integer*)
and id = loc * string 

(*not a big difference between object idenfiers and type identifiers*)
(* both strings wiht location*)
and cool_type = id 

(*id option for optional inheritance*)
(* a class is an identifier, with an optoinal idenfier (superclass), the class has a list of features.*)
and cool_class = id * (id option) * feature list

(*variant types use to pattern match accross different expressions*)
and feature =
  | Attribute of id * cool_type * (exp option) (*attribute is an identifier with a type, with an optional expression (initalizer)*)
  | Method of id * (formal list) * cool_type * exp

and formal = id * cool_type (*formals are the parameters listed in the method signature*)

and binding = id * cool_type * (exp option) (* let initialize or no intialize*)
and case_element = id * cool_type * exp 

(* wrapper for all things expressions *)
and exp = {
  loc: loc;
  exp_kind:exp_kind;
  (* starts out as None, but in the process of type  checking we'll fill it out. *)
  mutable static_type : static_type option; 
}
and exp_kind = (* represents the type and values of expressions*)
  | Assign of id * exp
  | Dynamic_Dispatch of exp * id * exp list
  | Static_Dispatch of exp * id * id * exp list
  | Self_Dispatch of id * exp list
  | If of exp * exp * exp (* if then else*)
  | While of exp * exp
  | Block of exp list (* last expression is the blocks value *) 
  | New of cool_type 
  | IsVoid of exp
  | Plus of exp * exp
  | Minus of exp * exp 
  | Times of exp * exp
  | Divide of exp * exp
  | LessThan of exp * exp
  | LessThanEqual of exp * exp
  | Equal of exp * exp
  | Not of exp 
  | Negate of exp
  | Integer of string (*really an int*)
  | String of string
  | Identifier of id
  | Bool of string (*true or false*)
  | Let of binding list * exp (*id ends up being the count *)
  | Case of loc * exp * case_element list
  | Internal of string (* functions for predefined methods *)

let main () = begin 

  (* de serialize cl-ast file*)
  let fname = Sys.argv.(1) in

  (* file input *)
  let fin = open_in fname in 

  let read () = 
    input_line fin (*read up until newline*)
  in

  (* create a decrementing list of numbers*)
  let rec range k = 
    if k <= 0 then []
    else k ::(range (k-1))
  in 

  (* 
  reads in a number first, then
  calls worker k times and 
    returns a list containing the outputs of worker
  *)
  let read_list worker =  
    let k = int_of_string(read()) in
    let lst = range k in
    List.map(fun _ -> worker()) lst
  in

  let rec read_cool_program() = 
    read_list read_cool_class

  and read_id() =  
    let loc = read() in
    let name = read() in
    (loc,name)
  
  and read_cool_class () =
    let cname = read_id () in 
    let inherits = match read() with
    | "no_inherits" -> None
    | "inherits" ->
      let super = read_id () in
      Some(super)
    | x -> failwith ("error reading class:" ^ x)
    in
    let features = read_list read_feature in
    (cname, inherits, features)
  and read_feature () = 
    let feature_type = read() in
    match feature_type with
    | "attribute_no_init" ->
      let fname = read_id() in
      let ftype = read_id() in
      Attribute(fname, ftype, None) 
    | "attribute_init" ->
      let fname = read_id() in
      let ftype = read_id() in
      let finit = read_exp() in
      Attribute(fname, ftype, (Some finit))
    | "method" ->
      let mname = read_id() in
      let formals = read_list read_formal in
      let mtype = read_id() in
      let mbody = read_exp() in 
      Method(mname,formals,mtype,mbody)
    | x -> failwith ("error reading feature:" ^ x)
  and read_formal () =
    let fname = read_id () in
    let ftype = read_id() in
    (fname, ftype)
  and read_binding () =
    match read() with
    | "let_binding_no_init" ->
      let bname = read_id() in
      let btype = read_id() in
      (bname,btype,None)
    | "let_binding_init" ->
      let bname = read_id() in
      let btype = read_id() in
      let bval = read_exp() in
      (bname,btype,Some(bval))
    | x -> failwith("error reading let binding:" ^x)
  and read_case_element() = 
    let cevar = read_id() in
    let cetype = read_id() in
    let cebody = read_exp() in
    (cevar,cetype,cebody) 
  and read_exp () =
    let eloc = read() in
    let ekind  = match read() with
    | "assign" ->
      let id = read_id() in
      let exp = read_exp() in
      Assign(id,exp)
    | "dynamic_dispatch" ->
      let exp = read_exp() in
      let id = read_id() in
      let exp_list = read_list read_exp in
      Dynamic_Dispatch(exp,id,exp_list)
    | "static_dispatch" ->
      let exp = read_exp() in
      let id1 = read_id() in
      let id2 = read_id() in
      let exp_list = read_list read_exp in
      Static_Dispatch(exp,id1,id2,exp_list)
    | "self_dispatch" ->
      let id = read_id() in
      let expl = read_list (read_exp) in
      Self_Dispatch(id,expl)
    | "if" ->
      let _if = read_exp() in
      let _then = read_exp() in
      let _else = read_exp() in
      If(_if,_then,_else)
    | "while" ->
      let _while = read_exp() in
      let body = read_exp() in
      While(_while,body)
    | "block" ->
      Block(read_list read_exp)
    | "new" ->
      let ntype = read_id() in (*the class name*)
      New(ntype) 
    | "isvoid" ->
      let exp = read_exp() in
      IsVoid(exp)
    | "plus" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      Plus(exp1,exp2)
    | "minus" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      Minus(exp1,exp2)
    | "times" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      Times(exp1,exp2)
    | "divide" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      Divide(exp1,exp2)
    | "lt" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      LessThan(exp1,exp2)
    | "le" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      LessThanEqual(exp1,exp2)
    | "eq" ->
      let exp1 = read_exp() in
      let exp2 = read_exp() in
      Equal(exp1,exp2)
    | "not" ->
      let exp = read_exp() in
      Not(exp)
    | "negate" ->
      let exp = read_exp() in
      Negate(exp)
    | "integer" ->
      let ival=read() in 
      Integer(ival)
    | "string" ->
      let sval=read() in
      String(sval)
    | "identifier" -> 
      let ident = read_id () in
      Identifier(ident)
    | "false" ->
      Bool("false")
    | "true" ->
      Bool("true")
    | "let" ->
      let binding_list = read_list(read_binding) in
      let body = read_exp() in 
      Let(binding_list,body)
    | "case" ->
      let case = read_exp() in
      let case_elements = read_list(read_case_element) in
      Case(eloc,case,case_elements)
    | x -> (*do all of other expressions*)
      failwith ("expression kind unhandled: " ^ x)
    in 
    {
      loc = eloc;
      exp_kind = ekind;
      static_type = None; (* havent annotated yet*)
    }
  in

  let ast = read_cool_program() in
  close_in fin;
  (*Check for class related errors*)

  let base_classes = ["Int" ; "String" ; "Bool" ; "IO" ; "Object"] in 
  let user_classes = List.map(fun ((_,cname),_,_) -> cname) ast in
  let all_classes = base_classes @ user_classes in
  let all_classes = List.sort compare all_classes in

  (*THEME IN PA2 - make internal data structure to hold helper 
information so you can do the checks more easily.*)
  (* NEED TO CONSIDER ATTRIBUTES FROM BASE CLASSES 
  1. construct a mapping from child to parent
    use toposort here to find the right order of traversal (or detect inheritance cycles)
  2. recursively walk up mapping until get object
  3. add in all of attributes that we find
  4. while there look for attribute override problems.

  we use toposort to find the right order to gather up attributes
    need to do this to correctly consider shadowing.
  *)


  (* BEGIN TYPECHECKING *)
  let build_parent_map ast = 
    let parent_map = Hashtbl.create (List.length ast) in
    List.iter(fun ((_,cname), inherits, _) -> 
      match inherits with
      (* user declared class in ast that does not have parent *)
      | None ->
        (
          (* printf "%s does not have a parent, assigning Object as its parent.\n" cname; *)
           Hashtbl.add parent_map cname "Object" 
        )
      | Some(_,pname)->
        (
          (* printf "%s has a parent named %s\n" cname pname; *)
           Hashtbl.add parent_map cname pname
        )
      )ast;
    (* add object as parent for predefined classes *)
    List.iter (fun cname -> 
      match cname with
      | "IO" | "Int" | "String" | "Bool" -> 
        Hashtbl.add parent_map cname "Object" 
      | _ -> ();
      ) all_classes;
    parent_map 
  in
  let detect_inheritance_cycles parent_map =
    (* ensure we dont visit the same classes *)
    let visited = Hashtbl.create (Hashtbl.length parent_map) in
    let rec dfs cname path =
      (* already encountered this class! (in children classes) *)
      if List.mem cname path then begin
        printf "ERROR: 0: Type-Check: cyclic inheritance involving class %s and %s\n" cname (Hashtbl.find parent_map cname);
        exit 1
      end;
      
      (* traverse up inheritance tree if we have ohterwise not traversed to the class before *)
      if not (Hashtbl.mem visited cname) then begin 
        Hashtbl.add visited cname true;

        (* see if there are any parent nodes to traverse to. *)
        match Hashtbl.find_opt parent_map cname with
        | Some parent -> dfs parent (cname :: path) (* keep track of previous children classes *)
        | None -> ()
      end
    in
    (* dfs each class seperate group of inherited classes *)
    Hashtbl.iter(fun cname _ -> dfs cname []) parent_map 
  in


  (* PREDEFINED METHODS *)
  (* cheated it a bit and manually put the object methods in alphabetical order *)
  let object_methods = [
      Method(("0","abort"),[],("0","Object"),{loc="0";exp_kind=Internal("Object.abort"); static_type= Some(Class("Object"))});
      Method(("0","copy"),[],("0","SELF_TYPE"),{loc="0";exp_kind=Internal("Object.copy"); static_type= Some(SELF_TYPE("Object"))});
      Method(("0","type_name"),[],("0","String"),{loc="0";exp_kind=Internal("Object.type_name"); static_type= Some(Class("String"))});
  ]
  in
  let io_methods = [
      Method(("0","out_string"),[(("0","x"),("0","String"))],("0","SELF_TYPE"),{loc="0";exp_kind=Internal("IO.out_string");static_type=Some(Class("SELF_TYPE"))});
      Method(("0","out_int"),[(("0","x"),("0","Int"))],("0","SELF_TYPE"),{loc="0";exp_kind=Internal("IO.out_int");static_type=Some(Class("SELF_TYPE"))});
      Method(("0","in_string"),[],("0","String"),{loc="0";exp_kind=Internal("IO.in_string");static_type=Some(Class("String"))});
      Method(("0","in_int"),[],("0","Int"),{loc="0";exp_kind=Internal("IO.in_int");static_type=Some(Class("Int"))});
  ]
  in
  let string_methods = [

      Method(("0","length"),[],("0","Int"),{loc="0";exp_kind=Internal("String.length");static_type=Some(Class("Int"))});
      Method(("0","concat"),[(("0","s"),("0","String"))],("0","String"),{loc="0";exp_kind=Internal("String.concat");static_type=Some(Class("String"))});
      Method(("0","substr"),[(("0","i"),("0","Int"));(("0","l"),("0","Int"))],("0","String"),{loc="0";exp_kind=Internal("String.substr");static_type=Some(Class("String"))});
  ]
  in
  (* 
  given a class name, collect all of its features(including from inherited classes 
  does this by merging the current class and parent class features.
    does this recursively. (keeps going up the inheritance tree)
  *)
  let rec collect_features parent_map ast cname =
    (* get parent class name if there is one *)
    let parent_features = (match Hashtbl.find_opt parent_map cname with
    | Some parent -> (
      (* printf "%s has parent %s\n" cname parent; *)
      collect_features parent_map ast parent
    )
    | None -> [] (* base class, no parent*)
    )
    in  
    let class_features = (*extract features for current class in method *)
      try
        match cname with 
        | "Object" -> object_methods;
        | "IO" -> io_methods;
        | "String" -> string_methods;
        | _ -> ( 
        (* extract features from cname *)
        let (_,_,features) = List.find(fun ((_,cname2),_,_) -> cname = cname2) ast in
        features
        )
      with Not_found -> []
    in
    (* 
    mrege attributes
    returns list of attributes, ensuring that child attributes override parents.
    *)
    let rec merge_features parent child = 
      match parent with
      | [] -> child (* parent has no attributes *)
      | (Attribute((_,aname),_,_) as attr) :: rest ->

          (* ensure that the attribute is not already declared in parent. *)
          List.iter (fun (a) -> (
            match a with 
            | Attribute((aloc,aname2),_,_) -> (
              if aname = aname2 then begin
                printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" aloc cname aname;
                exit 1
              end
            )
            | _ -> ()
          ) ) child;

          (* if the attribute exists in the child *)
          if List.exists (fun feature ->
            match feature with
            | Attribute((_, aname2), _, _) -> aname = aname2
            | Method _ -> false  
          ) child then begin
            merge_features rest child 
          end
          (* inherit the attribute*)
          else begin 
            attr :: merge_features rest child
          end
      | (Method((_,mname),formals,(_,mtype),_) as meth) :: rest ->
        (
          (* if method exists in child (user explicitly define method in child) *)
          if List.exists (fun (e) -> 
            match e with
            | Method((_,mname2),_,(_,mtype2),_) ->
             mname = mname2
            | _ -> false
            ) child then begin
              (* method exists in child *) 
              List.iter(fun (e) -> 
                match e with 
                | Method((mloc2,mname2),formals2,(_,mtype2),_) -> (
                  if mname = mname2 then begin
                    (* TYPECHECKING METHODS - inheritance related *) 
                    (* check if parameters are the same *) 
                    if List.length formals <> List.length formals2 then begin
                      printf "ERROR: %s: Type-Check: class %s redefines method %s with different number of parameters!\n" mloc2 cname mname2;
                      exit 1
                    end;
                    (* 
                    check if parameters are same type
                    COOL doenst allow out of order formal for overriding. thanks COOL! 
                    *)
                    List.iter2 (fun ((_,fname1),(_,ftype1)) ((floc2,fname2),(_,ftype2)) ->
                      if ftype1 <> ftype2 then begin
                        printf "ERROR: %s: Type-Check: class %s redefines method %s and changes type of formal!%s\n" floc2 cname mname2 fname2;
                        exit 1
                      end
                    ) formals formals2;
                    (* check if return type for overriden methods are the same *)
                    if not (mtype = mtype2) then begin
                      printf "ERROR: %s: Type-Check: class %s redefines method %s but return types arent the same (%s and %s)\n" mloc2 cname mname2 mtype mtype2;
                      exit 1
                    end;
                  end
                  )
                | _ -> ()
              ) child;
              (* after doing inheritance related checks for methods, merge the features. *)
              merge_features rest child 
            end
            else 
              (* method not overriden in child, so juts get current definition of method. *)
              meth :: merge_features rest child 
        )
  in
  let merged_features = merge_features parent_features class_features 
  in
  (* we need to take Object methods out, so that we can print it first *)
  let non_object_internal_methods = List.filter (fun e -> match e with
  | Method(_,_,_,{exp_kind=Internal(s)}) -> not (String.starts_with ~prefix:"Object." s)
  | _ -> false
  ) merged_features
  in
  let sorted_internal_methods = List.sort (fun m1 m2 ->
    match m1, m2 with 
    | Method(_,_,_,{exp_kind=Internal(meth1)}), Method(_,_,_,{exp_kind=Internal(meth2)})->
      String.compare meth1 meth2
    | _ -> 0
  ) non_object_internal_methods
  in
  let non_internal_methods = List.filter (fun e -> match e with
  | Method(_,_,_,{exp_kind=Internal(_)}) -> false
  | _ -> true
  ) merged_features 
  in
  object_methods @ sorted_internal_methods @ non_internal_methods  
  in

    (* PARENT MAP - MAP CHILDREN TO PARENT CLASSES *)
  let parent_map = build_parent_map ast in
  detect_inheritance_cycles parent_map; 
  
 (*
    loop through ast to check for class related errors
 *)
  List.iter (fun ((cloc,cname),inherits,features) ->
    (*
      Ensure that we dont redefine IO or String
    *)
      if cname = "Int" then begin
        printf "ERROR: %s: Type-Check: class Int redefined\n" cloc;
        exit 1
      end;
      if cname = "IO" then begin
        printf "ERROR: %s: Type-Check: class IO redefined\n" cloc;
        exit 1
      end;
      if cname = "String" then begin
        printf "ERROR: %s: Type-Check: class String redefined\n" cloc;
        exit 1
      end;
      if cname = "Bool" then begin
        printf "ERROR: %s: Type-Check: class Bool redefined\n" cloc;
        exit 1
      end;


    (*
      Typechecking inheritance
    *)
      match inherits with
    | None -> ()
    | Some(iloc,iname) -> (*inherited type identifier*)
      if iname = "Int" then begin
        printf "ERROR: %s: Type-Check: inheriting from forbidden class %s\n" iloc iname;
        exit 1
      end;
      if iname = "String" then begin
        printf "ERROR: %s: Type-Check: inheriting from forbidden class %s\n" iloc iname;
        exit 1
      end;
      if iname = "Bool" then begin
        printf "ERROR: %s: Type-Check: inheriting from forbidden class %s\n" iloc iname;
        exit 1
      end;


      (*check if iname is in all_classes*)
      if not (List.mem iname all_classes) then begin
        printf "ERROR: %s: Type-Check: inheriting from undefined class %s\n" iloc iname;
        exit 1
      end;
   ) ast;


  (*
      look for redeclaration of class
      since the class names are sorted, we can simply check if the next is equal to current.
  *)
  let rec check_class_redeclaration ast = match ast with
  | ((cloc,cname),_,_) :: (((cloc2,cname2),_,_) as h2) :: t ->
    if cname=cname2 then begin
      (* TODO: print identifer location *)
      printf "ERROR: %s: Type-Check: cannot redeclare class %s!\n" cloc2 cname2;
      exit 1
    end;
    check_class_redeclaration (h2 :: t) 
  | _ -> ()
  in
  check_class_redeclaration ast;


  (*
    Typechecking methods
  *)
  List.iter (fun((cloc,cname),_,current_features) ->
        let features = collect_features parent_map ast cname in
        let seen_methods = Hashtbl.create (List.length features) in
        let seen_attributes = Hashtbl.create (List.length features) in
        if cname = "Main" && List.length features = 0 then begin
            printf "ERROR: 0: Type-Check: class Main method main not found!!\n";
            exit 1
        end;
     
        (*check for main method*)
        if cname = "Main" then begin
          let has_main = List.exists(fun m -> match m with
            | Method((_,mname),_,_,_) -> mname = "main"
            | _ -> false) current_features
          in 
          if not has_main then begin
            printf "ERROR: 0: Type-Check: class Main method main not found!!\n";
            exit 1
          end 
          else begin
            List.iter (fun (m) -> (
              match m with 
              | Method ((_,mname),mformals,_,_) ->
                (
                  if mname = "main" then begin
                    if(List.length mformals) <> 0 then begin 
                      printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n";
                      exit 1
                    end
                  end
                )
              | _ -> ()
            )) current_features
          end
        end;

        List.iter(fun(feature) -> (
        match feature with
        | Method((mloc,mname),formals,_,_) -> 
          (* check formals *)
          let seen_formals = Hashtbl.create (List.length formals) in
          
          List.iter (fun ((floc,fname),(_,ftype)) -> 
            
            if Hashtbl.mem seen_formals fname then begin
              printf "ERROR: %s: Type-Check: class %s has method %s with duplicate formal %s!\n" floc cname mname fname;
              exit 1
            end;
            Hashtbl.add seen_formals fname true;

            (* check if formal has type that exists *)
            if not (List.mem ftype all_classes) then begin
              printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter of unknown type %s\n" floc cname mname ftype;
              exit 1
            end
            ) formals;

          (*
            check for duplicate method
          *)
          if Hashtbl.mem seen_methods mname then begin
            printf "ERROR: %s: Type-Check: cannot redeclare Method %s in class %s!\n" mloc mname cname;
            exit 1
          end;
          
          Hashtbl.add seen_methods mname true

        | Attribute((aloc,aname),(tloc,tname),None) -> 
          (*Check for nonexistent type for attribute*)
          if not ( List.mem tname all_classes )then begin
            printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s\n" tloc cname aname tname;
            exit 1
          end;

          (* Check for mismatched types for attributes*)
          if Hashtbl.mem seen_attributes aname then begin
            printf "ERROR: %s: Type-Check: cannot redeclare Attribute %s in class %s!\n" aloc aname cname;
            exit 1
          end;
          Hashtbl.add seen_attributes aname true
          
         | Attribute((aloc,aname),(tloc,tname),Some(exp)) -> 
          (* DRY oops*)
                (*Check for nonexistent type for attribute*)
          if not ( List.mem tname all_classes )then begin
            printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s\n" tloc cname aname tname;
            exit 1
          end;

          (* Check for mismatched types for attributes*)
          if Hashtbl.mem seen_attributes aname then begin
            printf "ERROR: %s: Type-Check: cannot redeclare Attribute %s in class %s!\n" aloc aname cname;
            exit 1
          end;
          Hashtbl.add seen_attributes aname true;
          
          (*check if expression type matches attribute type*)
            (* should we generalize to user defined classes? i dunno*)
          (*
          I think we were supposed to do this after annotating expressions
          let check_expression_type loc expression_type attribute_type = 
            if attribute_type <> expression_type && attribute_type <> "Object" then begin
              printf "ERROR: %s: Type-Check: %s does not conform to %s in initialized attribute\n" loc expression_type attribute_type;
              exit 1
            end
          in
          match exp with
          | (_, Integer(_)) -> check_expression_type tloc "Int" tname
          | (_, String(_)) -> check_expression_type tloc "String" tname
          | (_, Bool(_)) -> check_expression_type tloc "Bool" tname
          | _ -> ()
            *)
        )) features
        
    
    ) ast;
   
  
   (* DONE WITH ERROR CHECKING *)


    (* 
       BUILD THE METHOD ENVIRONMENT
       loop over ast, (each class)
       for each class, loop throuhg methods
       Update the method environment with the 
            Class name, method name, list of formal types, and return type
    *)
    (*
    let object_methods = [
        Method(("0","abort"),[],("0","Object"),{loc="0";exp_kind=Block([]); static_type= Some(Class("Object"))});
        Method(("0","type_name"),[],("0","String"),{loc="0";exp_kind=Block([]); static_type= Some(Class("String"))});
        Method(("0","copy"),[],("0","SELF_TYPE"),{loc="0";exp_kind=Block([]); static_type= Some(SELF_TYPE("Object"))});
    ]
          in
    List.iter (fun m -> 
        match m with
        | Method((_, mname), formals, (_, rtype), _) ->
            let formal_types = List.map (fun _ -> Class "Object") formals in
            Hashtbl.add m_env ("Object", mname) (formal_types, if rtype = "SELF_TYPE" then SELF_TYPE("Object") else Class rtype);
        | _ -> ()
    ) object_methods;
    *)
    List.iter (fun((_,cname),_,features) -> 
        (*loop through features for a class *)
       List.iter(fun feat -> 
           match feat with
           | Method((_,mname),formals, (_,rtype),_) -> (
               (* extract types from formals *)
               let formal_types = List.map (fun ((_,_),(_,ftype)) ->
                    Class ftype
                ) formals 
                in 
                Hashtbl.add m_env (cname, mname) (formal_types, Class rtype)
           ) 
            | _ -> ()
        ) features 
    ) ast;
   (* 
   TIME TO DO EXPRESSION TYPECHECKING 
   1. Iterate over every class
   2. Iterate over every feature. 
   3. Typecheck the expressoins in that feature.

   We implement our expression typechecking procedure
   by reading the typing rules from the CRM or class notes.

   Every line in a typing rule corresponds to a line in typechecking code.
        ( will probably be tested on this :/)
   *)
    
    let rec typecheck (o: object_environment) (exp : exp) : static_type =
        let static_type = match exp.exp_kind with
        | Integer(i) -> (Class "Int")
        | Plus(e1, e2) ->
            (
            (*
                O |- e1 : Int       [1]
                O |- e2 : Int       [2]
                --------------------------
                O |- e1 + e2 : Int  [3]
            *)
            
            (*[1]*)
            let t1 = typecheck o e1 in
            if t1 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: adding %s instead of Int\n" exp.loc (type_to_str t1);
                exit 1
            end;
            (*[2]*)
             let t2 = typecheck o e2 in
            if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: adding %s instead of Int\n" exp.loc (type_to_str t2);
                exit 1
            end;
            (*[3]*)
            (Class "Int");
            )
        | Identifier((vloc,vname)) -> 
            if Hashtbl.mem o vname then
                (
                Hashtbl.find o vname;
                )
            else begin
                printf "ERROR: %s Type-Check: undeclared variable %s\n" vloc, vname;
                exit 1
            end
        | Let(bindings,body) ->
            (*
            This is the extend O with x having type T0 part
            we will add the declare variable to object environment,
            then type chcek the expression with this "extended" environment.

            After doing so, we remove x having type T0 from typing environment.
            *)
            List.iter (fun ((vloc,vname),(tloc,tname),init) -> 
                match init with 
                | None -> (
                    (* add variable to current scope *)
                    Hashtbl.add o vname (Class tname) (* TODO: SELF_TYPE*)
                )
            ) bindings;
            (* typecheck let body with bound varible added to the object environment*)
            let body_type = typecheck o body in 

            (*
            IMPORTANT - remove object identifier(s)
            This is where mutable data can become tricky to debug :/
            *)
            List.iter( fun((_,vname),(_,_),_) ->
                Hashtbl.remove o vname
            ) bindings;

            body_type
        in        
        (* annotate the AST with the new found static type! *)
        exp.static_type <- Some(static_type);
        static_type
    in

    (*iterate over every class and typecheck all features*)
    List.iter (fun((cloc,cname),inherits,features)->
        current_class := cname; 
        List.iter (fun feat -> 
            match feat with 
            | Attribute((nameloc,name),(dtloc,declared_type),Some(init_exp)) -> (
                (* 
                   x : Int <- 5+3 
                   1. typecheck initializer expression
                   2. ensure that it conforms to the variable type. 
                *)
                (* TODO: THIS IS WRONG, CONSIDER INHERITED FEATURES IN OBJECT ENVIRONMENT*)
                let o = empty_object_environment () in

                let init_type = typecheck o init_exp in 
                if is_subtype init_type (Class declared_type) then
                    (*good*)
                    ()
                else begin
                    printf "ERROR: %s: Type-Check initializer for %s should be %s not %s\n" nameloc name declared_type (type_to_str init_type);
                    exit 1
                end

            )
            | Attribute((nameloc,name),(dtloc,declared_type),None) -> (
                if not (List.mem declared_type all_classes) then begin
                    printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s\n" nameloc cname name declared_type;
                    exit 1
                end
                )
            | Method((mloc, mname),formals, (rtloc,return_type), mbody) -> (
                let o = empty_object_environment () in

                (* add formals to object environment*)
                List.iter(fun ((floc,fname), (_,ftype)) ->
                    Hashtbl.add o fname (Class ftype)
                ) formals;
                
                let body_type = typecheck o mbody in

                (* check if body type conforms to declared return type *)
                let expected_type = if return_type = "SELF_TYPE" then 
                    SELF_TYPE !current_class 
                else
                    Class return_type 
                in

                if not (is_subtype body_type expected_type) then begin
                    printf "ERROR: %s: Type-Check: method %s return type should be %s but got %s\n" mloc mname (type_to_str expected_type) (type_to_str body_type);
                    exit 1
                end
            )
        ) features;
    ) ast;

   (* emit cl-type file*)
   let cmname = (Filename.chop_extension fname) ^ ".cl-type" in
   let fout = open_out cmname in

   let rec output_exp (e) =
        (* print expressions with line number first *)
        fprintf fout "%s\n" e.loc;

       (* now after the location, print out the TYPE ANNOTATION ( and then newline)*)
       (match e.static_type with
       | None -> ( 
           printf "line %s: forgot to do typechecking???\n" e.loc;
           exit 1
       )
       | Some(Class(c)) -> fprintf fout "%s\n" c
       (* | Some(SELF_TYPE(c)) -> fprintf fout "%s\n" !current_class; *)
       | Some(SELF_TYPE(c)) -> fprintf fout "SELF_TYPE\n";
       );
       
        

       
        match e.exp_kind with
        | Assign((id_loc, id_name), exp) ->
              
            fprintf fout "assign\n%s\n%s\n" id_loc id_name;
            output_exp exp
        | Dynamic_Dispatch(exp, (id_loc, id_name), exp_list) ->
            fprintf fout "dynamic_dispatch\n";
            output_exp exp;
            fprintf fout "%s\n%s\n" id_loc id_name;
            fprintf fout "%s\n" (string_of_int(List.length exp_list));
            List.iter output_exp exp_list
        | Static_Dispatch(exp, (t_loc, t_name), (id_loc, id_name), exp_list) ->
            fprintf fout "static_dispatch\n";
            output_exp exp;
            fprintf fout "%s\n%s\n" t_loc t_name;
            fprintf fout "%s\n%s\n" id_loc id_name;
            fprintf fout "%s\n" (string_of_int(List.length exp_list));
            List.iter output_exp exp_list
        | Self_Dispatch((id_loc, id_name), exp_list) ->
            fprintf fout "self_dispatch\n%s\n%s\n" id_loc id_name;
            fprintf fout "%s\n" (string_of_int(List.length exp_list));
            List.iter output_exp exp_list
        | If(exp1, exp2, exp3) ->
            fprintf fout "if\n";
            output_exp exp1;
            output_exp exp2;
            output_exp exp3
        | While(exp1, exp2) ->
            fprintf fout "while\n";
            output_exp exp1;
            output_exp exp2
        | Block(exp_list) ->
            fprintf fout "block\n";
            fprintf fout "%s\n" (string_of_int (List.length exp_list));
            List.iter output_exp exp_list
        | New((nloc, nval)) ->
            fprintf fout "new\n%s\n%s\n" nloc nval
        | IsVoid(e) ->
            fprintf fout "isvoid\n";
            output_exp e
        | Plus(exp1, exp2) ->
            fprintf fout "plus\n";
            output_exp exp1;
            output_exp exp2
        | Minus(exp1, exp2) ->
            fprintf fout "minus\n";
            output_exp exp1;
            output_exp exp2
        | Times(exp1, exp2) ->
            fprintf fout "times\n";
            output_exp exp1;
            output_exp exp2
        | Divide(exp1, exp2) ->
            fprintf fout "divide\n";
            output_exp exp1;
            output_exp exp2
        | LessThan(exp1, exp2) ->
            fprintf fout "lt\n";
            output_exp exp1;
            output_exp exp2
        | LessThanEqual(exp1, exp2) ->
            fprintf fout "le\n";
            output_exp exp1;
            output_exp exp2
        | Equal(exp1, exp2) ->
            fprintf fout "eq\n";
            output_exp exp1;
            output_exp exp2
        | Not(exp) ->
            fprintf fout "not\n";
            output_exp exp
        | Negate(exp) ->
            fprintf fout "negate\n";
            output_exp exp
        | Integer(ival) ->
            fprintf fout "integer\n%s\n" ival
        | String(sval) ->
            fprintf fout "string\n%s\n" sval
        | Identifier((id_loc, id_name)) ->
            fprintf fout "identifier\n%s\n%s\n" id_loc id_name
        | Bool(bval) ->
            fprintf fout "%s\n" bval
        | Let(bindings,body) ->
            fprintf fout "let\n";
            fprintf fout "%s\n" (string_of_int (List.length bindings));
            List.iter (fun ((id_loc, id_name), (type_loc, type_name), exp_opt) ->
              (
               match exp_opt with
               | Some exp ->  fprintf fout "let_binding_init\n"
               | None -> fprintf fout "let_binding_no_init\n"
              );
              fprintf fout "%s\n%s\n%s\n%s\n" id_loc id_name type_loc type_name;
              (match exp_opt with 
              | Some exp -> output_exp exp 
              | None -> ())) bindings;
              output_exp body
        | Case(_, exp, cases) ->
            fprintf fout "case\n";
            output_exp exp;
            fprintf fout "%s\n" (string_of_int (List.length cases));
            List.iter (fun ((id_loc, id_name), (type_loc, type_name), exp) ->
              fprintf fout "%s\n%s\n%s\n%s\n" id_loc id_name type_loc type_name;
              output_exp exp) cases
        | Internal(meth)->(
            (* type? *)
            fprintf fout "internal\n";
            fprintf fout "%s\n" meth;
        )
   in

   (* CLASS MAP*)
   fprintf fout "class_map\n";
   fprintf fout "%d\n" (List.length all_classes);

   List.iter(fun cname ->
        (* name of class, # attrs, each attr that is a feature*)
        fprintf fout "%s\n" cname;
        let features = collect_features parent_map ast cname in 

        (* print out number of features for the class *)
        fprintf fout "%d\n" (List.fold_left (fun acc element -> 
          match element with 
          | Attribute _->
            acc+1
          | _ -> acc+0
        ) 0 features);

        List.iter (fun attr -> match attr with
        | Attribute((_,aname),(_,atype),None) ->
          fprintf fout "no_initializer\n";
          fprintf fout "%s\n" aname;
          fprintf fout "%s\n" atype
        | Attribute((_,aname),(_,atype),(Some init))->  
          fprintf fout "initializer\n";
          fprintf fout "%s\n" aname;
          fprintf fout "%s\n" atype;
          output_exp init
        | Method(_,_,_,_)->() (* just doing class map *) 
        ) features;
   ) all_classes ;
    
   
   (* IMPLEMENTATION MAP*)
    fprintf fout "implementation_map\n";
    fprintf fout "%d\n" (List.length all_classes);
    (* dont need inherits, we have parent map to get inherited features. *)
    List.iter (fun (cname) -> ( 
        fprintf fout "%s\n" cname;

        let features = collect_features parent_map ast cname in 

        let methods = List.filter (fun f -> match f with
        | Method _ -> true
        | _ -> false
        ) features in

        fprintf fout "%d\n" (List.length methods);

        List.iter(fun method_feature ->
            match method_feature with
            | Method((_,mname), formals, (_,rtype), mbody) -> 
                    (* printf "%s\n" mname; *)
                    fprintf fout "%s\n" mname;
                    fprintf fout "%d\n" (List.length formals);

                    List.iter(fun ((_,fname),_)->
                        fprintf fout "%s\n" fname
                    ) formals;

                    (*
                        get base clsas of method (assuming we didnt override it)
                    *)
                    let rec find_original_class cname mname = 
                        match Hashtbl.find_opt parent_map cname with
                        | None -> cname (* no parent*)
                        (* look at parent for cname *)
                        | Some parent -> 
                              (* metohd is defiined in parent *)
                              (* go up the inheritance tree see if its farthur up, or if its just definde in parent*)
                              (* for example: A->B->C->Object!*)
                              if List.exists (fun f->match f with 
                                | Method((_,mn),_,_,_) -> mn = mname
                                | _ -> false
                              ) (collect_features parent_map ast parent) then
                                find_original_class parent mname
                              else
                                cname

                    in
                    let original_class = find_original_class cname mname in
                    fprintf fout "%s\n" original_class;

                    output_exp mbody
            | _ -> failwith "whattt"
        ) methods
    )) all_classes; 
    Hashtbl.iter (fun key value -> printf "Class %s has parent %s\n" key value) parent_map; 
   close_out fout;
end ;;
main () ;;
