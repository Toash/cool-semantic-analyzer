(*
Some code stolen from westley weimer
https://www.youtube.com/watch?v=Wa9zMygcv_M&ab_channel=WestleyWeimer
*)

open Printf


(* static type of cool expression*)
type static_type = 
| Class of string (*Int, or Object etc*)
| SELF_TYPE of string (*implement later*)

let type_to_str t = match t with
| Class(x) -> x
| SELF_TYPE(c) -> "SELF_TYPE"

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
    current_class := cname;
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
        current_class := cname;
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
      current_class := cname;
      List.iter(fun feat -> 
        match feat with
        | Method((_,mname),formals, (_,rtype),_) -> (
            let formal_types = List.map (fun ((_,_),(_,ftype)) ->
              if ftype = "SELF_TYPE" then SELF_TYPE cname else Class ftype
            ) formals in
            let return_type = if rtype = "SELF_TYPE" then SELF_TYPE cname else Class rtype in
            Hashtbl.add m_env (cname, mname) (formal_types, return_type)
        ) 
        | _ -> ()
      ) features 
    ) ast;
    (* return true if t1 is a subtype of t2 *)
let rec is_subtype t1 t2 = 
  (match t1, t2 with
  | Class(x), Class(y) when x = y -> true
  | Class(x), Class("Object") -> true
  | Class(x), Class(y) -> 
    (* recursively go up inheritance tree to see if x is subtype of y *)
    (try 
      let parent = Hashtbl.find parent_map x in
      is_subtype (Class parent) t2
    with Not_found -> false)
  | SELF_TYPE(c1), SELF_TYPE(c2) when c1 = c2 -> true
  | SELF_TYPE(c1), Class(c2) -> is_subtype (Class c1) (Class c2)
  | Class(c1), SELF_TYPE(c2) -> is_subtype (Class c1) (Class c2)
  | _,_ -> false
  )
  in

  (* can be eithre then or else, return lub *)
  let rec least_upper_bound t1 t2 =
    if is_subtype t1 t2 then t2
    else if is_subtype t2 t1 then t1
    else match t1, t2 with
      (* not subtype, go up inheritance tree to check again*)
      | Class(c1), Class(c2) ->
          let parent1 = Hashtbl.find parent_map c1 in
          let parent2 = Hashtbl.find parent_map c2 in
          least_upper_bound (Class parent1) (Class parent2)
      | SELF_TYPE(c1), SELF_TYPE(c2) when c1 = c2 -> SELF_TYPE(c1)
      | SELF_TYPE(c1), Class(c2) -> least_upper_bound (Class c1) (Class c2)
      | Class(c1), SELF_TYPE(c2) -> least_upper_bound (Class c1) (Class c2)
      | _ -> failwith "unexpected types in least_upper_bound"
  in
  (* 
  we are trying to also consider parent methods from a class, 
  *)
  let rec find_method_in_class cname mname =
    let features = collect_features parent_map ast cname in
    try
      List.find (function
        | Method((_, mname2), _, _, _) when mname = mname2 -> true
        | _ -> false
      ) features
    with Not_found ->
      match Hashtbl.find_opt parent_map cname with
      | Some parent -> find_method_in_class parent mname
      | None -> raise Not_found
    in
    let get_class_name t = match t with
      | Class(c) -> c
      | _ -> failwith "unexpected type"
    in
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
        | Identifier((vloc,vname)) -> 
            if Hashtbl.mem o vname then
                (
                Hashtbl.find o vname;
                )
            else begin
                printf "ERROR: %s Type-Check: undeclared variable %s\n" vloc, vname;
                exit 1
            end
        | Assign((var_loc, var_name), e1) ->
          if Hashtbl.mem o var_name then
            let t0 = Hashtbl.find o var_name in
            let t = typecheck o e1 in
            if is_subtype t t0 then
                t0
            else begin
                printf "ERROR: %s: Type-Check: type %s does not conform to declared type %s in assignment\n" exp.loc (type_to_str t) (type_to_str t0);
                exit 1
            end
          else begin
            printf "ERROR: %s: Type-Check: undeclared variable %s\n" var_loc var_name;
            exit 1
          end
        | Bool(_) -> Class "Bool"
        | Integer(i) -> (Class "Int")
        | String(s) -> (Class "String")
        | New((_, tname)) -> 
        if tname = "SELF_TYPE" then
            SELF_TYPE !current_class
        else
            Class tname
            | Dynamic_Dispatch(e0, (f_loc, f_name), args) ->
              (*receiever *)
              let t0 = typecheck o e0 in
              (* typecheck args on method called on reciever*)
              let arg_types = List.map (typecheck o) args in
              let t00 = match t0 with
                | SELF_TYPE _ -> Class !current_class
                | _ -> t0
              in
              let (formal_types, return_type) = 
                let get_class_name t = match t with
                  | Class(c) -> c
                  | _ -> failwith "unexpected type"
                in
                try 
                  match find_method_in_class (get_class_name t00) f_name with
                  | Method(_, formals, (_, return_type), _) -> 
                      (List.map (fun (_, t) -> t) formals, return_type)
                  | _ -> failwith "Expected a method"
                with Not_found ->
                  printf "ERROR: %s: Type-Check: method %s not found in class %s\n" f_loc f_name (type_to_str t00);
                  exit 1
              in
              if List.length formal_types <> List.length arg_types then begin
                printf "ERROR: %s: Type-Check: method %s called with wrong number of arguments\n" f_loc f_name;
                exit 1
              end;
              List.iter2 (fun t arg_t ->
                if not (is_subtype arg_t t) then begin
                  printf "ERROR: %s: Type-Check: argument type %s does not conform to formal type %s\n" f_loc (type_to_str arg_t) (type_to_str t);
                  exit 1
                end
              ) (List.map (fun (_, t) -> if t = "SELF_TYPE" then SELF_TYPE !current_class else Class t) formal_types) arg_types;
              let t_return = if return_type = "SELF_TYPE" then SELF_TYPE !current_class else Class return_type in
              t_return
          | Static_Dispatch(e0, (t_loc, t_name), (f_loc, f_name), args) ->
              let t0 = typecheck o e0 in
              let t = Class t_name in
              if not (is_subtype t0 t) then begin
                  printf "ERROR: %s: Type-Check: type %s does not conform to declared type %s in static dispatch\n" exp.loc (type_to_str t0) (type_to_str t);
                  exit 1
              end;
              let arg_types = List.map (typecheck o) args in
              let (formal_types, return_type) = 
                try 
                  match find_method_in_class t_name f_name with
                  | Method(_, formals, (_, return_type), _) -> 
                      (List.map (fun ((_, t), _) -> 
                        if t = "SELF_TYPE" then 
                          SELF_TYPE !current_class 
                        else 
                          Class t) formals, 
                          if return_type = "SELF_TYPE" then 
                            SELF_TYPE !current_class 
                        else Class return_type)
                  | _ -> failwith "Expected a method"
                with Not_found -> 
                  printf "ERROR: %s: Type-Check: method %s not found in class %s\n" f_loc f_name t_name;
                  exit 1
              in
              if List.length formal_types <> List.length arg_types then begin
                printf "ERROR: %s: Type-Check: method %s called with wrong number of arguments\n" f_loc f_name;
                exit 1
              end;
              List.iter2 (fun t arg_t ->
                if not (is_subtype arg_t t) then begin
                  printf "ERROR: %s: Type-Check: argument type %s does not conform to formal type %s\n" f_loc (type_to_str arg_t) (type_to_str t);
                  exit 1
                end
              ) formal_types arg_types;
              let t_return = match return_type with
                | SELF_TYPE _ -> t0
                | _ -> return_type
              in
              t_return
          | Self_Dispatch((m_loc, m_name), exp_list) ->
              let t0 = SELF_TYPE !current_class in
              let arg_types = List.map (typecheck o) exp_list in
              let (formal_types, return_type) = 
                try 
                  match find_method_in_class !current_class m_name with
                  | Method(_, formals, (_, return_type), _) -> (List.map (fun (_, t) -> t) formals, return_type)
                  | _ -> failwith "Expected a method"
                with Not_found ->
                  printf "ERROR: %s: Type-Check: method %s not found in class %s\n" m_loc m_name !current_class;
                  exit 1
              in
              if List.length formal_types <> List.length arg_types then begin
                printf "ERROR: %s: Type-Check: method %s called with wrong number of arguments!\n" m_loc m_name;
                exit 1
              end;
              List.iter2 (fun t arg_t ->
                if not (is_subtype arg_t t) then begin
                  printf "ERROR: %s: Type-Check: argument type %s does not conform to formal type %s!\n" m_loc (type_to_str arg_t) (type_to_str t);
                  exit 1
                end
              ) (List.map (fun (_, t) -> if t = "SELF_TYPE" then SELF_TYPE !current_class else Class t) formal_types) arg_types;
              let t_return = if return_type = "SELF_TYPE" then SELF_TYPE !current_class else Class return_type in
              t_return
        | If(e1, e2, e3) ->
            let t1 = typecheck o e1 in
              if t1 <> Class "Bool" then begin
                  printf "ERROR: %s: Type-Check: condition in if expression must be Bool, but got %s\n" e1.loc (type_to_str t1);
                  exit 1
              end;
              let t2 = typecheck o e2 in
              let t3 = typecheck o e3 in
              
              least_upper_bound t2 t3
        | Block(exp_list) ->
          (* 
          the type of a sequence is the last expression in it 
          so we are juts going to recursively call this until we get down to one 
            expression, and return that as its type.
          *)
          let rec typecheck_sequence exps =
            match exps with
            | [] -> failwith "empty block"
            | [e] -> typecheck o e
            | e :: rest -> 
                let _ = typecheck o e in
                typecheck_sequence rest
          in
          typecheck_sequence exp_list

        | Let(bindings, body) ->
          List.iter (fun ((vloc, vname), (tloc, tname), init) -> 
              (*T0'*)
              let declared_type = if tname = "SELF_TYPE" then SELF_TYPE !current_class else Class tname in
              match init with 
              | None -> 
                  Hashtbl.add o vname declared_type
              | Some exp -> 
                  (*T1, we try to prove that exp is of a type T1*)
                  let init_type = typecheck o exp in 
                  (* T1 <= T0' *)
                  if is_subtype init_type declared_type then
                      Hashtbl.add o vname declared_type
                  else begin
                      printf "ERROR: %s: Type-Check: initializer for %s should be %s not %s\n" vloc vname tname (type_to_str init_type);
                      exit 1
                  end
          ) bindings;
          (* we are now passing the extended objcet environment
                with this extended object environet, check if the 
                  body resolves to a valid type. *)
          let body_type = typecheck o body in
          (* important to remove mappings from object environment *)
          List.iter (fun ((_, vname), (_, _), _) -> Hashtbl.remove o vname) bindings;
          body_type

        | Case(_, e0, cases) ->
          let t0 = typecheck o e0 in
          let case_types = List.map (fun ((_, x), (_, t), e) ->
              (* type check each case body with its corresponding variable in extended object map *)
              let case_type = if t = "SELF_TYPE" then SELF_TYPE !current_class else Class t in
              Hashtbl.add o x case_type;
              let t0i = typecheck o e in
              Hashtbl.remove o x;
              t0i
          ) cases in
          let rec least_upper_bound_list types =
            match types with
            | [] -> failwith "empty case!"
            | [t] -> t
            | t1 :: t2 :: rest -> least_upper_bound_list ((least_upper_bound t1 t2) :: rest)
          in
          least_upper_bound_list case_types
        | While(e1, e2) ->
          let t1 = typecheck o e1 in
          if t1 <> Class "Bool" then begin
              printf "ERROR: %s: Type-Check: condition in while expression must be Bool, but got %s\n" e1.loc (type_to_str t1);
              exit 1
          end;
          let _ = typecheck o e2 in
          Class "Object"
        | IsVoid(e1)->
          let _ = typecheck o e1 in
          Class("Bool")
        | Not(e)->
          let t = typecheck o e in
          if t <> Class("Bool") then begin
            printf "ERROR: %s: Type-Check: not expression must be Bool, but got %s\n" e.loc (type_to_str t);
            exit 1
          end;
          Class("Bool")
        | LessThan(e1,e2) | LessThanEqual(e1,e2) ->
            let t1 = typecheck o e1 in
            if t1 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: comparison on %s instead of Int\n" e1.loc (type_to_str t1);
                exit 1
            end;
            (*[2]*)
            let t2 = typecheck o e2 in
            if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: comparison on %s instead of Int\n" e2.loc (type_to_str t2);
                exit 1
            end;
            Class("Bool");
        | Negate(e1) ->
          let t1 = typecheck o e1 in
          if t1 <> (Class "Int") then begin
            printf "ERROR: %s: Type-Check: negate on %s instead of Int\n" e1.loc (type_to_str t1);
            exit 1
          end 
          else
            (Class "Int")
        | Times(e1,e2) | Plus(e1, e2) | Minus(e1,e2) | Divide(e1,e2) ->
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
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Int\n" exp.loc (type_to_str t1);
                exit 1
            end;
            (*[2]*)
             let t2 = typecheck o e2 in
            if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Int\n" exp.loc (type_to_str t2);
                exit 1
            end;
            (*[3]*)
            (Class "Int");
            )
        | Equal(e1, e2) ->
          let t1 = typecheck o e1 in
          let t2 = typecheck o e2 in
          let is_primitive_type t = match t with
              | Class("Int") | Class("String") | Class("Bool") -> true
              | _ -> false
          in
          if (is_primitive_type t1 || is_primitive_type t2) && t1 <> t2 then begin
              printf "ERROR: %s: Type-Check: cannot compare %s with %s\n" exp.loc (type_to_str t1) (type_to_str t2);
              exit 1
          end;
          Class("Bool")
        | Internal(e) ->
        match e with 
          | "Object.abort" -> Class("Object")
          | "Object.type_name" -> Class("String")
          | "Object.copy" -> SELF_TYPE("Object")
          | "IO.out_string" -> SELF_TYPE("IO")
          | "IO.out_int" -> SELF_TYPE("IO")
          | "IO.in_string" -> Class("String")
          | "IO.in_int" -> Class("Int")
          | "String.length" -> Class("Int")
          | "String.concat" -> Class("String")
          | "String.substr" -> Class("String")
          | _ -> failwith "wat internal method is that?\n"
        in 

        (* annotate the AST with the new found static type! *)
        exp.static_type <- Some(static_type);
        static_type
    in
    (* Loops over attributes for Class, and populates object environment.*)
    let build_object_environment_for_class parent_map ast cname =
      let o = empty_object_environment () in
      let features = collect_features parent_map ast cname in
      List.iter (fun feature ->
        match feature with
        | Attribute((_, aname), (_, tname), _) ->
          let attr_type = if tname = "SELF_TYPE" then SELF_TYPE cname else Class tname in
          Hashtbl.add o aname attr_type
        | _ -> ()
      ) features;
      o
    in
    (*iterate over every class and typecheck all features*)
    List.iter (fun((cloc,cname),inherits,features)->
    current_class := cname; 
    let o = build_object_environment_for_class parent_map ast cname in
    List.iter (fun feat -> 
        match feat with 
        | Attribute((nameloc,name),(dtloc,declared_type),Some(init_exp)) -> (
            let expected_type = if declared_type = "SELF_TYPE" then SELF_TYPE !current_class else Class declared_type in
            Hashtbl.add o "self" (SELF_TYPE !current_class);
            let init_type = typecheck o init_exp in
            Hashtbl.remove o "self";
            if not (is_subtype init_type expected_type) then begin
                printf "ERROR: %s: Type-Check: initializer for %s should be %s not %s\n" nameloc name (type_to_str expected_type) (type_to_str init_type);
                exit 1
            end
        )
        | Attribute((nameloc,name),(dtloc,declared_type),None) -> (
            let expected_type = if declared_type = "SELF_TYPE" then SELF_TYPE !current_class else Class declared_type in
            if not (List.mem declared_type all_classes) then begin
                printf "ERROR: %s: Type-Check: class %s has attribute %s with unknown type %s\n" dtloc cname name declared_type;
                exit 1
            end
        )
        (* the high level idea is that  we need the method formals and class attributes 
      in scope when typechecking the  method body. *)
        | Method((mloc, mname),formals, (rtloc,return_type), mbody) -> (
            let o = build_object_environment_for_class parent_map ast cname in
            Hashtbl.add o "self" (SELF_TYPE !current_class);
            List.iter (fun ((floc,fname), (_,ftype)) ->
                Hashtbl.add o fname (if ftype = "SELF_TYPE" then SELF_TYPE !current_class else Class ftype)
            ) formals;
            let body_type = typecheck o mbody in
              
            let expected_type = if return_type = "SELF_TYPE" then SELF_TYPE(!current_class) else Class return_type in
            if not (is_subtype body_type expected_type) then begin
                printf "ERROR: %s: Type-Check: method %s return type should be %s but got %s\n" mloc mname (type_to_str expected_type) (type_to_str body_type);
                exit 1
            end
        )
        | _ -> ()
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

        current_class := cname;
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
        
        current_class := cname;
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
    (* Hashtbl.iter (fun key value -> printf "Class %s has parent %s\n" key value) parent_map;  *)
    (* PARENT MAP *)
    fprintf fout "parent_map\n";
    fprintf fout "%s\n" (string_of_int (Hashtbl.length parent_map));
    (* 
    print hashtbl where keys are in alphabetical order
      gotta extract keys into a list, sort the list then iter through list and get the values from those keys.
    *)
    let keys = Hashtbl.fold (fun key _ acc -> key :: acc) parent_map [] in
    let sorted_keys = List.sort String.compare keys in
    List.iter (fun key -> fprintf fout "%s\n%s\n" key (Hashtbl.find parent_map key)) sorted_keys;

    (*
    ANNOTATED AST
    *)

    (* let find_features_for_class cname = List.iter(fun ((_,cname2),_,features) -> (
      if cname = cname2 then
        features
    )) 
    in *)
    fprintf fout "%s\n" (string_of_int(List.length ast));
    List.iter (fun ((cloc,cname),inherits,features)-> (
      (* printf "%s\n" cname; *)
      fprintf fout "%s\n" cloc;
      fprintf fout "%s\n" cname;

      (* i dont know why, but when you remove the parantehesis aroud 
      this match expressoin, the output changes *)
      (match inherits with
      | Some(ploc,pname) -> (
        fprintf fout "inherits\n";
        fprintf fout "%s\n" ploc;
        fprintf fout "%s\n" pname;
      )
      | None -> (
        fprintf fout "no_inherits\n";
      );
      );
      (* printf "%s features for class %s\n" (string_of_int(List.length features)) cname; *)

      fprintf fout "%s\n" (string_of_int(List.length features));
      List.iter(fun feature ->(
        match feature with 
        | Attribute((aloc,aname),(tloc,tname),None) ->(
          fprintf fout "attribute_no_init\n";
          fprintf fout "%s\n" aloc;
          fprintf fout "%s\n" aname;
          fprintf fout "%s\n" tloc;
          fprintf fout "%s\n" tname;
        )
        | Attribute((aloc,aname),(tloc,tname),(Some finit)) ->
          fprintf fout "attribute_init\n";
          fprintf fout "%s\n" aloc;
          fprintf fout "%s\n" aname;
          fprintf fout "%s\n" tloc;
          fprintf fout "%s\n" tname;
          output_exp finit
        | Method((mloc,mname), formals, (tloc,tname), mbody) ->
          fprintf fout "method\n";
          fprintf fout "%s\n" mloc;
          fprintf fout "%s\n" mname;
          fprintf fout "%d\n" (List.length formals);
          List.iter (fun ((floc,fname), (ftloc,ftname)) ->
            fprintf fout "%s\n" floc;
            fprintf fout "%s\n" fname;
            fprintf fout "%s\n" ftloc;
            fprintf fout "%s\n" ftname;
          ) formals;
          fprintf fout "%s\n" tloc;
          fprintf fout "%s\n" tname;
          output_exp mbody
      )) features;
                        
    )) ast;

  (* 
            test if ast is the same here
  List.iter (fun ((_,cname),_,features) -> (
  printf "%s features for class %s\n" (string_of_int(List.length features)) cname;
  )
    ) ast; *)
   close_out fout;
end ;;
main () ;;
