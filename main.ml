(* 
current score:
17/40
*)
(*
Basically, you’ll look at classes, methods and attibutes (but not method bodies).

*)
(*
Some code stolen from westley weimer
https://www.youtube.com/watch?v=Wa9zMygcv_M&ab_channel=WestleyWeimer
*)
(*Sementic analyzer checkpoint*)


(*
WE ARE DECLARING RECURSIVE DATA STRUCTURES FOR ENCODING COOL AST

When declaring types, the * creates a tuple
| indicates different forms for variant types.
ie a feature can be a Method or an Attribute

The and keyword allows you to reference other types in a type definitoin
  ie using other types to define a type

Declaring types allows us to define the valid types that methods can return
ie a method will return a feature, which can either be a Method or Attribute.

They are also self documenting.
*)

open Printf

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

and exp = loc * exp_kind
and exp_kind = 
  | Integer of string (*really an int*)

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

  (*
  many mutually recursive procedures to read in cl-ast file
  we use and to define mutual functions - ie they can reference eachother
  otherwise we could use let ... in .... to define functions beforehand.
  *)
  let rec read_cool_program() = 
    read_list read_cool_class

  (*
  reads a locatoin and name
  then returns the tuple
  *)
  and read_id() =  (*IDENTIFER*)
    let loc = read() in
    let name = read() in
    (loc,name)
  
  and read_cool_class () = (*CLASS*)
    let cname = read_id () in 
    let inherits = match read() with
    | "no_inherits" -> None
    | "inherits" ->
      let super = read_id () in
      Some(super)
    | x -> failwith ("cannot happen:" ^ x)
    in
    let features = read_list read_feature in
    (cname, inherits, features)
  and read_feature () = 
    match read() with
    | "attribute_no_init" ->
      let fname = read_id() in
      let ftype = read_id() in
      Attribute(fname, ftype, None) (*call attribute constructor*)
    | "attribute_init" ->
      let fname = read_id() in
      let ftype = read_id() in
      let finit = read_exp() in
      (*debug_print_id fname;
      debug_print_id ftype;*)
      Attribute(fname, ftype, (Some finit))
    | "method" ->
      let mname = read_id() in
      let formals = read_list read_formal in
      let mtype = read_id() in
      let mbody = read_exp() in 
      Method(mname,formals,mtype,mbody)
    | x -> failwith ("cannot happen:" ^ x)
  and read_formal () =
    let fname = read_id () in
    let ftype = read_id() in
    (fname, ftype)
  and read_exp () =
    let eloc = read() in
    let ekind  = match read() with
    | "integer" ->
      let ival=read() in 
      Integer(ival)
    | x -> (*do all of other expressions*)
      failwith ("expression kind unhandled: " ^ x)
    in 
    (eloc, ekind)
  in

  let ast = read_cool_program() in
  close_in fin;
  (*
  printf "CL-AST de-serialized, %d classes\n" (List.length ast);
  printf "printing...\n";
  List.iter (fun ((_, cname),_,_)-> printf "%s\n" cname) ast; 
  *)
  (*Check for class related errors*)

  let base_classes = ["Int" ; "String" ; "Bool" ; "IO" ; "Object"] in 
  (*
  the ast is a cool program, which is a list of cool classes.
  extract cname from list of cool_classes.
  *)
  let user_classes = List.map(fun ((_,cname),_,_) -> cname) ast in
  let all_classes = base_classes @ user_classes in
  let all_classes = List.sort compare all_classes in
  (*THEME IN PA2 - make internal data structure to hold helper 
information so you can do the checks more easily.*)


  (*extract attributes classes in the ast*)
  (* NEED TO CONSIDER ATTRIBUTES FROM BASE CLASSES 
  hard part is considering inheritance
  1. construct a mapping from child to parent
    use toposort here to find the right order of traversal (or detect inheritance cycles)
  2. recursively walk up mapping until get object
  3. add in all of attributes that we find
  4. while there look for attribute override problems.

  we use toposort to find the right order to gather up attributes
    need to do this to correctly consider shadowing.
  *)

  (* mapping from child to parent*)
  (*
  build_parent_map, detect_inheritance_cycles and collect_attribute
  generated by chatgpt (prompt was the above comment)
  *)
  let build_parent_map ast = 
    let parent_map = Hashtbl.create (List.length ast) in
    List.iter(fun ((_,cname), inherits, _) -> 
      match inherits with
      | None -> ()
      | Some(_,pname)-> Hashtbl.add parent_map cname pname
      )ast;
    parent_map 
  in
  let detect_inheritance_cycles parent_map =
    (*
    visited works differently than path,
    it ensures that we dont visit the same node.
    
    path ensures that we dont visit the same node in the inheritance tree.
    *)
    let visited = Hashtbl.create (Hashtbl.length parent_map) in
    let rec dfs cname path =
      (* already encountered this class! (from path variable) *)
      if List.mem cname path then begin
        (*crm- inheritance cycle always line 0.*)
        printf "ERROR: 0: Type-Check: cyclic inheritance involving class %s and %s\n" cname (Hashtbl.find parent_map cname);
        exit 1
      end;
      if not (Hashtbl.mem visited cname) then begin 
        Hashtbl.add visited cname true;
        (* see if there are any parent nodes to traverse to. *)
        match Hashtbl.find_opt parent_map cname with
        | Some parent -> dfs parent (cname :: path)
        | None -> ()
      end
    in
    Hashtbl.iter(fun cname _ -> dfs cname []) parent_map 
  in
  (* 
  given a class name, collect all of its features(including from inherited classes 
  does this by merging the current class and parent class features.
    does this recursively. (keeps going up the inheritance tree)
  *)
  let rec collect_features parent_map ast cname =
    (* get parent class name if there is one *)
    let parent_features = match Hashtbl.find_opt parent_map cname with
    | Some parent -> collect_features parent_map ast parent
    | None -> [] (* base class, no parent*)
    in  
    let class_features = 
      try
        (* extract features from cname *)
        let _,_,features = List.find(fun ((_,cname2),_,_) -> cname = cname2) ast in
        features
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
          (* if method exists in child (user explicitly define method in child) *)
          if List.exists (fun (Method((_,mname2),_,(_,mtype2),_)) -> mname = mname2) child then begin
            
            List.iter(fun (Method((mloc2,mname2),formals2,(_,mtype2),_)) -> 
              if mname = mname2 then begin 
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
              ) child;
            merge_features rest child 
          end
          else 
            meth :: merge_features rest child 
    in
    merge_features parent_features class_features 
  in 


  let parent_map = build_parent_map ast in
  detect_inheritance_cycles parent_map; 
  
  (*
  look for inheritance from int
  look for inheritance from undeclared class
  *)
  List.iter (fun ((cloc,cname),inherits,features) ->
    match inherits with
    | None -> ()
    | Some(iloc,iname) -> (*inherited type identifier*)
      if iname = "Int" then begin
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
   features shenanigans
  *)
  List.iter (fun((cloc,cname),_,_) ->
    let features = collect_features parent_map ast cname in
    let seen_methods = Hashtbl.create (List.length features) in
    let seen_attributes = Hashtbl.create (List.length features) in
    List.iter(fun(feature) -> (
    match feature with
    | Method((mloc,mname),formals,_,_) -> 
      (*check for main method*)
      if cname = "Main" then begin
        let has_main = List.exists(fun m -> match m with
          | Method((_,mname),_,_,_) -> mname = "main"
          | _ -> false) features
        in 
        if not has_main then begin
          printf "ERROR: 0: Type-Check: class Main method main not found!!\n";
          exit 1
        end;
      end;
      
      (* 
      check for redefining formals 
      *)
      
      let seen_formals = Hashtbl.create (List.length formals) in
      
      List.iter (fun ((floc,fname),_) -> 
        if Hashtbl.mem seen_formals fname then begin
          printf "ERROR: %s: Type-Check: class %s has method %s with duplicate formal %s!\n" floc cname mname fname;
          exit 1
        end;
        Hashtbl.add seen_formals fname true
        ) formals;
        


      (*

      *)
      if Hashtbl.mem seen_methods mname then begin
        printf "ERROR: %s: Type-Check: cannot redeclare Method %s in class %s!\n" mloc mname cname;
        exit 1
      end;
      
      Hashtbl.add seen_methods mname true
    | Attribute((aloc,aname),_,_) -> 
      if Hashtbl.mem seen_attributes aname then begin
        printf "ERROR: %s: Type-Check: cannot redeclare Attribute %s in class %s!\n" aloc aname cname;
        exit 1
      end;
      Hashtbl.add seen_attributes aname true
    )) features
  ) ast;
   
  
   (* DONE WITH ERROR CHECKING *)

   (* emit cl-type file*)
   (*for pa2c, just do class map*)
   let cmname = (Filename.chop_extension fname) ^ ".cl-type" in
   let fout = open_out cmname in

   let rec output_exp (eloc, ekind) =
    fprintf fout "%s\n" eloc;
    match ekind with
    | Integer(ival) -> fprintf fout "integer\n%s\n" ival
   in

   fprintf fout "class_map\n";
   fprintf fout "%d\n" (List.length all_classes);

   List.iter(fun cname ->
    (* name of class, # attrs, each attr that is a feature*)
    fprintf fout "%s\n" cname;
    let features = collect_features parent_map ast cname in 

    (* print out number of features for the class *)
    fprintf fout "%d\n" (List.fold_left (fun acc e -> 
      match e with 
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
        
   close_out fout;
end ;;
main () ;;