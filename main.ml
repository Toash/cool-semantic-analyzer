(*
Some code stolen from westley weimer
https://www.youtube.com/watch?v=Wa9zMygcv_M&ab_channel=WestleyWeimer
*)
(*Sementic analyzer checkpoint*)

(*make some types to correspond what we find in cool program*)
(*A cool program is a list of cool classes*)



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

  (*debug printing*)
  let debug_print_id (loc, string) = 
    printf("DEBUG PRINTING ID: (%s %s )\n") loc string 
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
      Attribute(fname, ftype, None)
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

  (*
  loop through cool program for cool classes.
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

   (* DONE WITH ERROR CHECKING *)

   (* emit cl-type file*)
   (*for pa2c, just do class map*)
   let cmname = (Filename.chop_extension fname) ^ ".cl-type" in
   let fout = open_out cmname in
   fprintf fout "class_map\n";
   fprintf fout "%d\n" (List.length all_classes);

   close_out fout;
end ;;
main () ;;