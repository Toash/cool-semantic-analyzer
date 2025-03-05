(* hairy  . . .*)

class Foo inherits Blah{

     doh() : Int { (let i : Int <- h in { h <- h + 2; i; } ) };

};

class Blah inherits Bazz{

     doh() : Int { (let i: Int <- h in { h <- h + 1; i; } ) };
};

class Bazz inherits IO {

     h : Int <- 1;


     doh() : Int { (let i: Int <- h in { h <- h + 1; i; } ) };
};

(* scary . . . *)
class Main inherits IO {

  main(): String { { out_string("\n") ; "do nothing" ; } };

};







