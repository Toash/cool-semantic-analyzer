
class A {
    method() : Int { 1 };
};

-- wrong method override type
class B inherits A {
    method() : String { "asdf" }; 
};

class Main {
    main() : Object {
        new Object
    };
};