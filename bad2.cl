
class A inherits B {};
class B inherits A {}; -- 

class Main {
    main() : Object {
        (new A)
    };
};