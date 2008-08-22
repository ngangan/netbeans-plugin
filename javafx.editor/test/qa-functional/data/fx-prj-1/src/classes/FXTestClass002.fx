package classes;

import java.lang.System;

class Foo {
    var a : String ;
    var b : String = bind a;
    var c : String = "Test";
    
    function bleep() : String {
        
        return "roll";
    }

    function mud() : Integer {
        System.out.println(this.a);
        return 0;
    }
}

