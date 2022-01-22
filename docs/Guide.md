#Guide to Po#
This is a small guide to introduce the syntax of the language.
It is very WIP, but I do want to provide basic info here so anyone
would be able to write Po# code.

###Basics
```
//currently, the top level must be enclosed in brackects, bracket nesting is allowed
{ 
    print(5); // each statement must be ended with ";", does not need to be on new line
    {
        print(3);
    };
    /*
        all c like comments are supported
     */
}
```

###Printing and arithmetic
```
{
    print(5); //basic print command, supports all numeric values
    print(5 + 5); //basic addition
    print((5 + 5) + (9 * 6)); //currently, all subexpressions must be eclosed in ()
}
```

###Variables
```
{
    def a = 5; //defines and sets value
    print(a); //access variable
    a = (a + 3); // assing to variable
    print(a);
}
```

###Conditions
```
{
    def a = 5;
    //basic if, very similar to c style, but no support for single line statements
    if(a == 5) {
        print(0);
    } else {
        print(1);
    };
    
    //else if is not explicitly supported, so it needs to be nested with a block
    if(a == 5) {
        print(0);
    } else {
        if(a == 3) {print(1);};
    };
    
    def b = 3;
    // conditions are chained with && and ||, conditions must be enclosed in ()
    // different types of logical ops cant be in the same enclosure
    if(((a < 5) || (b==5)) && (a > 1)) { 
        print(b);
    };
}
```

###While
```
{
    def a = 5;
    while (a != 0) { //same as c styke
        a = (a - 1);
    };
    print(a);
}
```

###Arrays
```
{
    def a[8]; // similar to variable definition, but in [] the array size is specified
              // arrays are always initialized with 0
              // currently only support numbers
    a[0] = 5; // rest of the syntax is the same as c
    print(a[0]);
}
```