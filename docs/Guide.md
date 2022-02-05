# Guide to Po#

This is a small guide to introduce the syntax of the language.
It is very WIP, but I do want to provide basic info here so anyone
would be able to write Po# code.

### Basics

```
//currently, a main function must be defined, together with its return type
def main(): int { 
    print(5); // each statement must be ended with ";", does not need to be on new line
    {
        print(3);
    };
    /*
        all c like comments are supported
     */
}
```

### Printing and arithmetic

```
def main(): int {
    print(5); //basic print command, supports all numeric values
    print(5 + 5); //basic addition
    print((5 + 5) + (9 * 6)); //currently, all subexpressions must be eclosed in ()
}
```

### Variables

```
def main(): int {
    val a = 5; //defines and sets value
    print(a); //access variable
    a = (a + 3); // assing to variable
    print(a);
    val b: int = 3; //variables can also have types
    // currently only integers are supported
}
```

### Floats

```
def main(): int {
   val a = 2.3; // any float declaration uses a dot, even 2.0, otherwise it is an int
   print(a);
   val b: int = a.toInt; // floats can be converted to integers
   print(b);
   print(2.3 + 4.5); //arithmetic
   print(2 + 4.5); //ints are automatically converted to floats
   // be aware, this returns a float
   // something like val a: float = (2 + 2); does not work
   // use toFloat to convert
}
```

### Conditions

```
def main(): int {
    val a = 5;
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
    
    val b = 3;
    // conditions are chained with && and ||, conditions must be enclosed in ()
    // different types of logical ops cant be in the same enclosure
    if(((a < 5) || (b==5)) && (a > 1)) { 
        print(b);
    };
}
```

### While

```
def main(): int {
    val a = 5;
    while (a != 0) { //same as c style
        a = (a - 1);
    };
    print(a);
}
```

### Arrays

```
def main(): int {
    val a = array[8]; // define an array using array keyword and size
              // arrays are initialized with 0
    a[0] = 5; // rest of the syntax is the same as c
    print(a[0]);
    val b = array(1, 2, 3); //arrays can also be initialized with default values
                            // their size is equal to the default values list

    print(b.size); // array size can also be accessed with the size keyword

    val i = 0; //prints all values of b
    while(i < b.size) {
        print(b[i]);
        i = (i+1);
    };
}
```

### Functions

```
// we have been using just the main function, but any function can be defined
//below is a recursive fibonacci implementation
def main(): int {
  val a = 9;
  print(fib(a));
  return 0;
}
//functions have intput type annotations, together with the return type
def fib(n: int): int {
  if(n <= 1) {return n;};
  return (fib((n-1)) + fib((n-2)));
}
```
