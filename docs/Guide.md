# Guide to Po#

This is a small guide to introduce the syntax of the language.
It is very WIP, but I do want to provide basic info here so anyone
would be able to write Po# code.

### Basics

```scala
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

```scala
def main(): int {
    print(5); //basic print command, supports all numeric values
    print(5 + 5); //basic addition
    print((5 + 5) + (9 * 6)); //currently, all subexpressions must be eclosed in ()
}
```

### Variables

```scala
def main(): int {
    val a = 5; //defines and sets value
    print(a); //access variable
    a = (a + 3); // assing to variable
    print(a);
    val b: int = 3; //variables can also have types
}
```

#### Currently supported Types:
* int
* string
* char
* bool
* float
* array[*type*]
* void (return nothing for functions)

### Floats

```scala
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

```scala
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

#### Booleans

```scala
    def main(): int {
        //booleans variables are also possible, similar to c++ and such
        val a: bool = true;
        val b: bool = (3 == 5); //conditions can be outside ifs and assigned to bools
        if(a || b){
          print(b); //fancy printing for booleans
        };
    }
```

### Loops

```scala
def main(): int {
    val a = 5;
    while (a != 0) { //same as c style
        a = (a - 1);
    };
    //for loops are also supported
    for (val i = 0; i < 5; i+=1;) {
      print(i);
    };
    print(a);
}
```

### Arrays

```scala
def main(): int {
    val a = array[int][8]; // define an array using array keyword, type and size
              // arrays are initialized with 0
    a[0] = 5; // rest of the syntax is the same as c
    print(a[0]);
    
    val b = array(1, 2, 3); //arrays can also be initialized with default values
                            // their size is equal to the default values list

    print(b.size); // array size can also be accessed with the size keyword

    //prints all values of b
    for(val i = 0; i < b.size; i+=1;) {
        print(b[i]);
    };
}
```

### Strings
```scala
def main(): int {
    val str = "hello"; //declare strings like the usual languages
    print(str); //print directly
    val ch = str[0]; //access characters
    print(ch);
    //the underlying implementation of strings uses char array, there is no string type
    val str2: array[char] = array('a', 'b');
    print(str2);
}
```

### Functions

```scala
// we have been using just the main function, but any function can be defined
//below is a recursive fibonacci implementation
def main(): int {
  val a = 9;
  print(fib(a));
}
//functions have intput type annotations, together with the return type
def fib(n: int): int {
  if(n <= 1) {return n;};
  return (fib((n-1)) + fib((n-2)));
}
```

### Objects

```scala
object Coord {
  x: int; //declare variables with name and type
  y: int; //does not currently support default values, use the constructor
  //main constructor of the function, use the same name as object
  //non-static functions in an object have self as a first argument, similar to python
  def Coord(self: Coord, x: int, y: int) {
    self.x = x; //reference elements of an object used . operator
    self.y = y;
  }
  //regular function of a object, variables can have types of custom objects
  def compare(self: Coord, other: Coord): bool {
    return ((self.x == other.x) && (self.y == other.y));
  }
}
def main(): int {
  //instantiate an object using the new keyword and calling the constructor
  val a = new Coord(0,1);
  val b = new Coord(1,5);
  print(a.compare(b)); //call object function with . op
}
```