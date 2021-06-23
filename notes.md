# Notes

## Variance

### class Foo[+A] // A covariant class

- It goes down on type hierarchy
- Indicates that can be of type A or any subtype of A
- The hierarchy of types is going to be equivalent for Foo
- Cat <: Animal ==> Foo[Cat] <: Foo[Animal]

### class Bar[-A] // A contravariant class

- It goes up on type hierarchy
- Indicates that can be of type A or any supertype of A
- The hierarchy of types is going to be contra-equivalent for Bar
- Cat <: Animal ==> Bar[Animal] <: Bar[Cat]

### class Baz[A]  // An invariant class

- Of type A and type A only
- No type hierarchy is inherited


## Strictness and laziness

- ```def func[A](x: => A): A = x + x``` - x is non-strict argument meaning it gets evaluated every time
it is called in the body of the function and not earlier.
- ```def func[A](x: => A): A = {lazy val y = x; x + x}``` - use ```lazy``` keyword to created things evaluated
in a lazy manner - only once and reused if necessary.

## Syntax

- Functions accepting a single argument may be called with braces instead
of parantheses. ```Try {age.toInt} <==> Try(age.toInt)```
  
- For-comprehensions -- syntactic sugar for a series of maps and flatMaps. yield is converted
to a map while the preceeding bindings are designated to flatMap.
  
