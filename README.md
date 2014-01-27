#bfc

bfc is a toy compiler written in Haskell that compiles to Brainfuck. The source language is a simple ad-hoc language. It performs some optimizations to reduce the size of the output. It is a work in progress, so far it supports:

* variables referencing strings, bools, or unsigned integers of arbitrary size
* operators: =, +, -, *, +=, -=, ++, --, >, >=, <, <=, ==, !=, &&, ||
* if/else/while control flow
* functions
* arrays with dynamic indices. So far only elements with size of 1 byte are supported.

## Example: project euler problem #1

Solution to the [first projecteuler problem](http://projecteuler.net/problem=1):
Input: see [samples/euler1](https://raw.github.com/benma/bfc/master/samples/euler1)
`./bfc < samples/euler1` produces  [samples/euler1.bf](https://raw.github.com/benma/bfc/master/samples/euler1.bf)

When executed:
```
$ bf samples/euler1.bf
please input upper
1000
hang on
............[...]
233168
```

## Other small examples:

Short representation of bytes:
```
$ echo "100" | ./bfc
>--[<->+++++]<--
```

Output some dots: 
```
n = 300
while(n) {
  print "."
  n--
}
```

Result:
```
+>>-[<->+++++]<-------<[>>>+>>+<<<<<-]>>>>>[<<<<<+>>>>>-]<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]+<[<<<[-]+>>>[-]>-<]>[<<<[<[-]+>[-]]>>>-]<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]+<[<<<[-]+>>>[-]>-<]>[<<[<<[-]+>>[-]]>>-]<<<<[>[-]>[-]-[<->+++++]<-----.[-]+>[-]>[-]<[-]<<<[>>>+>+<<<<-][-]>>>>[<<<<+>>>>-]<[<->[-]]<<<->>[<<<->>>[-]][-]>[-]>[-]<<<<<[>>>+>>+<<<<<-][-]>>>>>[<<<<<+>>>>>-][-]<<<<[>>>+>+<<<<-][-]>>>>[<<<<+>>>>-]<<<[-]>>>[-]>[-]<[-]<<<[>>>+>+<<<<-][-]>>>>[<<<<+>>>>-][-]+<[<<<[-]+>>>[-]>-<]>[<<<[<[-]+>[-]]>>>-]<[-]>[-]<[-]<<<[>>>+>+<<<<-][-]>>>>[<<<<+>>>>-][-]+<[<<<[-]+>>>[-]>-<]>[<<[<<[-]+>>[-]]>>-]<<<<]<<
```

When executed, 300 dots are printed:
```
$ bf result.bf
............................................................................................................................................................................................................................................................................................................
```
