# Ulv Lang 🐺

Some examples

```ulv
# a comment

55 # an integer

3.14159 # a float

"some words" # a string

2 3 + # math, that equals 5

23 :age # a variable

(1 2 3) # a quote of 3 values

^age # push the value of `age` to the stack, in this case `23`

# how about some built-ins!

force # pop a quote and execute it

stack # duplicates the stack, onto the stack

print # pops and prints the top value

+ - * / # your basic math operators

(1 2 3) (1 +) map # outputs (2 3 4)

(1 2 3) 0 (+) fold
```