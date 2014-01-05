Intro to R - Module 1
========================================================

In this module, we will explore some basic building blocks of the R programming language.

---

In it's simplest form, R can be used as an interactive calculator. Type `5 + 7` and press Enter.


```r
5 + 7
```

```
## [1] 12
```


--- 

R simply prints the result of 12 by default. However, R is a programming language and often the reason we use a programming language as opposed to a calculator is to automate some process or avoid unnecessary repetition.

---

In this case, we may want to use our result from above in a second calculation. Instead of retyping `5 + 7` every time we need it, we can just create a new variable that stores the result.

---

The way you assign a value to a variable in R is by using the assignment operator, which is just a "less than" symbol followed by a "minus" sign. It looks like this: <-

---

Think of the assignment operator as an arrow. You are assigning the value on the right side of the arrow to the variable name on the left side of the arrow.

---

To assign the result of `5 + 7` to a new variable called `x`, you type `x <- 5 + 7`. This can be read as "x gets 5 plus 7." Give it a try now.


```r
x <- 5 + 7
```


---

You'll notice that R did not print the result of 13 this time. When you use the assignment operator, R assumes that you don't want to see the result immediately, but rather that you intend to use the result for something else later on.

---

To view the contents of the variable `x`, just type `x` and press Enter. Try it now.


```r
x
```

```
## [1] 12
```

---

Now, store the result of `x - 3` in a new variable called `y`.


```r
y <- x - 3
```


---

What is the value of `y`? Type `y` to find out.


```r
y
```

```
## [1] 9
```


---

Now, let's create a small collection of numbers called a vector. Any object that contains data is called a data structure and numeric vectors are the simplest type of data structure in R. In fact, even a single number is considered a vector of length one.

---

The easiest way to create a vector is with the `c()` function, which stands for "concatenate" or "combine". To create a vector containing the numbers 1.1, 9, and 3.14, type `c(1.1, 9, 3.14)`. Try it now and store the result in a variable called `z`.


```r
z <- c(1.1, 9, 3.14)
```


---

Type `z` to view it's contents. Notice that there are no commas separating the values in the output.


```r
z
```

```
## [1] 1.10 9.00 3.14
```


---

You can combine vectors to make a new vector. Create a new vector that contains z, 555, then z again in that order. Don't assign this vector to a new variable, so that we can just see the result immediately.


```r
c(z, 555, z)
```

```
## [1]   1.10   9.00   3.14 555.00   1.10   9.00   3.14
```


---

Numeric vectors can be used in arithmetic expressions. Type the following to see what happens: `z * 2 + 100`.


```r
z * 2 + 100
```

```
## [1] 102.2 118.0 106.3
```


---

First, R multiplied each of the three elements in `z` by 2. Then it added 100 to each element to get the result you see above.

---

Other common arithmetic operators are `+`, `-`, `/`, and `^` (where `x^2` means "x squared"). To take the square root, use the `sqrt()` function and to take the absolute value, use the `abs()` function.

---

Take the square root of `z - 1` and assign it to a new variable called `mySqrt`.


```r
mySqrt <- sqrt(z - 1)
```


---

Before we view the contents of the `mySqrt` variable, what do you think it contains?

1. a vector of length 3
2. a single number (i.e a vector of length 1)
3. a vector of length 0 (i.e. an empty vector)

---

Print the contents of `mySqrt`.


```r
mySqrt
```

```
## [1] 0.3162 2.8284 1.4629
```


---

As you may have guessed, R first subtracted 1 from each element of z, then took the square root of each element. This leaves you with a vector of the same length as the original vector `z`.

---

Now, create a new variable called `myDiv` that gets the value of `z` divided by `mySqrt`.


```r
myDiv <- z/mySqrt
```


---

Which statement do you think is true?

1. The first element of `myDiv` is equal to the first element of `z` divided by the first element of `mySqrt`, and so on...
2. `myDiv` is a single number (i.e a vector of length 1)
3. `myDiv` is undefined

---

Go ahead and print the contents of `myDiv`.


```r
myDiv
```

```
## [1] 3.479 3.182 2.146
```


---

When given two vectors of the same length, R simply performs the specified arithmetic operation (`+`, `-`, `*`, etc.) element-by-element. If the vectors are of different lengths, R "recycles" the shorter vector until it is the same length as the longer vector.

---

When we did `z * 2 + 100` in our earlier example, `z` was a vector of length 3, but technically `2` and `100` are each vectors of length 1.

---

Behind the scenes, R is "recycling" the `2` to make a vector of 2s and the `100` to make a vector of 100s. In other words, when you ask R to compute `z * 2 + 100`, what it really computes is this: `z * c(2, 2, 2) + c(100, 100, 100)`.

---

To see another example of how this vector "recycling" works, try adding `c(1, 2, 3, 4)` and `c(0, 10)`. Don't worry about saving the result in a new variable.


```r
c(1, 2, 3, 4) + c(0, 10)
```

```
## [1]  1 12  3 14
```


---

If the length of the shorter vector does not divide evenly into the length of the longer vector, R will still apply the "recycling" method, but will throw a warning to let you know something fishy might be going on.

---

Try `c(1, 2, 3, 4) + c(0, 10, 100)` for an example.


```r
c(1, 2, 3, 4) + c(0, 10, 100)
```

```
## Warning: longer object length is not a multiple of shorter object length
```

```
## [1]   1  12 103   4
```

