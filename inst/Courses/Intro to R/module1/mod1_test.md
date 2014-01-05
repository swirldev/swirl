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

R simply prints the result of 12 by default. However, R is a programming language and often the reason we use a programming language as opposed to a calculator is to automate some process or avoid unneccessary repetition.

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

You can combine vectors to make a new vector. Create a new vector that contains z, 55, then z again in that order. Don't assign this vector to a new variable, so that we can just see the result immediately.


```r
c(z, 55, z)
```

```
## [1]  1.10  9.00  3.14 55.00  1.10  9.00  3.14
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
2. a single number

---

Print print the contents of `mySqrt`.


```r
mySqrt
```

```
## [1] 0.3162 2.8284 1.4629
```


