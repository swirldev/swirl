# swirlfancy/demo/cbk_tests

Demo integrating two experimental features: answers are tested via a list of keywords referring to standard tests, and interaction of control with content through a standard interface.

### Warts:

1. Variables defined by the user are tracked via expressions entered by the user. As implemented, it works for testMod, but needs improvement to deal with possibilities like `cars$weight <- 1000*cars$weight`.

2. Much of the code currently in function testByPhrase (states.R) belongs in a separate function as in ansTests.R.

3. As implemented, tests seem too tightly coupled to details of states, their classes, structures, and so on. Some sort of standardized interfacing seems called for, whether by S3 classes, or simply conventions governing test inputs and outputs.