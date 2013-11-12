# swirlfancy/demo/cbk_tests

Demo integrating two experimental features: answers are tested via a list of keywords referring to standard tests, and interaction of control with content through a standard interface.

### Warts:

1. Variables defined by the user are tracked via expressions entered by the user. As implemented, it works for testMod, but needs improvement to deal with possibilities like `cars$weight <- 1000*cars$weight`.

2. Much of the code currently in function testByPhrase (states.R) belongs in distinct test functions, as in ansTests.R.

3. As implemented, tests seem too tightly coupled to details of states, their classes, structures, and so on. Some sort of standardized interfacing seems called for, whether by S3 classes, or simply conventions governing test inputs and outputs.

### Suggestions:

1. Tracking variables defined by the user via expressions entered by the user seems more robust to me than alternatives. A full implementation of the feature might entail:
  * two additional methods for states, getVars and putVars,
  * a solid function to extract assigned variable names from expressions.

2. Lines 199 on, in states.R, should be moved to ansTests.R as a test function, perhaps replacing testVal. The same applies to the multiple choice test, lines 100-107 in states.R.

3. REMARK: Swirl 1.0 stores user progress and allows users to pick up where they left off. This requires swirl to execute all assignments prior to the point of resumption. To support this, states probably need an execute method (which would do nothing except in the case of assignments.)