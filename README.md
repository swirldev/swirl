# swirlfancy/exp.FSM

Experimental (i.e., hastily written, lightly tested) implementation of a layered, extensible architecture capable of running swirl course material.

To run the demo, source R/testMod.R, and call function hi(arg), where arg is one of "default", "alt", or "testMod". The last option will run through the testMod4Daphne test data.

File R/miniMulti.R which uses a callback to multitask between the R prompt and a swirl tutorial, using a persistent environment to store data between invocations. Most of its logic is solely for this purpose. For additional functionality it invokes an S3 method, "resume." Methods resume.default and resume.alt are meant to illustrate the multitasking logic itself. Method resume.testMod is meant to illustrate how an extensible architecture for swirl might be organized.

Method resume.testMod implements a finite state (or virtual) machine which could be generalized but is specialized here for testMod4Daphne. It runs a fixed "program" consisting of three "instructions" which in turn present information, capture a user's response, and test and retry if necessary. The three instructions are themselves S3 methods which depend on the class of the active row of the course module. The instruction set is thus extensible. It can be found in R/testModInstr.R. 

In the case of questions, user responses must be tested for correctness. Tests appropriate to a given question are given by keyphrases in the module's AnswerTests column. Keyphrases must, somehow, be associated with functions which actually implement the tests. In testMod4Daphne's keyphrases, the test itself was identified by the substring prior to "=" in a keyphrase, and its parameters by the substring following. For extensibility, the substring prior to "=" was made the name of an S3 class and its extraction was hard-coded into the testResponse instruction. Interpretation of test parameters was relegated to the test method for the indicated class.

This architecture is thus extensible in a number of ways.

* New resume methods may be written.
* For resume methods of the "testMod" variety, the instruction set may be expanded, and the virtual machine's program may be more complex, or dynamically changed.
* The class structure of module rows need not be hardcoded but made extensible by, e.g., adding a class column to modules.
* New tests may be added, provided keyphrases and associated test methods conform to a given convention.
