# swirlfancy, initial prototype for Swirl 2.0.

The initial prototype supports course and module selection, and autosaves user progress. All users are "swirladmin" at this writing.

To run, load the package and type swirl() at the R prompt.

File swirl.R contains the core function, swirl(), and method, resume(). Functionality can be added by expanding the "instruction set" in instructionSet.R, and expanding the tests for user responses in answerTests.R.

The default resume() method runs a fixed "program" consisting of three "instructions" which present information, capture a user's response, and test and retry if necessary. Each instruction is an S3 method which depends on the "Class" column of the active row of the course module. Thus, a presently unimplemented way to capture user input, e.g., using a pop-up function editor as suggested by Peter, can be accommodated by defining a new class of question and writing a method for it.

Major features, such as special support for course authors, could be added by overriding the default resume() method. Given the class of the new method as an argument, the swirl() function will run it rather than the default.

In the case of questions, user responses must be tested for correctness. Tests appropriate to a given question are given by keyphrases in the module's AnswerTests column. Keyphrases must, somehow, be associated with functions which actually implement the tests. In this prototype, the test itself is identified by the substring prior to "=" in a keyphrase, and the test's parameters are given by the substring following. For extensibility, the substring prior to "=" was made the name of an S3 class and its extraction was hard-coded into the testResponse() instruction. Parsing the test parameters is the responsibility of the test method for the indicated class.

Since R is single-threaded, and since certain questions require response from the R prompt, a multitasking mechanism is required. Hadley Wickham  suggested R's callback capability and provided an example, which we adapted for the purpose. The callback function is defined and registered within the body of the function swirl(), hence the callback retains a reference to the normally transient environment which is created when swirl() is invoked. The chain of references from R's callback manager to that environment thus keeps the latter from being garbage collected. Hence, it may be used to store information between invocations of the callback.

The callback does very little except to capture commands entered at the R prompt and to pass them along to the resume() method, along with a reference to the persistent environment.
