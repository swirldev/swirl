# swirlfancy/exp.FSM

A piece of experimental (i.e., hastily written, lightly tested) code implementing a small version of swirl as a Finite State Machine.

It might be useful way to view swirl architecture as a loop of three basic functions:

1. presentation & response
2. evaluation & logging
3. branching

In this view, a unit of content is a little script which runs on a "machine" represented by the above loop whose "instruction set" is loosely illustrated by the following examples. Interpreting a unit of content would be a matter of translating from a course authorship language, say a flavor of markdown, to a language understood by this machine.

Examples of presentation & response would be:
* select.list(<choices>), choice
* readline("Would you like to watch a movie?"), y or n
* "Find the mean of the variable you defined earlier", prompt> expr
* browseURL(<movie>), prompt> nxt()
* "Correct! You are doing so well", response is NA
* readline("..."), "play" or anything else

Examples of evaluation and logging might be
* Given expr, val, ok, has the user defined a new variable? If so store its name, newVar : name, for later reference.
* Given expr, val, ok, has the user found the mean of a variable defined earlier?
* Given expr, val, ok, has the user done something completely inscrutable? If so, log the response, but erase its effect.
* Has the multiple choice question been answered correctly?
* Has the user answered yes to a movie?
* Response is NA, just continue

Examples of branching
* Response was correct, branch to next unit of instruction
* Response was incorrect, go to hint stage of current unit
* User wants to watch a movie, go to browseURL/nxt()
* User wants to suspend instruction, go to "Type nxt() to continue."


