# swirlfancy/exp.Modularity

A piece of experimental (i.e., hastily written, lightly tested) code illustrating how four areas of functionality might be modularized. The four areas are control, presentation of content, tracking user progress, and testing responses.

Control consists mainly of a callback function which returns the user to an R prompt, or not, depending on feedback from content.

Content conists of "states" which are instances of S3 classes supporting nextState() and a doStage() methods.

States aggregate pre-written tests and apply them to user responses. Tests associated with a particular question are determined by the question's author.

Control also updates a history of user progress. Expressions entered at the command prompt by the user in response to questions are evaluated in a clean environment, thus mirroring what the user has done in the global enviroment. A two-deep history the clean enviroment is saved.