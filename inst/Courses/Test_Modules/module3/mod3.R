modpath <- "inst/Courses/Test_Modules/module3/mod3_new.csv"

savemod <- function(){
  write.csv(module, modpath, row.names=FALSE)
}

newrow <- function(Class=NA, Output=NA, CorrectAnswer=NA, AnswerChoices=NA, AnswerTests=NA, 
                               Hint=NA, Figure=NA, FigureType=NA, VideoLink=NA){
  temp <- data.frame(Class=Class, Output=Output, CorrectAnswer=CorrectAnswer, 
                     AnswerChoices=AnswerChoices, AnswerTests=AnswerTests, Hint=Hint,
                     Figure=Figure, FigureType=FigureType, VideoLink=VideoLink)
}

module <- newrow(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)

# just text, no question
module <- rbind(module, newrow(
  Class="text", 
  Output="This is test module for function omnitest. Omnitest can test for a correct expression, a correct value, or both. In the case of values it is limited to testing for character or numerical vectors of length 1."
  ))

# multiple choice question
module <- rbind(module, newrow(
  Class="mult_question",
  Output="Testing for a correct e$val given a multiple choice question. Which movie involve Pierce Brosnan in a singing role?",
  AnswerChoices="Mamma Mia!;Guys and Dolls;Once is Not Enough",
  CorrectAnswer="Mamma Mia!",
  AnswerTests="omnitest(correctVal='Mamma Mia!')"
  ))

# User must give an exact, numerical answer
module <- rbind(module, newrow(
  Class="exact_question",
  Output="Testing for an exact numerical answer entered at the command line. How many Stooges are there?",
  CorrectAnswer=3,
  AnswerTests="omnitest(correctVal=3)",
  Hint="This one should be obvious! Nyuk, Nyuk, Nyuk!"
  ))

# requires user to enter a command
module <- rbind(module, newrow(
  Class="cmd_question",
  Output="Testing that a user has entered a particular command. Enter myVar <- c(3, 5, 7)",
  CorrectAnswer="myVar <- c(3, 5, 7)",
  AnswerTests="omnitest(correctExpr='myVar <- c(3, 5, 7)')",
  Hint="Enter myVar <- c(3, 5, 7)"
  ))

# requires user to enter a command
module <- rbind(module, newrow(
  Class="cmd_question",
  Output="Testing that a user has entered a command which computes a specific value but perhaps in a different manner than anticipated. The correct expression is 3^2, but enter 3*3.",
  CorrectAnswer="3^2",
  AnswerTests="omnitest(correctExpr='3^2', correctVal=9)",
  Hint="Enter 3^2"
  ))

# requires user to enter a command
module <- rbind(module, newrow(
  Class="cmd_question",
  Output="Testing that a user has entered a command which computes a specific value in a particular way. The correct expression is 3^2. Entering 3*3 should fail.",
  CorrectAnswer="3^2",
  AnswerTests="omnitest(correctExpr='3^2', correctVal=9, strict=TRUE)",
  Hint="Enter 3^2"
))

# just text, no question
module <- rbind(module, newrow(
  Class="text", 
  Output="That completes the omnitest test. Thank you."
  ))