fixdata <- function(filename){
#filename="data/Courses/Open_Intro/module1/mod1.csv"
mod <- read.csv(filename,as.is=TRUE)
nr <- nrow(mod)
mod[,"Class"] <- NA
mod[,"AnswerTests"] <- NA
cmds <- list(text = "text_question", 
             text_order = "text_order_question",
             text_many = "text_many_question",
             multiple = "mult_question",
             exact="exact_question", 
             range="range_question",
             command="cmd_question")
for (n in 1:nr)
{
#determine class type based on answertype and outputtype
 if (mod[n,"Output.Type"]=="question")
   class_type <- cmds[[ mod[n,"Answer.Type"] ]]
 else 
    class_type <- mod[n,"Output.Type"]
 mod[n,"Class"] <- class_type
 
 #set tests
 test_string <- NA
 if (identical(class_type,"mult_question")){
  test_string <- paste(text="word=",mod[n,"Correct.Answer"])
 }
 if (identical(class_type,"text_question")){
   correct_ans <- mod[n,"Correct.Answer"]
   new_correct <- paste("word=",correct_ans,sep="",collapse=";")
   test_string <- new_correct
 }
 if (identical(class_type,"text_order_question")){
   correct_ans <- mod[n,"Correct.Answer"]
#   correct_ans <- str_trim(unlist(strsplit(mod[n,"Correct.Answer"],",")))
   test_string <- paste("word_order=",correct_ans,sep="")
 }
 if (identical(class_type,"text_many_question")){
   correct_ans <- mod[n,"Correct.Answer"]
   test_string <- paste("word_many=",correct_ans,sep="")
 }
 if (identical(class_type,"cmd_question")){
   correct_ans <- mod[n,"Correct.Answer"]
   test_string <- paste("swirl1cmd=",correct_ans,sep="")
 }
 if (identical(class_type,"exact_question")){
  # correct_ans <- str_trim(unlist(strsplit(mod[n,"Correct.Answer"],",")))
   correct_ans <- mod[n,"Correct.Answer"]
   new_correct <- paste("exact=",correct_ans,sep="",collapse=";")
   test_string <- new_correct
 } 
 if (identical(class_type,"range_question")){
   correct_ans <- mod[n,"Correct.Answer"]
  # correct_ans <- str_trim(unlist(strsplit(mod[n,"Correct.Answer"],";")))
   new_correct <- paste("range=",correct_ans,sep="",collapse=";")
   test_string <- new_correct
 } 
 mod[n,"AnswerTests"] <- test_string
}

#file column headings to match daphne test mod
colheads <- c("OutputType","Output",
              "AnswerType","AnswerChoices",
              "CorrectAnswer","Hint",
              "Figure","FigureType","VideoLink",
              "Tag","Notes",
              "Class","AnswerTests")

len <- str_length(filename)
newfilename <-str_c(substr(filename,1,str_length(filename)-4),"_new.csv",sep = "")
write.table(mod,newfilename,col.names  =colheads,sep = ",")
}