mod <- read.csv("data/Courses/Open_Intro/module1/mod1.csv",as.is=TRUE)
nr <- nrow(mod)
mod[,"Class"] <- NA
cmds <- list(text = "text_question", 
             multiple = "mult_question",
             exact="exact_question", 
             range="range_question",
             command="cmd_question")
for (n in 1:nr)
{
 if (mod[n,"Output.Type"]=="question")
   class_type <- cmds[[ mod[n,"Answer.Type"] ]]
 else 
    class_type <- mod[n,"Output.Type"]
 mod[n,"Class"] <- class_type
}
colheads <- c("OutputType","Output","AnswerType","AnswerChoices","AnswerTests","Hint",
                 "Figure","FigureType","VideoLink","Tag","Notes","Class")

write.table(mod,file="data/Courses/Open_Intro/module1/mod1_new.csv",col.names  =colheads,sep = ",")