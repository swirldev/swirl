# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("You got it!",
                 "Nice work!",
                 "Keep up the great work!",
                 "You are doing so well!",
                 "All that hard work is paying off!",
                 "You nailed it! Good job!",
                 "You're the best!",
                 "You are amazing!",
                 "Keep working like that and you'll get there!",
                 "Perseverance, that's the answer.",
                 "Great job!",
                 "You are quite good my friend!",
                 "Your dedication is inspiring!",
                 "You got it right!",
                 "That's correct!",
                 "You are really on a roll!",
                 "Excellent job!",
                 "All that practice is paying off!",
                 "Excellent work!",
                 "That's a job well done!",
                 "That's the answer I was looking for.")
  } else {
    phrases <- "Correct!"
  }
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("Almost! Try again.",
                 "You almost had it, but not quite. Try again.",
                 "Give it another try.",
                 "Not quite! Try again.",
                 "Not exactly. Give it another go.",
                 "That's not exactly what I'm looking for. Try again.",
                 "Nice try, but that's not exactly what I was hoping for. Try again.",
                 "Keep trying!",
                 "That's not the answer I was looking for, but try again.",
                 "Not quite right, but keep trying.",
                 "You're close...I can feel it! Try it again.",
                 "One more time. You can do it!",
                 "Not quite, but you're learning! Try again.",
                 "Try again. Getting it right on the first try is boring anyway!")
  } else {
    phrases <- "Incorrect. Please try again."
  }
  sample(phrases, 1)
}
