# Return random praise.
praise <- function() {
  phrases <- c("You got it!",
               "Nice work!",
               "Keep up the great work!",
               "You are doing so well!",
               "You nailed it! Good job!",
               "You're the best!",
               "You are amazing!",
               "Great job!",
               "You are quite good my friend!",
               "You got it right!",
               "That's correct!",
               "You are really on a roll!",
               "Excellent job!",
               "That's a job well done!")
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
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
               "You're close...I can feel it! Try it again.")
  sample(phrases, 1)
}