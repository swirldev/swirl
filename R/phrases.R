# Return random praise.
praise <- function() {
  phrases <- c("You got it!",
               "Nice work!",
               "Keep up the great work!",
               "All that hark work is paying off!",
               "You nailed it! Good job!",
               "Keep working like that and you'll get there!",
               "Perseverance, that's the answer.",
               "Great job!",
               "You're dedication is inspiring!",
               "You got it right!",
               "That's correct!",
               "All that practice is paying off!",
               "Excellent work!",
               "That's a job well done!",
               "That's the answer I was looking for.")
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
               "You're close...I can feel it! Try it again.",
               "One more time, you can do it.",
               "Not quite, but you're learning! Try again.",
               "Try again, getting it right on the first try is boring anyway.")
  sample(phrases, 1)
}
