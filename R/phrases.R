# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c(s()%N%"You got it!",
                 s()%N%"Nice work!",
                 s()%N%"Keep up the great work!",
                 s()%N%"You are doing so well!",
                 s()%N%"All that hard work is paying off!",
                 s()%N%"You nailed it! Good job!",
                 s()%N%"You're the best!",
                 s()%N%"You are amazing!",
                 s()%N%"Keep working like that and you'll get there!",
                 s()%N%"Perseverance, that's the answer.",
                 s()%N%"Great job!",
                 s()%N%"You are quite good my friend!",
                 s()%N%"Your dedication is inspiring!",
                 s()%N%"You got it right!",
                 s()%N%"That's correct!",
                 s()%N%"You are really on a roll!",
                 s()%N%"Excellent job!",
                 s()%N%"All that practice is paying off!",
                 s()%N%"Excellent work!",
                 s()%N%"That's a job well done!",
                 s()%N%"That's the answer I was looking for.")
  } else {
    phrases <- s()%N%"Correct!"
  }
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c(s()%N%"Almost! Try again.",
                 s()%N%"You almost had it, but not quite. Try again.",
                 s()%N%"Give it another try.",
                 s()%N%"Not quite! Try again.",
                 s()%N%"Not exactly. Give it another go.",
                 s()%N%"That's not exactly what I'm looking for. Try again.",
                 s()%N%"Nice try, but that's not exactly what I was hoping for. Try again.",
                 s()%N%"Keep trying!",
                 s()%N%"That's not the answer I was looking for, but try again.",
                 s()%N%"Not quite right, but keep trying.",
                 s()%N%"You're close...I can feel it! Try it again.",
                 s()%N%"One more time. You can do it!",
                 s()%N%"Not quite, but you're learning! Try again.",
                 s()%N%"Try again. Getting it right on the first try is boring anyway!")
  } else {
    phrases <- s()%N%"Incorrect. Please try again."
  }
  sample(phrases, 1)
}
