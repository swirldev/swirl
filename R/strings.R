get_string <- function(filename, index){
  eval(parse(text = paste0(get_swirl_option("language"), "_strings")))[[filename]][[index]]
}

english_strings <- list(
    actions = list(
      "Resuming lesson...",
      "I just reset the script to its original state. If it doesn't refresh immediately, you may need to click on it.",
      "Sourcing your script...",
      "Entering play mode. Experiment as you please, then type nxt() when you are ready to resume the lesson.",
      "Returning to the main menu...",
      "This feature is not implemented yet for Swirl."
    ),
    answerTests = list(
      "That's not the expression I expected but it works.",
      "The given range", "is not numeric."
    ),
    answerTests2 = list(
      "That's not the expression I expected but it works.",
      "I've executed the correct expression in case the result is needed in an upcoming question.",
      "does not exist."
    ),
    install_course = list(
      "Course installed successfully!",
      "Course installation failed."
    ),
    instructionSet = list(
      "Type nxt() to continue"
    ),
    menu = list(
      "To begin, you must install a course. I can install a",
      "course for you from the internet, or I can send you to a web page",
      "(https://github.com/swirldev/swirl_courses)",
      "which will provide course options and directions for",
      "installing courses yourself.",
      # 6-10
      "(If you are not connected to the internet, type 0 to exit.)",
      "Don't install anything for me. I'll do it myself.",
      "Sorry, but I'm unable to fetch ",
      "right now. Are you sure you have an internet connection?",
      "If so, would you like to try again or visit",
      # 11-15
      "the course repository for instructions on how to",
      "install a course manually? Type 0 to exit.",
      "Try again!",
      "Send me to the course repository for manual installation.",
      "OK. I'm opening the swirl course respository in your browser.",
      # 16-20
      "Welcome to swirl!",
      "Please sign in. If you've been here before, use the same name as you did then. If you are new, call yourself something unique.",
      "What shall I call you? ",
      "Please don't use any quotes or other punctuation in your name.",
      "Thanks, ",
      # 21-25
      ". Let's cover a few quick housekeeping items before we begin our first lesson. First of all, you should know that when you see '...', that means you should press Enter when you are done reading and ready to continue.",
      "...  <-- That's your cue to press Enter to continue",
      "Also, when you see 'ANSWER:', the R prompt (>), or when you are asked to select from a list, that means it's your turn to enter a response, then press Enter to continue.",
      "Continue.",
      "Proceed.",
      # 26-30
      "Let's get going!",
      "Select 1, 2, or 3 and press Enter",
      "You can exit swirl and return to the R prompt (>) at any time by pressing the Esc key. If you are already at the prompt, type bye() to exit and save your progress. When you exit properly, you'll see a short message letting you know you've done so.",
      "Let's get started!",
      "No. Let me start something new.",
      # 31-35
      "Would you like to continue with one of these lessons?",
      "Take me to the swirl course repository!",
      "Please choose a course, or type 0 to exit swirl.",
      "Please choose a lesson, or type 0 to return to course menu."
    ),
    options = list(
      "Please provide arguments so that appropriate options can be set.",
      "Options set successfully!",
      "Option name '",
      "' not found.",
      "Option '",
      "' deleted successfully!"
    ),
    phrases = list(
      c("You got it!",
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
        "That's the answer I was looking for."),
      "Correct!",
      c("Almost! Try again.",
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
        "Try again. Getting it right on the first try is boring anyway!"),
      "Incorrect. Please try again."
    ),
    post = list(
      "Lesson complete! Exiting swirl now..."
    ),
    progress = list(
      "Please enter a valid username.",
      "Deleted progress for user: ",
      "Could not find account for user: "
    ),
    swirl = list(
      "Leaving swirl now. Type swirl() to resume.",
      "When you are at the R prompt (>):",
      "-- Typing skip() allows you to skip the current question.",
      "-- Typing play() lets you experiment with R on your own; swirl will ignore what you do...",
      "-- UNTIL you type nxt() which will regain swirl's attention.",
      # 6-10
      "-- Typing bye() causes swirl to exit. Your progress will be saved.",
      "-- Typing main() returns you to swirl's main menu.",
      "-- Typing info() displays these options again.",
      "I just sourced the following script, which demonstrates one possible solution.",
      "Press Enter when you are ready to continue...",
      # 11-15
      "Entering the following correct answer for you...",
      "You've reached the end of this lesson! Returning to the main menu..."
    ),
    utilities = list(
      "Attempting to load lesson dependencies...",
      "Package",
      "loaded correctly!",
      "This lesson requires the",
      "package. Would you like me to install it for you now?",
      # 6-10
      "Trying to install package",
      "now...",
      "Could not install package",
      "Correct script not found at ",
      "Completing the first part of the lesson for you..."
    ),
    zzz = list(
      "Hi! I see that you have some variables saved in your",
      "workspace. To keep things running smoothly, I recommend you clean up",
      "before starting swirl.",
      "Type ls() to see a list of the variables in your workspace.",
      "Then, type rm(list=ls()) to clear your workspace.",
      "Type swirl() when you are ready to begin.",
      "Hi! Type swirl() when you are ready to begin."
    )
)

save(english_strings, file = "R/sysdata.rda")
