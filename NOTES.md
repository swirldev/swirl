# NOTES

Could instructions be S3 objects with execute methods? If so, what would they look like?

Output instructions could be prettyOut(text), browseURL(url), source(r-file)
Input instructions include exit/reenter => expr, val, ok, vis
* which requires a flag to indicate completion
Combined instructions include 
* readline(text) => text and 
* select.list(list) => text 

tests require data but that's no problem since everything in the persistent environment can be named. Moreover, there is no reason 

The 9 testMod4Daphne.csv questions as FSM pseudo-code

1. "Hi, welcome to swirl!"
  * **print** output; **response**=NULL; **tests**=NULL
  * readline("..."); **response**="play"||any; **tests**=suspend/continue; **branch**=prompt/next

2. "To create a new variable in R..." same as 1.

3. "Create a new variable that stores the value 20."
  * **print** output & get **response** from prompt: 
     **tests**: assign; newVar; word=20, 
     **branch**: praise if correct; hint if not
  * HINT: **print** hint & get **response** from prompt;
    **tests**: assign; newVar; word=20
    **branch**: praise if correct; hint if not
  * PRAISE: **print** praise; **response**=NULL; **tests**=NULL;
  * readline("..."); **response**="play"||any; test=suspend/continue; **branch**=prompt/next
  
4. "The syntax for creating a vector of numbers..." (same as 1.)
  
5. "Try creating a vector of numbers..." same as 3 except:
  * **tests**: assign; newVar; useFunc=c
  
6. "Now compute the mean of these numbers.." same as 2 & 3 except:
  * **tests**: useFunc=mean; result=mean(newVar)
  
7. "Which function in R computes the mean of a bunch of numbers?"
  * **print** output and select.list(median(); mean(); average());
    **tests** word=mean();
    **branch** PRAISE if correct; HINT if not
  * HINT: **print** hint & get **response** from prompt;
    **tests**: word=mean();
    **branch**: praise if correct; hint if not
  * PRAISE: **print** praise; **response**=NULL; **tests**=NULL;

8. "Would you like to view a brief YouTube video.."
  * **print** output and **response** readline("Yes or No > ");
    **tests** yes or no; **branch** continue if no, UTUBE if yes
  * UTUBE: output browseURL, response= nxt() from prompt.
  
9. "Great work!..." same as 1.

    


