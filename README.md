# Haskell Assignment 4 - Palindrome TM (RSSELI007)
Hi there!

To run the palindrome TM, use the palindromeTM function in GHCi as such:
palindromeTM *string*
where *string* is a string over {a,b}

ps The function prints out the configuration of the machine for each step

## TM Configurations for string "aba" (in order)
<Q0, a, _, Q1>
<Q1, b, b, Q1>
<Q1, a, a, Q1>
<Q1, _, _, Q2>
<Q2, a, _, Q3>
<Q3, b, b, Q3>
<Q3, _, _, Q0>
<Q0, b, _, Q4>
<Q4, _, _, Q5>
<Q5, _, _, QAccept>