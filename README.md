levenshteingen
==============

This program generates the states for levenshtein automata for arbitary n. The alogorithm is implemented after Schulz and Mihov in Fast String Correction with Levenshtein-Automata. It is in very early state and will most likely not improve very much over time. Take it as is.
The code is quite bad but, well, it's only used to generate the states and is nerver used in production itself.

- Accepting states need to be printed

To specify the max edit distance you have to edit the editDistance function in source code and compile again. 

The output can be interpreted as 

"fromState" -> (x, k) -> ("toState", d)

where x is the characteristic vector of W with length k = min(2 * n + 1, w - i) and i denotes the minimal boundary of the current state. The start state is always (A, 0), which means, state A with boundary 0. d means the increment to the current boundary. 
