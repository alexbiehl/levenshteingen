levenshteingen
==============

This program generates the states for levenshtein automata for arbitary n. The alogorithm is implemented after Schulz and Mihov in Fast String Correction with Levenshtein-Automata. It's in a very early state and will most likely not improve very much over time. Take it as is.
The code is quite bad but, well, it's only used to generate the states and is nerver used in production itself.

- The one thing I'm going to implement is a proper naming for the states for easier reading the output.

To specify the max edit distance you have to edit the editDistance function in source code and compile again. 


