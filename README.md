turing-hs
=========

Turing Machine interpreter written in haskell during the [Meta-Rhein-Main Chaos Days 2012](http://mrmcd.net) in Darmstadt, Germany.

Turing Machine descriptions are passed to the program in the form of a config file containing a description of the transition function. A line starting with '#' is treated as a comment and ignored. Check the enclosed sample configurations for the layout of the machine descriptions.  

The second parameter gives the initial input on which to run the Turing Machine. This is the blank symbol ('⎵') by default.
