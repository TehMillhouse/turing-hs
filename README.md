turing-hs
=========

Turing Machine interpreter written in haskell during the [Meta-Rhein-Main Chaos Days 2012](http://mrmcd.net) in Darmstadt, Germany.

Turing Machine descriptions are passed to the program in the form of a config file containing a description of the transition function. A line starting with '#' is treated as a comment and ignored. Check the enclosed sample configurations for the layout of the machine descriptions.  

The second parameter gives the initial input on which to run the Turing Machine. This is the blank symbol ('⎵') by default.

Here, have an example:
```
~/turing-hs % ./turing ./busybeaver3-2 "⎵1"  
╭A ╮  
╰⎵1╯  
╭ B╮  
╰11╯  
╭  B╮  
╰11⎵╯  
╭   C╮  
╰11⎵⎵╯  
╭  C ╮  
╰11⎵1╯  
╭ C  ╮  
╰1111╯  
╭A   ╮  
╰1111╯  
╭ H  ╮  
╰1111╯  
```

Caveats
-------

* Technically, the config file is redundant - alphabet and state list can be extracted from the function table alone. I haven't gotten around to fixing this yet
* Spacing is crucial in the function table - `('⎵',L,"A")` is a legal transition, `('⎵', L, "A")` _isn't_.
