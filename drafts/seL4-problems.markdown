# Problems with the seL4 project

## Problems with Isabelle

###  Some thoughts on Isabelle emacs improvements

Have output of thm, locate, term, etc go to a different buffer. This buffer should be added to so that you can see your history.

### Pain point: find_theorem is sensitive to order

this works: 

```
find_theorems "⟦ ¬?P; ?P ⟧ ⟹ _"
```

but this doesn't:

```
find_theorems "⟦ ?P; ¬?P ⟧ ⟹ _"
```


## The every growing list of gripes

* Tool `find_theorems` doesn't have any documentation anywhere.
* c-kernel source code, especially the headers where all the data
* structures are stored has no comments! Comments would be extremely
   useful here. It would like to know:
     - what various things, such as `pde`, `pte`, `cte` stand for. I
       have some idea but it should be explicit.
     - where various data strutures are used, some use cases. etc.
