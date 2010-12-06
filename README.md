# random-forests

An incomplete implementation of Random Forests in Clojure. 

A description of random forests can be found at: http://www.stat.berkeley.edu/~breiman/RandomForests/

Decision trees are constructed recursively as anonymous functions choosing splitting nodes that minimize the Gini impurity.  A textual representation of the generated tree is generated and stored as meta data.

## Example

Feaures are represented by the index in the training example.  A tree can be built using the build-tree method providing the training examples and the indices of the features to use.

    (def t (build-tree (list ["M" "<25" 1] ["M" "<40" 0] ["F" "<35" 1] ["F" "<30" 1] ) #{0 1}))
    (meta t) ;; => {:tree "if(1=<40){0}else{1}"}

The tree is a function, and new examples can classified by calling the function:

    (t ["M" "<20"]) ;; => 1

## License

Copyright (C) 2010 Erik Andrejko

Distributed under the Eclipse Public License, the same as Clojure.
