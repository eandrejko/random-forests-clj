# Random Forests in Clojure

A simple implementation of Random Forests for classification and regression in Clojure.

Features:
- Supports categorical, continuous and text features (as bag of words)
- Supports classification
- Supports regression
- Estimates out of sample error during training

Limitations:
- All training examples must fit into memory

A description of random forests can be found at: [http://www.stat.berkeley.edu/~breiman/RandomForests/](http://www.stat.berkeley.edu/~breiman/RandomForests/).

Decision trees are constructed recursively as anonymous functions choosing splitting nodes that minimize the Gini impurity.  A textual representation of the generated tree is generated and stored as meta data.

## Usage

To use add to your `project.clj`:

```clojure
    [random-forests-clj "0.2.0"]
```

## Example

Feaures are represented by the index in the training example.  A forest can be built using the `build-random-forest` method providing the training examples and the indices of the features to use.

```clojure
    (use 'random-forests.core)

    ;; target is in the last position
    (def examples (list ["M" "<25" 1] ["M" "<40" 0] ["F" "<35" 1] ["F" "<30" 1]))

    ;; features can be continuous, categorical or text
    (def features (set (list (feature 0 :categorical) (feature 1 :categorical))))

    ;; return a lazy sequence of decision trees with:
    ;; - 2 random feature values to determine split per splitting node
    ;; - a bootstrap resample of 3 examples per tree
    (def t (first (build-random-forest examples features 2 3))
    (meta t) ;; => {:tree "if(1=<40){0}else{1}"}
```

Each tree is a function, and new examples can classified by calling the function:

```clojure
    (t ["M" "<20"]) ;; => 1
```

## Feature Encoding

Categorical features can be left as strings, continuous features should be stored as doubles, text features should be stored as sets:

```clojure
   (use '[random-forests.encoding :only (text-tokens)])

   ;; text features should be encoded as text using text-tokens for stemming
   (set (text-tokens "this is a text training example")) ;; => #{"train" "text" "exampl"}

   ;; training examples with categorical, continuous and text feature
   ;; (target is last element)
   (def examples (list ["a" 4.5 #{"foo" "bar"} 1] ["b" 4.6 #{"bar" "baz"} 0])

```

## Command Line Usage

Models can built from the command line using `lein run`:

```
Usage:

 Switches                   Default  Desc
 --------                   -------  ----
 -h, --no-help, --help      false    Show help
 -f, --features             []       Features specification (matching CSV header): name=continuous,foo=text
 -s, --size                 1000     Size of bootstrap sample per tree
 -m, --split                100      Number of features to sample for each split
 -o, --output                        Write detailed training error output in CSV format to output file
 -t, --target                        Prediction target name
 -b, --no-binary, --binary  false    Perform binary classification of target (measures AUC loss)
 -l, --limit                100      Number of trees to build
 ```

To build a binary classifier on the provided test data set using a
forest of 500 trees:

```
lein run -f V1=categorical,V2=categorical,V3=categorical,V4=categorical,V5=categorical,V6=categorical,V7=categorical,V8=categorical,V9=categorical \
         -l 500 \
         -t target=continuous \
         -b \
         test/data/cancer.csv

```

which will output out of sample AUC loss for the entire forest as each tree is added to the forest:

```
1: 0.875000
2: 0.843000
3: 0.824000
4: 0.798000
5: 0.843000
6: 0.855000
7: 0.855000
8: 0.878000
9: 0.864000
10: 0.883000
11: 0.879000
12: 0.892000
13: 0.906000
14: 0.906000
15: 0.935000
...
```

## License

Copyright (C) 2010-2012 Erik Andrejko

Distributed under the Eclipse Public License, the same as Clojure.
