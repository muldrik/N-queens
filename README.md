# Mathlogic hw-2. N Queens problem
![Test CI](https://github.com/muldrik/N-queens/actions/workflows/haskell.yml/badge.svg)

The program accepts an integer `N` and determines whether it is possible to place `N` queens on an `N` by `N` chessboard without hitting each other. If a solution is found, the setup is printed to console

Before the first execution it is recommended to run
    
    cabal update

### Example usage

    cabal run 'queen-sat' 8  

Output:

```
[. . . . . . . Q]
[. Q . . . . . .]
[. . . . Q . . .]
[. . Q . . . . .]
[Q . . . . . . .]
[. . . . . . Q .]
[. . . Q . . . .]
[. . . . . Q . .]
```


### To execute tests locally run
    cabal run 'queen-sat-test'
