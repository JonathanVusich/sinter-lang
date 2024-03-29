### Typechecking notes

1. We only need to type check function bodies and global const expressions.
2. Generic parameters should be instantiated for each generic usage.
3. The THIR only needs to represent inferrable expressions.
4. The THIR needs to be traversable and folded for type substitution once
   all constraints have been collected.