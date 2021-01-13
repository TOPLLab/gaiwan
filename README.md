# gaiwan


## Style

We use neoformat with:

- Haskell:  britanny
- All other: prettier


## Language Features

### Loops

A step can be repeated (like a for loop with):

```
...  |  3:i { ... steps to repeat (using i) ... } | ...
```

is equivalent to:

```

...  |        ... steps to repeat (using 0) ...
     |        ... steps to repeat (using 1) ...
     |        ... steps to repeat (using 2) ...   | ...
```
