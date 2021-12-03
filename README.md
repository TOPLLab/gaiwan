# Gaiwan

## Style

We use neoformat with:

- Haskell: Ormolu
- All other: prettier

## Language Features

A Gaiwan program is specified as a series of computation steps to be performed
in order. Each step has one or more input buffers and one or more output
buffers.

### Kinds of operations

Gaiwan distinguishes between two kinds of operations: mapping and shuffling.

#### Mapper

A mapper changes the values of items in a buffer by applying a function to them.
This function combines one value of multiple buffers to produce one or more new
values.

#### Shufflers

A shuffler rearanges items in buffers. The output of a shuffler can only contain
values from the input buffers.

```
shuffler(i, A, Alen, B, Blen){
  A[i] ; B[i] ; A[i]
}
```

#### `split` and `join` (A special kind of shufflers)

Split and join serve to split and join multiple buffers at once in a zipping way

### Acting on multiple buffers

We can work with multiple buffers by separating the expressions in the shuffler
and mappers with a semicolon.

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

## Implementation

The input program is converted into one long list of
[PipeLineStep](src/Pipelining.hs). Each of these steps describe the list of
input buffers and output buffers and how the input buffer is converted to the
output buffer.

### Optimizations

#### Repeated mapping/shuffling

If multiple mappers are chained, they can be unified to one. If multiple
shuffling are chained, we can compose the shuffle operations.

A map chained with shuffler is not optimized.

### Backend

We use OpenCL as a backend for doing the computations.

## Benchmarking

The code can be benchmarked by using the `stack-bench` from the `nix-shell`

Or by issueing:

```bash
stack bench
```

Always report the GPU with benchmark results
