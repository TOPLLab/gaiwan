# Gaiwan

## Style

We use neoformat with:

- Haskell: britanny
- All other: prettier

## Language Features

A Gaiwan program is specified as a series of computation steps to be performed
in order. Each step has one or more input buffers and one or more output
buffers.

### Kinds of operations

Gaiwan distinguishes between two kinds of operations: mapping and shuffling.

#### Mapper

If a mapper takes n input buffers, and has m output buffers, a mapper must
generate an m-tupple of values derived from the n-tuple on the input.

#### Shufflers

Move the data in an array form place

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
