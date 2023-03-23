# Gaiwan

Gaiwan is a GPU programming language focused on processing time series data.


## Language Features

A Gaiwan program is specified as a series of computation steps to be performed
in order. Each step has one or more input buffers and one or more output
buffers.

### Kinds of operations

Gaiwan distinguishes between three kinds of operations: mappers, reducers and shapers.

#### Mapper

A mapper changes the values of items in a buffer by applying a function to them.
It takes in one element of the input buffer (which may be a tuple) and returns a new value to use in its place.

A mapper that increments every value of a buffer is shown below. (It is of type `int[n]->int[n]`)

```
mapper doinc(i, a:int) : int {
    a+1
}
```

#### Shaper

A shaper rearanges items in buffers. The output of a shuffler can only contain
values from the input buffers and must not inpect their values.


A shaper that takes two elements from a buffer named `a` and one element of a buffer named `b` is shown below.
The function signature mandates that `a` is twice as long as `b` and that `a` and `b` both have elements of type `T`.
The result is an array containing triples (`tuple(T,T,T)`) of lenght `n`, the length of `b` (and half the length of `a`).

```
shaper makePairs(i, a:T[2*n], b:T[n]) : tuple(T,T,T)[n] {
    tuple(a[2*i], a[2*i+1], b[i])
}
```

#### Reducers

Reducers combine all the values of a buffer.
A reducer has an accumulator and repeatedly receives a value from the buffer to combine with it.
The result of a reducer is always a buffer of length 1.


Below, a reducer of type `int[n]->int[1]` is shown, reduces a buffer of any length `n` to a buffer of length one containing the sum of the values.
The signature also contains an initial value for the accumulator (`acc`), in this case 0.

```
reducer sum(i,acc: int , d : int) : int[1] (0){
    d + acc
}
```

### Abstractions

Abstractios allows combining operaions and giving them a name.
They combine multiple operations (explained above) with a `|` symbol.
These operaions are executed one after the other.


Below, an example of an abstraction is shown that increments every value of a buffer with a value `v` and then sums the result.
```

abstraction inc(v:int){
    mapper doinc(i, a:int) : int {
        a+v
    }
    |
    reducer sum(i,acc: int , d : int) : int[1] (0){
        d + acc
    }
}
```

An abstriaction can be called from the coordination plan.

### Coordination plan

The Gaiwan operaions above can be invoked by the coordination plan.
Such a plan is a list of steps to execute:

- `return (a,b)`, reads data from the input folder in the files a.*.gw and b.*.gw
- `abstracion_name(args...)` calls an abstracion
- `let name = { <plan_1> } in { <plan_2 >}` executes `plan_1` and stores the result in `name` such that it can be retreived with `return name` from withing `<plan_2>` the result of the entire construct is the result of `<plan_2>`

### Programs

A program consists of a list of abstractions (separated by newlines) and a coordination plan.

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

There are also some scripts in the `bench` folder that may be modified to repeatedly run a program.
