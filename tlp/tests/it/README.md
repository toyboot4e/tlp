# Integration test

## Why `it/` directory?

Each module in `tests/` is treated as a crate by Rust. So let's wrap them all in a single crate (`it`) and reduce the build time.

See also: [Delete Cargo Integration Tests](https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html)

## Tests

### Syntax test

It has data-driven test cases like:

```
# num
(+ 1.15e-4 2)
----------------------------------------
Call@0..13
    LParen@0..1
    Path@1..2
        Ident@1..2
    Ws@2..3
    Literal@3..10
        Num@3..10
    Ws@10..11
    Literal@11..12
        Num@11..12
    RParen@12..13
----------------------------------------
```

### IR test

It has some hard-coded IR item tests.

### Type tests

It has some hard-coded IR type tests.

### VM tests

It's going to have bytecode tests.

