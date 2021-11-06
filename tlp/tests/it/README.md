# Integration test

Each module in `tests/` is treated as a crate by Rust. So let's wrap them all in a single crate (`it`) and reduce the build time.

See also: [Delete Cargo Integration Tests](https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html)

