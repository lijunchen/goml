# bootstrap-goml

`bootstrap-goml` is the minimal GoML implementation of the project build and test driver. The Rust `goml` implementation remains available and is used to create the initial binary.

Build it with the Rust driver:

```sh
cargo run -p goml -- build bootstrap-goml
```

Use the checked-in GoML compiler implementation:

```sh
bootstrap-goml/artifact/bin/bootstrap_goml check bootstrap-goml --compiler bootstrap/artifact/bin/gomlc
bootstrap-goml/artifact/bin/bootstrap_goml build bootstrap-goml --compiler bootstrap/artifact/bin/gomlc
bootstrap-goml/artifact/bin/bootstrap_goml test bootstrap-goml --compiler bootstrap/artifact/bin/gomlc --jobs 4
```

The current implementation provides deterministic local package discovery, topological check/build/link plans, executable runs, internal test discovery, filtering, ignored tests, text or JSON test events, and a bounded `Channel` worker pool.

Registry dependencies, external black-box tests, incremental fingerprints, test timeouts, and package-management commands remain in the Rust driver for now.
