# Compile-time evaluation architecture

GoML compile-time evaluation is a typed frontend phase:

```text
lexer → parser → CST → AST → HIR → TAST
                                      │
                                      ├─ capability validation
                                      ├─ CTIR lowering
dependency interfaces and CTIR ───────┤
                                      ├─ CTIR verification
                                      ├─ deterministic evaluation
                                      └─ value reification
                                                │
                                                ▼
                                      finalized TAST
                                                │
                           Core → Mono → Lift → ANF → Go
```

The phase boundary has three invariants:

- `#[comptime]` bodies are validated in their defining package even when no call site currently uses them.
- Every imported CTIR unit is treated as untrusted data and verified before it enters a dependency environment.
- Finalized TAST contains no `Comptime` expression. Core lowering checks this invariant again, so Core and every later IR are independent of CTFE.

CTIR is a typed, direct-call, tree-walking representation. It contains deterministic values, local state, structured control flow, pattern matching, direct calls, and `compile_error`. It has no closures, indirect calls, dynamic dispatch, runtime hooks, host I/O, references, channels, or concurrency. Local slot types, call signatures, control-flow targets, source-origin IDs, structured value types, and target compatibility are verified before evaluation.

Evaluation uses semantic fuel, call-depth, temporary-node, temporary-memory, and final-result-size limits. It never uses a wall-clock deadline. Direct-call results are memoized only within the current evaluation. Integer values store signedness, width, and raw bits; the CTIR target specification determines the width of `int` and `uint` and participates in semantic hashing.

An interface artifact exports public comptime entries and the reachable closure of private comptime helpers and constants. Public constants carry canonical values. Source origins are debug metadata, use package-relative paths, and do not participate in the CTIR semantic hash. Function bodies, local slot types, target semantics, public values, and referenced dependency interface hashes do participate. The artifact decoder checks its format and semantic hash, and dependency loading then runs the CTIR verifier with the complete imported module set.

CTFE failures are recoverable compiler diagnostics. They identify the failing source origin, the requesting comptime site, and the compile-time call stack. A failed or resource-exhausted evaluation never terminates the compiler process.
