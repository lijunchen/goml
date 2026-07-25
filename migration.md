# Compiler Test Migration

This file records the relationship between the Rust compiler tests and their
bootstrap GoML equivalents. Counts refer to Rust test functions unless a row
explicitly refers to fixture cases.

## Status

- `migrated`: the GoML test provides equivalent or stronger observable coverage.
- `oracle`: the Rust test remains only to generate snapshots or compare Rust and
  bootstrap implementations.
- `blocked`: the Rust and bootstrap implementations do not currently agree.
- `internal`: the test inspects Rust-only data structures or implementation
  invariants and should remain in Rust.
- `pending`: the test has not been migrated yet.

## Corpus suites

| Rust source or fixture suite | GoML equivalent | Status |
| --- | --- | --- |
| `crates/compiler/src/tests/pipeline/` | `bootstrap/pipeline_test/pipeline_test.gom` | migrated; Rust `tests::test_cases` remains as snapshot oracle |
| `crates/compiler/src/tests/e2e/good/` | `bootstrap/compiler_test/e2e_test.gom` | migrated, 558 fixtures |
| `crates/compiler/src/tests/e2e/bad/` | `bootstrap/compiler_test/e2e_test.gom` | migrated, 71 fixtures |
| `crates/compiler/src/tests/diagnostics/` | `bootstrap/compiler_test/diagnostics_test.gom` | migrated, 4 fixtures |
| `crates/compiler/src/tests/typer/` | `bootstrap/compiler_test/diagnostics_test.gom` | migrated, 84 fixtures |
| `crates/compiler/src/tests/module/` | `bootstrap/compiler_test/module_test.gom` | migrated, 34 projects plus binary stdio coverage |
| `crates/compiler/src/tests/module_diagnostics/` | `bootstrap/compiler_test/module_test.gom` | migrated, 24 projects |
| `crates/compiler/src/tests/crashers/` | `bootstrap/compiler_test/crashers_test.gom` | migrated, 100 fixtures |
| `crates/compiler/src/tests/trait_impl/` | `bootstrap/compiler_test/trait_impl_test.gom` | migrated, 84 fixtures representing 76 Rust tests |
| `crates/compiler/src/tests/struct_type/` | `bootstrap/compiler_test/struct_type_test.gom` | migrated, 3 fixtures |
| `crates/compiler/src/tests/bootstrap/` | no replacement | oracle; Rust/bootstrap differential infrastructure |

Every migrated diagnostic fixture maps as follows:

```text
crates/compiler/src/tests/<suite>/<case>.gom
crates/compiler/src/tests/<suite>/<case>.gom.diag
    -> bootstrap/compiler_test/<suite>_test.gom
```

Every migrated runtime fixture maps as follows:

```text
crates/compiler/src/tests/<suite>/<case>/main.gom
crates/compiler/src/tests/<suite>/<case>/main.gom.out
    -> bootstrap/compiler_test/<suite>_test.gom
```

## Rust module inventory

| Rust module | Current count | GoML destination | Status |
| --- | ---: | --- | --- |
| `trait_impl_test.rs` | 2 | `bootstrap/compiler_test/trait_impl_test.gom` | migrated; 76 observable tests migrated and 2 internal assertions retained |
| `visibility_test.rs` | 2 | `bootstrap/compiler_test/module_test.gom` | partially migrated; 18 migrated and 2 blocked |
| `package_model_test.rs` | 8 | `bootstrap/compiler_test/module_test.gom` | partially migrated; 3 migrated and 8 blocked |
| `entrypoint_test.rs` | 0 | module diagnostics and crasher fixtures | migrated |
| `toplevel_validation_test.rs` | 1 | crasher fixtures | partially migrated; 11 migrated and 1 blocked |
| `dyn_coercion_test.rs` | 13 | crasher fixtures | partially migrated; retain Go AST assertions |
| `while_expr_test.rs` | 0 | crasher fixtures | migrated |
| `operator_semantics_test.rs` | 0 | e2e and crasher fixtures | migrated |
| `struct_type_test.rs` | 8 | `bootstrap/compiler_test/struct_type_test.gom` | partially migrated; 3 migrated, 3 blocked, and 5 internal |
| `assignment_target_test.rs` | 0 | e2e and crasher fixtures | migrated |
| `constructor_value_test.rs` | 1 | future runtime fixture | pending |
| `multiline_string_test.rs` | 0 | e2e and crasher fixtures | migrated |
| `ref_type_test.rs` | 1 | future diagnostic fixture | pending |
| `try_expr_test.rs` | 3 | future diagnostic/runtime fixtures | pending |
| `tuple_projection_test.rs` | 2 | future runtime fixtures | pending |
| `vec_effect_test.rs` | 1 | crasher fixtures | partially migrated; retain Go AST assertion |
| `testing_test.rs` | 2 | bootstrap `gomlc` and `goml test` self-tests | partially migrated; 5 migrated and 2 internal |
| `separate_compile_test.rs` | 3 | module corpus | partially migrated; 2 migrated, 1 blocked, and 2 internal |
| `query_test.rs` | 44 | no replacement by decision | retained in Rust; query tests are intentionally not migrated |
| `builtin_functions_test.rs` | 11 | no replacement | internal |
| `intrinsics_test.rs` | 7 | no replacement | internal |
| `anf_stack_test.rs` | 7 | no replacement | internal stress and complexity coverage |
| `deep_pattern_test.rs` | 2 | no replacement | internal stress coverage |
| `go_name_mangling_test.rs` | 1 | no replacement | internal Go output invariant |
| `tests::reference_runtime_executes` | 1 | no replacement | internal Rust executor smoke test |
| `tests::go_run_failure_is_error` | 1 | no replacement | internal Rust executor error-path test |
| `compile_match::tests` | 5 | no replacement | internal match compiler unit tests |
| `go::mangle::tests` | 6 | no replacement | internal mangling unit tests |

## Trait implementation migration

For a single-source case, the mapping is:

```text
trait_impl_test.rs::<name>
    -> crates/compiler/src/tests/trait_impl/<name>/main.gom
    -> crates/compiler/src/tests/trait_impl/<name>/main.gom.diag
    -> bootstrap/compiler_test/trait_impl_test.gom
```

Multi-source Rust tests use one additional directory level named after the
original Rust variable, such as `accepted`, `rejected`, `missing`, or
`duplicate`.

### Migrated

- `associated_outputs_disambiguate_generic_trait_methods`
- `associated_type_bound_is_checked_at_impl_definition`
- `associated_type_binding_must_match_trait_method_signature`
- `associated_type_cycles_are_rejected`
- `associated_type_impl_requires_complete_known_unique_bindings`
- `associated_type_projection_requires_one_defining_bound`
- `ambiguous_trait_goal_does_not_commit_inference`
- `ambiguous_trait_goal_is_retried_after_unique_inference`
- `canonical_trait_cache_replays_unique_inference`
- `coherence_is_rechecked_after_all_impls_are_collected`
- `composite_type_parameter_trait_goal_requires_proof`
- `deterministic_coercion_precedes_trait_inference`
- `dyn_trait_unavailable_method_is_reported`
- `dyn_trait_parent_method_ambiguity_is_reported`
- `equality_operator_requires_eq_evidence`
- `equality_predicate_transfers_trait_bound`
- `expected_return_type_disambiguates_generic_trait_method`
- `explicit_generic_trait_arguments_are_validated`
- `for_loop_accepts_custom_into_iterator`
- `for_loop_uses_iterator_associated_item_type`
- `forward_and_diamond_supertraits_are_supported`
- `generic_into_iterator_bound_implies_iterator_for_into_iter`
- `generic_trait_arguments_are_validated`
- `generic_trait_arity_is_checked`
- `generic_trait_applications_are_distinct`
- `generic_trait_parameters_and_signatures_are_validated`
- `generic_traits_are_rejected_as_dyn_types`
- `impl_equality_predicate_restricts_application`
- `impl_for_generic_type_reports_missing_method_diagnostic`
- `impl_for_struct_reports_missing_method_diagnostic`
- `impl_for_unknown_trait_reports_diagnostic`
- `impl_missing_trait_method_reports_diagnostic`
- `impl_where_predicate_controls_trait_selection`
- `impl_with_extra_method_reports_diagnostic`
- `impl_with_mismatched_param_type_reports_diagnostic`
- `impl_with_mismatched_return_type_reports_diagnostic`
- `impl_with_parameter_arity_mismatch_reports_diagnostic`
- `inherited_method_name_conflicts_remain_ambiguous`
- `into_iterator_impl_rejects_inconsistent_item`
- `into_iterator_impl_rejects_non_iterator_into_iter`
- `invalid_impl_does_not_satisfy_trait_goal`
- `iterator_associated_item_prevents_conflicting_impls`
- `local_projection_type_positions_are_resolved`
- `method_arguments_disambiguate_generic_trait_applications`
- `method_resolution_waits_for_receiver_inference`
- `nested_impl_bound_can_drive_inference`
- `nested_impl_bound_must_be_satisfied`
- `overlapping_generic_and_concrete_impls_are_rejected_at_definition`
- `overlapping_generic_trait_applications_are_rejected`
- `projected_trait_application_impl_is_selectable`
- `projection_only_impl_parameter_is_rejected`
- `structural_where_equality_relates_nested_type_parameters`
- `supertrait_cycles_are_rejected`
- `supertrait_impl_is_required_at_definition`
- `symbolic_projection_equality_selects_single_impl`
- `trait_coverage_associated_type_bound_is_implied`
- `trait_coverage_constrained_blanket_is_disjoint_without_bound`
- `trait_coverage_constrained_blanket_method_is_available_with_bound`
- `trait_coverage_constrained_blanket_method_is_unavailable_without_bound`
- `trait_coverage_constrained_blanket_overlaps_when_bound_holds`
- `trait_coverage_declaration_where_predicate_is_implied`
- `trait_coverage_generic_applications_have_distinct_associated_types`
- `trait_coverage_generic_parameter_bound_is_implied`
- `trait_coverage_projection_equality_transfers_bound`
- `trait_coverage_supertrait_associated_type_is_projectable`
- `trait_goal_infers_nested_type_from_unique_impl`
- `trait_impl_methods_cannot_add_type_parameters`
- `trait_parameter_bounds_are_required_at_impl_definition`
- `trait_self_predicates_are_resolved_and_enforced`
- `traits_with_associated_types_are_rejected_as_dyn_types`
- `unconstrained_impl_type_parameter_is_rejected`
- `unused_generic_bound_still_creates_an_obligation`
- `where_constructed_bound_is_checked_at_call_site`
- `where_predicate_accepts_constructed_trait_receiver`
- `where_type_equality_is_available_in_generic_body`
- `where_type_equality_is_checked_at_call_site`

### Covered by the migrated crasher corpus

| Removed Rust test | Existing fixture |
| --- | --- |
| `builtin_generic_constraints_are_checked_at_call_site` | `crashers/println_option_without_tostring/main.gom` |
| `generic_constraints_reject_overlapping_trait_impls_at_definition` | `crashers/hashmap_ref_dyn_hash_overlapping_impl/main.gom` |
| `recursive_blanket_trait_impl_bound_does_not_crash` | `crashers/recursive_blanket_trait_impl_bound/main.gom` |

### Rust-only internal assertions

- `inherent_impl_registers_methods`
- `inherent_impl_instantiates_self_types`

These inspect `GlobalTypeEnv`, method schemes, and TAST nodes directly.

## Module and validation migration

The following entrypoint tests are migrated:

| Removed Rust test | GoML coverage |
| --- | --- |
| `main_function_with_parameter_is_rejected` | `crashers/main_with_parameter/main.gom` |
| `missing_main_function_is_rejected` | `crashers/missing_main/main.gom` |
| `canonical_main_package_rejects_parameter` | `module_diagnostics/canonical_main_package_rejects_parameter/` |
| `canonical_main_package_rejects_type_parameter` | `module_diagnostics/canonical_main_package_rejects_type_parameter/` |
| `canonical_main_package_requires_main_function` | `module_diagnostics/canonical_main_package_requires_main_function/` |

Eleven `toplevel_validation_test.rs` tests map directly to same-purpose
fixtures in `crates/compiler/src/tests/crashers/`. The remaining
`user_lang_item_declaration_is_rejected` test is blocked because bootstrap
currently accepts the declaration.

The following visibility tests are migrated to same-named directories under
`crates/compiler/src/tests/module_diagnostics/`:

- `public_function_is_visible`
- `private_function_is_hidden`
- `private_function_can_feed_public_function`
- `public_struct_is_visible`
- `private_struct_is_hidden`
- `public_enum_is_visible`
- `private_enum_is_hidden`
- `public_trait_import_enables_method_syntax`
- `private_trait_import_is_hidden`
- `public_associated_type_cannot_expose_private_type`
- `private_struct_field_is_hidden`
- `struct_with_private_fields_cannot_be_constructed_cross_package`
- `struct_pattern_with_private_fields_requires_rest`
- `struct_pattern_with_private_fields_accepts_rest`
- `public_inherent_method_is_visible`
- `private_inherent_method_is_hidden`
- `enum_variant_fields_cannot_use_pub`
- `trait_implementation_methods_cannot_use_pub`

The retained visibility blockers are:

- `public_field_cannot_expose_private_type`
- `public_inherent_method_cannot_expose_private_type`

The following package model tests are migrated to same-named directories
under `crates/compiler/src/tests/module_diagnostics/`:

- `explicit_aliases_allow_same_declared_package_name`
- `declared_package_name_is_the_default_alias`
- `transitive_public_type_metadata_is_available`

The retained package model blockers are:

- `imports_are_file_scoped`
- `package_alias_trait_use_is_order_independent`
- `every_project_file_requires_a_package_declaration`
- `files_in_one_directory_must_declare_one_package`
- `duplicate_package_import_is_rejected`
- `ambiguous_package_alias_is_rejected`
- `nested_module_is_not_loaded_as_a_package`
- `transitive_dependencies_are_not_source_visible`

## Struct, testing, and separate compilation migration

The following struct type diagnostics map to same-named fixtures under
`crates/compiler/src/tests/struct_type/`:

- `struct_type_arity_mismatch_reports_error`
- `unknown_type_constructor_reports_error`
- `unbound_type_parameter_reports_error`

The three equivalent enum payload diagnostics remain in Rust because the
bootstrap compiler omits the source position emitted by Rust. The other five
retained tests inspect `GlobalTypeEnv` or TAST nodes directly.

The following `testing_test.rs` coverage is migrated:

| Removed Rust test | GoML coverage |
| --- | --- |
| `test_build_collects_and_links_top_level_tests` | `bootstrap/cmd/gomlc/main_test.gom::collects_and_encodes_test_descriptors` and `bootstrap-goml/cmd/goml/cli_migration_test.gom::project_test_runs_private_tests_and_ignores_test_sources_in_check` |
| `invalid_test_signatures_are_rejected` | `bootstrap/cmd/gomlc/main_test.gom::collects_all_invalid_test_signature_diagnostics` and `bootstrap-goml/cmd/goml/cli_migration_test.gom::project_test_dry_run_and_invalid_signature_diagnostics` |
| `test_attribute_rejects_non_top_level_functions` | `bootstrap/cmd/gomlc/main_test.gom::collects_all_invalid_test_attribute_diagnostics` |
| `malformed_test_attributes_are_rejected` | `bootstrap/cmd/gomlc/main_test.gom::collects_all_invalid_test_attribute_diagnostics` |
| `duplicate_test_ids_have_cross_file_labels` | `bootstrap/cmd/gomlc/main_test.gom::labels_duplicate_test_ids_across_files` |

`test_link_accepts_multiple_test_package_roots` and
`analysis_compilation_retains_exact_override_sources` remain as Rust-only API
invariants.

Two `separate_compile_test.rs` behaviors are covered by stronger module
fixtures:

| Removed Rust test | GoML coverage |
| --- | --- |
| `separate_build_link_matches_project_008` | `module/project008_trait_bounds_across_packages/` |
| `separate_build_link_supports_std` | `module/project032_std_host_api/` |

`user_package_cannot_import_std_internal_host` remains in Rust because the
bootstrap compiler currently reports a missing static member instead of the
Rust compiler's package visibility diagnostic. The remaining two tests assert
linker artifact invariants and remain internal.

All 44 `query_test.rs` tests intentionally remain in Rust and are excluded
from the migration scope.

## Runtime and control-flow migration

The complete `operator_semantics_test.rs` module is migrated. Its diagnostic
and short-circuit cases map to same-named crasher fixtures. The
`numeric_bit_operators_and_casts_execute` case maps to
`e2e/good/0592_numeric_bit_operators_and_casts/`.

The complete `assignment_target_test.rs` module is migrated:

| Removed Rust test | GoML coverage |
| --- | --- |
| `shadowed_ref_get_array_assignment_is_rejected` | `crashers/ref_get_shadow_array_assignment_target/` |
| `intrinsic_ref_get_array_assignment_executes` | `e2e/good/0593_intrinsic_ref_get_array_assignment/` |

The complete `while_expr_test.rs` module maps to crasher fixtures. Three
previously inline cases now have dedicated fixtures:

- `crashers/while_condition_all_exit_match_wrapped_enum/`
- `crashers/while_condition_all_exit_match_wrapped_tuple/`
- `crashers/while_condition_all_exit_match_wrapped_array/`

The complete `multiline_string_test.rs` module is migrated:

| Removed Rust test | GoML coverage |
| --- | --- |
| `multiline_string_prints_lines` | `e2e/good/0594_multiline_string/` |
| `string_nul_escape_executes` | `crashers/string_nul_escape/` |

Three runtime-only tests from `vec_effect_test.rs` map to the existing
`discarded_let_vec_push_side_effect`, `vec_push_preserves_existing_binding`,
and `discarded_vec_len_pure_call` crasher fixtures. The remaining
`vec_method_push_mutates_shared_storage_in_go_codegen` test stays in Rust
because it asserts the generated Go representation directly.

Ten runtime or diagnostic tests from `dyn_coercion_test.rs` map to same-named
crasher fixtures:

- `dyn_tostring_builtin_impl_executes`
- `dyn_tostring_ref_dyn_impl_executes`
- `dyn_hash_ref_dyn_impl_executes`
- `direct_ref_dyn_show_hash_impl_is_rejected`
- `hash_ref_dyn_trait_builtin_ref_impl_executes`
- `hashmap_ref_dyn_hash_explicit_eq_is_rejected`
- `hashmap_ref_dyn_hash_builtin_ref_impl_executes`
- `hashmap_ref_dyn_show_explicit_eq_hash_is_rejected`
- `hashmap_dyn_hash_explicit_eq_executes`
- `dyn_trait_hashmap_method_set_if_return_subexpr_executes`

The remaining dyn coercion tests inspect generated Go vtables and type
declarations and therefore remain internal Rust tests.

## Verification commands

```text
just test-bootstrap-compiler
cargo test -p compiler --features bootstrap-tests tests::bootstrap::compiler_test_suites_match -- --test-threads=1
cargo test
cargo fmt --check
cargo clippy --all-targets --all-features --locked -- -D warnings
```
