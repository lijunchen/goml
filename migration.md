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
  invariants and remains in Rust; a GoML fixture may provide additive
  user-visible coverage.
- `pending`: the test has not been migrated yet.

Migration is additive. All Rust tests and harnesses remain enabled after their
GoML equivalents are added.

## Corpus suites

| Rust source or fixture suite | GoML equivalent | Status |
| --- | --- | --- |
| `crates/compiler/src/tests/pipeline/` | `bootstrap/pipeline_test/pipeline_test.gom` | migrated; Rust `tests::test_cases` remains as snapshot oracle |
| `crates/compiler/src/tests/e2e/good/` | `bootstrap/compiler_test/e2e_test.gom` | migrated, 558 fixtures; Rust e2e harness retained |
| `crates/compiler/src/tests/e2e/bad/` | `bootstrap/compiler_test/e2e_test.gom` | migrated, 71 fixtures; Rust e2e harness retained |
| `crates/compiler/src/tests/diagnostics/` | `bootstrap/compiler_test/diagnostics_test.gom` | migrated, 4 fixtures |
| `crates/compiler/src/tests/typer/` | `bootstrap/compiler_test/diagnostics_test.gom` | migrated, 84 fixtures |
| `crates/compiler/src/tests/module/` | `bootstrap/compiler_test/module_test.gom` | migrated, 34 projects plus binary stdio coverage; Rust module harness retained |
| `crates/compiler/src/tests/module_diagnostics/` | `bootstrap/compiler_test/module_test.gom` | migrated, 35 projects |
| `crates/compiler/src/tests/crashers/` | `bootstrap/compiler_test/crashers_test.gom` | migrated, 102 fixtures |
| `crates/compiler/src/tests/trait_impl/` | `bootstrap/compiler_test/trait_impl_test.gom` | migrated, 84 fixtures representing 76 Rust tests |
| `crates/compiler/src/tests/struct_type/` | `bootstrap/compiler_test/struct_type_test.gom` | migrated, 6 fixtures |
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
| `trait_impl_test.rs` | 81 | `bootstrap/compiler_test/trait_impl_test.gom` | migrated; all Rust tests retained |
| `visibility_test.rs` | 20 | `bootstrap/compiler_test/module_test.gom` | migrated; all Rust tests retained |
| `package_model_test.rs` | 11 | `bootstrap/compiler_test/module_test.gom` | migrated; all Rust tests retained |
| `entrypoint_test.rs` | 5 | module diagnostics and crasher fixtures | migrated; all Rust tests retained |
| `toplevel_validation_test.rs` | 12 | crasher fixtures | migrated; all Rust tests retained |
| `dyn_coercion_test.rs` | 23 | pipeline and crasher fixtures | additive coverage; all Rust tests retained |
| `while_expr_test.rs` | 14 | crasher fixtures | migrated; all Rust tests retained |
| `operator_semantics_test.rs` | 9 | e2e and crasher fixtures | migrated; all Rust tests retained |
| `struct_type_test.rs` | 11 | pipeline and struct diagnostics | additive coverage; all Rust tests retained |
| `assignment_target_test.rs` | 2 | e2e and crasher fixtures | migrated; all Rust tests retained |
| `constructor_value_test.rs` | 1 | `pipeline/209_payload_enum_constructor_value/` | migrated; Rust test retained |
| `multiline_string_test.rs` | 2 | e2e and crasher fixtures | migrated; all Rust tests retained |
| `ref_type_test.rs` | 1 | `pipeline/208_ref_typecheck_and_collect/` | additive coverage; Rust AST/TAST assertion retained |
| `try_expr_test.rs` | 3 | pipeline and crasher fixtures | migrated; Rust tests retained |
| `tuple_projection_test.rs` | 2 | pipeline fixtures | migrated; Rust tests retained |
| `vec_effect_test.rs` | 4 | `pipeline/215_builtin_environment_surface/` and crasher fixtures | additive coverage; all Rust tests retained |
| `testing_test.rs` | 14 | bootstrap `gomlc` and `goml test` self-tests | additive coverage; all Rust tests retained |
| `separate_compile_test.rs` | 5 | module corpus and bootstrap linker self-tests | additive coverage; all Rust tests retained |
| `query_test.rs` | 44 | no replacement by decision | retained in Rust; query tests are intentionally not migrated |
| `builtin_functions_test.rs` | 11 | pipeline, crasher, and iterator fixtures | additive coverage; Rust environment catalog retained |
| `intrinsics_test.rs` | 7 | bootstrap TAST and Go backend self-tests | additive coverage; Rust catalog invariants retained |
| `anf_stack_test.rs` | 7 | bootstrap ANF self-tests and pipeline corpus | partial additive coverage; Rust scale tests retained |
| `deep_pattern_test.rs` | 2 | `bootstrap/ast/ast_test.gom` | additive coverage; Rust stack tests retained |
| `go_name_mangling_test.rs` | 1 | `pipeline/151_generic_closure_multi_instantiation/` | additive coverage; Rust length assertion retained |
| `closure_return_test.rs` | 3 | crasher corpus | additive coverage; all Rust tests retained |
| `monomorphization_test.rs` | 2 | crasher corpus | additive coverage; all Rust tests retained |
| `name_collision_test.rs` | 29 | crasher corpus | additive coverage; all Rust tests retained |
| `tests::reference_runtime_executes` | 1 | bootstrap pipeline execution | additive coverage; Rust executor smoke test retained |
| `tests::go_run_failure_is_error` | 1 | bootstrap process and compiler command failure tests | additive coverage; Rust helper error path retained |
| `compile_match::tests` | 5 | bootstrap ANF/Go backend tests and pattern fixtures | additive coverage; Rust IR unit tests retained |
| `go::mangle::tests` | 6 | bootstrap Go backend self-tests and collision fixtures | additive coverage; Rust unit tests retained |

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

| Rust test | Existing fixture |
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

| Rust test | GoML coverage |
| --- | --- |
| `main_function_with_parameter_is_rejected` | `crashers/main_with_parameter/main.gom` |
| `missing_main_function_is_rejected` | `crashers/missing_main/main.gom` |
| `canonical_main_package_rejects_parameter` | `module_diagnostics/canonical_main_package_rejects_parameter/` |
| `canonical_main_package_rejects_type_parameter` | `module_diagnostics/canonical_main_package_rejects_type_parameter/` |
| `canonical_main_package_requires_main_function` | `module_diagnostics/canonical_main_package_requires_main_function/` |

Eleven `toplevel_validation_test.rs` tests map directly to same-purpose
fixtures in `crates/compiler/src/tests/crashers/`.
`user_lang_item_declaration_is_rejected` maps to
`crashers/user_lang_item_declaration/`.

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

The retained Rust visibility assertions have same-named module diagnostic
fixtures:

- `public_field_cannot_expose_private_type`
- `public_inherent_method_cannot_expose_private_type`

All package model tests are migrated to same-named directories under
`crates/compiler/src/tests/module_diagnostics/`, including:

- `explicit_aliases_allow_same_declared_package_name`
- `declared_package_name_is_the_default_alias`
- `transitive_public_type_metadata_is_available`

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

The three equivalent enum payload diagnostics map to same-named fixtures and
include matching source positions. The five environment and TAST assertions
remain in Rust and receive additive coverage from
`pipeline/214_internal_type_environment/`.

- `enum_struct_type_arity_mismatch_reports_error`
- `enum_struct_unknown_type_constructor_reports_error`
- `enum_struct_unbound_type_parameter_reports_error`

The following `testing_test.rs` coverage is migrated:

| Rust test | GoML coverage |
| --- | --- |
| `test_build_collects_and_links_top_level_tests` | `bootstrap/cmd/gomlc/main_test.gom::collects_and_encodes_test_descriptors` and `bootstrap-goml/cmd/goml/cli_migration_test.gom::project_test_runs_private_tests_and_ignores_test_sources_in_check` |
| `invalid_test_signatures_are_rejected` | `bootstrap/cmd/gomlc/main_test.gom::collects_all_invalid_test_signature_diagnostics` and `bootstrap-goml/cmd/goml/cli_migration_test.gom::project_test_dry_run_and_invalid_signature_diagnostics` |
| `test_attribute_rejects_non_top_level_functions` | `bootstrap/cmd/gomlc/main_test.gom::collects_all_invalid_test_attribute_diagnostics` |
| `malformed_test_attributes_are_rejected` | `bootstrap/cmd/gomlc/main_test.gom::collects_all_invalid_test_attribute_diagnostics` |
| `duplicate_test_ids_have_cross_file_labels` | `bootstrap/cmd/gomlc/main_test.gom::labels_duplicate_test_ids_across_files` |

`test_link_accepts_multiple_test_package_roots` and
`analysis_compilation_retains_exact_override_sources` remain Rust API
invariants. Their test-runner and source-override behavior is additionally
covered by `bootstrap/go_backend/go_test.gom::emits_test_runner_entrypoint`
and the bootstrap `goml` CLI migration tests.

Two `separate_compile_test.rs` behaviors are covered by stronger module
fixtures:

| Rust test | GoML coverage |
| --- | --- |
| `separate_build_link_matches_project_008` | `module/project008_trait_bounds_across_packages/` |
| `separate_build_link_supports_std` | `module/project032_std_host_api/` |

`user_package_cannot_import_std_internal_host` maps to the same-named module
diagnostic fixture. The remaining two linker artifact assertions stay in Rust
and are additionally covered by bootstrap artifact, link-order, and interface
hash self-tests.

All 44 `query_test.rs` tests intentionally remain in Rust and are excluded
from the migration scope.

## Constructor, reference, try, and tuple migration

All Rust tests in this section remain in place. The GoML fixtures add
end-to-end pipeline and bootstrap coverage.

| Rust test | GoML coverage |
| --- | --- |
| `payload_enum_constructor_values_compile` | `pipeline/209_payload_enum_constructor_value/` |
| `references_typecheck_and_collect` | `pipeline/208_ref_typecheck_and_collect/` |
| `reversed_result_variants_work_with_try` | `pipeline/210_reversed_result_try/` |
| `user_defined_option_variants_work_with_try` | `pipeline/211_user_option_try/` |
| `try_inside_match_while_condition_compiles_in_single_file_mode` | `crashers/while_condition_try_match/` |
| `function_value_tuple_return_can_be_projected_without_annotation` | `pipeline/212_function_value_tuple_projection/` |
| `generic_function_value_tuple_return_can_be_projected_without_annotation` | `pipeline/213_generic_function_value_tuple_projection/` |

The reference test remains an internal Rust assertion because it validates
the exact AST and TAST representation. Its pipeline fixture verifies the
same source-level operations through every emitted IR stage.

## Retained environment and representation assertions

These Rust tests inspect compiler data structures or generated Go text and
remain in Rust. Their GoML mappings provide additive source-level, snapshot,
runtime, or self-host coverage.

| Retained Rust test | Additive GoML coverage |
| --- | --- |
| `inherent_impl_registers_methods` | `pipeline/214_internal_type_environment/` |
| `inherent_impl_instantiates_self_types` | `pipeline/214_internal_type_environment/` |
| `collects_struct_definitions` | `pipeline/214_internal_type_environment/` |
| `enum_variants_record_struct_types` | `pipeline/214_internal_type_environment/` |
| `structs_and_enums_can_reference_each_other` | `pipeline/214_internal_type_environment/` |
| `closure_infers_param_and_return_types` | `pipeline/214_internal_type_environment/` |
| `closure_parameter_annotations_use_enclosing_generics` | `pipeline/214_internal_type_environment/` |
| `env_registers_builtin_function_signatures` | `pipeline/215_builtin_environment_surface/` |
| `env_does_not_register_legacy_int_operator_aliases` | `crashers/legacy_int_operator_aliases_unavailable/` |
| `env_registers_builtin_int_inherent_to_string` | `pipeline/215_builtin_environment_surface/` |
| `env_registers_builtin_int32_inherent_to_string` | `pipeline/214_internal_type_environment/` |
| `env_registers_builtin_vec_inherent_methods` | `pipeline/169_generic_trait_iterator/` and `pipeline/215_builtin_environment_surface/` |
| `env_registers_builtin_ref_inherent_methods` | `pipeline/208_ref_typecheck_and_collect/` |
| `env_registers_builtin_channel_inherent_methods` | `pipeline/206_channel/` and `pipeline/215_builtin_environment_surface/` |
| `env_registers_builtin_slice_inherent_methods` | `pipeline/169_generic_trait_iterator/` and `pipeline/215_builtin_environment_surface/` |
| `env_registers_builtin_iterator_trait_and_fn_iterator_methods` | `pipeline/169_generic_trait_iterator/` |
| `env_registers_builtin_string_inherent_methods` | `pipeline/215_builtin_environment_surface/` |
| `builtin_function_names_include_container_and_iterator_builtins` | `pipeline/165_self_host_builtins/`, `pipeline/169_generic_trait_iterator/`, and `pipeline/215_builtin_environment_surface/` |

| Retained Rust Go representation test | Additive GoML coverage |
| --- | --- |
| `mixed_dyn_vec_push_with_distinct_impls_compiles` | `pipeline/216_mixed_dyn_vec_push/` |
| `implicit_dyn_coercion_from_generic_call_result_compiles` | `pipeline/217_generic_call_dyn_coercion/` |
| `implicit_dyn_coercion_from_generic_enum_call_result_compiles` | `pipeline/218_generic_enum_call_dyn_coercion/` |
| `implicit_dyn_coercion_from_generic_enum_constructor_compiles` | `pipeline/219_generic_enum_constructor_dyn_coercion/` |
| `dyn_trait_types_are_emitted_for_early_return_subexpressions` | `crashers/dyn_trait_type_emission_return_subexpr/` |
| `dyn_trait_types_are_emitted_for_enum_fields_in_early_return_subexpressions` | `pipeline/220_dyn_enum_early_return_type_emission/` |
| `dyn_trait_tuple_types_are_emitted_for_nested_struct_fields_in_early_return_subexpressions` | `crashers/dyn_trait_type_emission_nested_tuple_return_subexpr/` |
| `dyn_trait_tuple_types_are_emitted_for_nested_enum_fields_in_early_return_subexpressions` | `crashers/dyn_trait_type_emission_enum_nested_tuple_return_subexpr/` |
| `dyn_trait_types_are_emitted_for_effect_only_hashmap_set_arguments` | `crashers/dyn_trait_type_emission_hashmap_set_return_subexpr/` |
| `dyn_trait_types_are_emitted_for_hashmap_method_set_arguments` | `crashers/dyn_trait_type_emission_hashmap_method_set_return_subexpr/` |
| `dyn_callable_types_are_emitted_for_early_return_subexpressions` | `crashers/dyn_callable_return_subexpr/` |
| `dyn_callable_tuple_types_are_emitted_for_nested_early_return_subexpressions` | `crashers/dyn_callable_nested_tuple_return_subexpr/` |
| `dyn_callable_types_are_emitted_for_hashmap_set_early_return_subexpressions` | `crashers/dyn_callable_hashmap_set_return_subexpr/` |
| `vec_method_push_mutates_shared_storage_in_go_codegen` | `pipeline/215_builtin_environment_surface/` |
| `complex_generated_names_are_bounded_and_execute` | `pipeline/151_generic_closure_multi_instantiation/` and `bootstrap/go_backend/go_test.gom::hashes_and_compacts_long_go_identifiers` |

## Retained intrinsic, runtime, and executor assertions

| Retained Rust test | Additive GoML coverage |
| --- | --- |
| `callable_ids_have_unique_round_trip_keys` | bootstrap artifact callable serialization and Go runtime-hook self-tests |
| `callable_catalog_signatures_validate_themselves` | bootstrap TAST extern validation and Go runtime-hook emission self-tests |
| `builtin_contract_declares_every_core_callable_once` | `pipeline/215_builtin_environment_surface/` and `bootstrap/go_backend/go_test.gom::lowers_callable_intrinsics` |
| `callable_catalog_rejects_signature_drift` | `bootstrap/tast/tast_test.gom::enforces_source_extern_capabilities` |
| `extern_capabilities_are_partitioned` | `bootstrap/tast/tast_test.gom::enforces_source_extern_capabilities` and `recovers_rejected_externs_with_builtin_signatures` |
| `callable_effects_describe_mutation_and_host_calls` | bootstrap Go backend effect, DCE, and intrinsic lowering tests |
| `every_runtime_hook_has_a_go_implementation` | `bootstrap/go_backend/go_test.gom::emits_every_core_runtime_hook` and `emits_every_standard_runtime_hook` |
| `reference_runtime_executes` | bootstrap pipeline execution, including `pipeline/215_builtin_environment_surface/` |
| `go_run_failure_is_error` | bootstrap process-command failure assertions; Rust helper error propagation remains directly tested in Rust |

## Retained stress, match, and mangling assertions

| Retained Rust test group | Additive GoML coverage |
| --- | --- |
| `wide_call_argument_list_compiles_without_crashing_anf` | `bootstrap/anf/anf_test.gom` plus pipeline ANF snapshots; Rust retains the 1500-argument scale limit |
| `wide_struct_literal_compiles_without_crashing_anf` | bootstrap struct and ANF snapshots; Rust retains the 1500-field scale limit |
| `wide_complex_call_argument_list_compiles_without_crashing_anf` | bootstrap call lowering and ANF snapshots; Rust retains the 500-argument scale limit |
| `wide_match_call_argument_list_compiles_without_crashing_anf` | bootstrap match lowering and ANF snapshots; Rust retains the 600-argument scale limit |
| `very_wide_match_call_argument_list_compiles_without_crashing_anf` | bootstrap match lowering and ANF snapshots; Rust retains the 2000-argument scale limit |
| `wide_or_pattern_compiles_without_exponential_growth` | `pipeline/192_pattern_matching_features/` and `pipeline/193_pattern_matching_invariants/`; Rust retains the complexity guard |
| `wide_if_chain_compiles_without_quadratic_join_resolution` | bootstrap ANF join-point tests; Rust retains the 128-branch complexity guard |
| `deep_struct_pattern_reports_lower_error` | `bootstrap/ast/ast_test.gom::rejects_deeply_nested_patterns_without_overflowing` |
| `deep_parenthesized_expression_reports_depth_error` | `bootstrap/ast/ast_test.gom::rejects_deeply_nested_expressions_without_overflowing` |
| `return_always_exits_control_flow` | bootstrap ANF unreachable-continuation tests |
| `rows_body_ty_ignores_return_only_arms` | `pipeline/201_never_control_flow/` and bootstrap ANF exiting-branch tests |
| `pure_or_patterns_expand_in_the_selected_column` | `pipeline/192_pattern_matching_features/` |
| `or_pattern_columns_expand_lazily` | `pipeline/193_pattern_matching_invariants/` |
| `enum_case_partition_keeps_unmentioned_variants_in_default` | bootstrap ANF enum-match and Go primitive-match tests |
| `valid_name_stays_readable` | bootstrap Go snapshots preserve readable user identifiers |
| `qualified_name_uses_compact_escapes` | `bootstrap/go_backend/go_test.gom::bounds_qualified_variant_names_after_qualification` |
| `compact_escapes_do_not_collide_with_user_text` | generated-name collision crasher fixtures |
| `long_names_are_bounded_and_deterministic` | `bootstrap/go_backend/go_test.gom::hashes_and_compacts_long_go_identifiers` |
| `generated_namespace_is_protected_from_user_names` | generated-name collision crasher fixtures |
| `hashed_names_keep_a_readable_hint` | `pipeline/151_generic_closure_multi_instantiation/` and the bootstrap long-name test |

## Retained testing and linker artifact assertions

| Retained Rust test | Additive GoML coverage |
| --- | --- |
| `test_link_accepts_multiple_test_package_roots` | bootstrap Go test-runner emission and bootstrap `goml test` CLI tests |
| `analysis_compilation_retains_exact_override_sources` | bootstrap `goml` source-override CLI tests |
| `link_rejects_interface_hash_mismatch` | bootstrap artifact interface-hash and dependency-link tests |
| `link_ignores_unreachable_core_inputs` | bootstrap core link ordering and Go dead-code elimination tests |

## Runtime and control-flow migration

The complete `operator_semantics_test.rs` module is migrated. Its diagnostic
and short-circuit cases map to same-named crasher fixtures. The
`numeric_bit_operators_and_casts_execute` case maps to
`e2e/good/0592_numeric_bit_operators_and_casts/`.

The complete `assignment_target_test.rs` module is migrated:

| Rust test | GoML coverage |
| --- | --- |
| `shadowed_ref_get_array_assignment_is_rejected` | `crashers/ref_get_shadow_array_assignment_target/` |
| `intrinsic_ref_get_array_assignment_executes` | `e2e/good/0593_intrinsic_ref_get_array_assignment/` |

The complete `while_expr_test.rs` module maps to crasher fixtures. Three
previously inline cases now have dedicated fixtures:

- `crashers/while_condition_all_exit_match_wrapped_enum/`
- `crashers/while_condition_all_exit_match_wrapped_tuple/`
- `crashers/while_condition_all_exit_match_wrapped_array/`

The complete `multiline_string_test.rs` module is migrated:

| Rust test | GoML coverage |
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
