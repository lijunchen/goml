# Releasing goml

Releases use strict `vX.Y.Z` tags and currently publish Linux amd64 binaries.

The root `VERSION` file is authoritative. `goml`, `gomlc`, `gomllsp`, and the VS Code extension must use the same version.

## Version policy

Each release must be one continuous SemVer step from the latest published release:

- patch: `0.1.0` to `0.1.1`
- minor: `0.1.0` to `0.2.0`
- major: `0.1.0` to `1.0.0`

Skipped steps such as `0.1.0` to `0.1.2` or `0.3.0` are rejected.

During the early bootstrap period, releases are limited to continuous `0.1.x` patch versions.

## Publish

Set the next version:

```sh
just set-version 0.1.1
just ci
```

Commit and push the version change, then wait for the main branch CI to succeed before creating the tag:

```sh
release_sha="$(git rev-parse HEAD)"
git push origin main
ci_run_id="$(gh run list --repo lijunchen/goml --workflow CI --branch main --commit "$release_sha" --event push --limit 1 --json databaseId --jq '.[0].databaseId')"
gh run watch "$ci_run_id" --repo lijunchen/goml --exit-status
git tag -a v0.1.1 -m "goml v0.1.1"
git push origin v0.1.1
```

The main branch CI verifies the stage2/stage3 fixed point and complete test suite. The Release workflow requires a successful main branch CI for the tagged commit, verifies the version, previous release, and stage0, rebuilds stage2, and tests the extracted release archive before publishing.

Release archives use a complete toolchain prefix:

```text
goml-X.Y.Z-linux-amd64/
├── bin/
│   ├── goml
│   ├── gomlc
│   ├── gomlfmt
│   └── gomllsp
└── lib/
    ├── builtin/
    │   ├── contract.gom
    │   ├── derive.gom
    │   └── numeric.gom
    ├── prelude/
    │   └── prelude.gom
    └── std/
```

The compiler resolves `lib` relative to its executable. The archive must preserve this layout. During the layout transition release, the archive also includes the four legacy flat files `builtin_contract.gom`, `builtin_prelude.gom`, `builtin_numeric.gom`, and `builtin_derive.gom` so the previous compiler can use the new archive.

## Advance stage0

The release is built by the previous release as stage0. After publishing, read the new archive checksum from its `SHA256SUMS`, then advance stage0:

```sh
just set-bootstrap-stage0 0.1.1 <sha256>
just bootstrap
```

Commit the updated `bootstrap/stage0.env` before using language features that the previous stage0 cannot compile. The next release workflow requires stage0 to match the latest published release.

For an offline bootstrap, download the pinned archive and run:

```sh
GOML_STAGE0_ARCHIVE=/path/to/goml-X.Y.Z-linux-amd64.tar.gz just bootstrap
```
