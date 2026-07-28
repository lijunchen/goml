# Releasing goml

Releases use strict `vX.Y.Z` tags and currently publish Linux amd64 binaries.

The root `VERSION` file is authoritative. `goml`, `gomlc`, `gomllsp`, and the VS Code extension must use the same version.

## Version policy

Each release must be one continuous SemVer step from the latest published release:

- patch: `0.1.0` to `0.1.1`
- minor: `0.1.0` to `0.2.0`
- major: `0.1.0` to `1.0.0`

Skipped steps such as `0.1.0` to `0.1.2` or `0.3.0` are rejected.

## Publish

Set the next version:

```sh
just set-version 0.1.1
just ci
```

Commit and push the version change, then create the tag:

```sh
git tag -a v0.1.1 -m "goml v0.1.1"
git push origin v0.1.1
```

The Release workflow verifies the version, the previous release, stage0, the fixed point, the complete test suite, and the extracted release archive before publishing.

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
