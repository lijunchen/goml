import hashlib
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Case:
    src_abs: Path
    src_rel: Path
    sha256: str
    short_hash: str
    group: str
    case_slug: str


def sha256_hex(data: bytes) -> str:
    h = hashlib.sha256()
    h.update(data)
    return h.hexdigest()


def read_bytes(path: Path) -> bytes:
    with path.open("rb") as f:
        return f.read()


def group_for_top(top: str) -> str:
    if top.startswith("goml_bug"):
        return "bug"
    if top.startswith("goml_test"):
        return "goml_test"
    if top.startswith("test"):
        return "test"
    if top.startswith("goml_lsp"):
        return "lsp"
    return "misc"


def slugify_relpath(rel: Path) -> str:
    s = rel.as_posix()
    s = re.sub(r"[^A-Za-z0-9]+", "_", s).strip("_")
    return s or "case"


def collect_existing_hashes(e2e_root: Path) -> set[str]:
    hashes: set[str] = set()
    for p in e2e_root.rglob("*.gom"):
        try:
            hashes.add(sha256_hex(read_bytes(p)))
        except OSError:
            continue
    return hashes


def run_single_exit_code(path: Path) -> int:
    cmd = ["cargo", "run", "-q", "-p", "goml", "--", "compiler", "run-single", str(path)]
    proc = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return proc.returncode


def unique_dir(base: Path) -> Path:
    if not base.exists():
        return base
    i = 2
    while True:
        p = base.with_name(f"{base.name}_{i}")
        if not p.exists():
            return p
        i += 1


def main() -> int:
    repo_root = Path(__file__).resolve().parents[5]
    e2e_root = repo_root / "crates" / "compiler" / "src" / "tests" / "e2e"
    src_root = Path("/home/li/git/goml-tests")
    tmp_root = e2e_root / "_migrate_tmp"

    if not src_root.exists():
        print(f"missing source dir: {src_root}", file=sys.stderr)
        return 2

    existing_hashes = collect_existing_hashes(e2e_root)

    src_files = sorted([p for p in src_root.rglob("*.gom") if p.is_file()])
    total_src = len(src_files)

    skipped_existing = 0
    skipped_dup = 0
    selected: list[Case] = []
    seen_new: dict[str, Path] = {}

    for p in src_files:
        rel = p.relative_to(src_root)
        data = read_bytes(p)
        h = sha256_hex(data)
        if h in existing_hashes:
            skipped_existing += 1
            continue
        if h in seen_new:
            skipped_dup += 1
            continue

        top = rel.parts[0] if rel.parts else ""
        group = group_for_top(top)
        base = slugify_relpath(rel.with_suffix(""))
        short_hash = h[:8]
        case_slug = f"{base}__{short_hash}"
        selected.append(
            Case(
                src_abs=p,
                src_rel=rel,
                sha256=h,
                short_hash=short_hash,
                group=group,
                case_slug=case_slug,
            )
        )
        seen_new[h] = rel

    tmp_root.mkdir(parents=True, exist_ok=True)

    good = 0
    bad = 0
    added = 0
    failures: list[str] = []

    for c in selected:
        tmp_case_dir = tmp_root / c.case_slug
        if tmp_case_dir.exists():
            shutil.rmtree(tmp_case_dir)
        tmp_case_dir.mkdir(parents=True, exist_ok=True)
        tmp_main = tmp_case_dir / "main.gom"
        tmp_main.write_bytes(read_bytes(c.src_abs))

        code = run_single_exit_code(tmp_main)
        bucket = "good" if code == 0 else "bad"
        if bucket == "good":
            good += 1
        else:
            bad += 1

        dest_dir = e2e_root / bucket / "migrated" / c.group / c.case_slug
        dest_dir = unique_dir(dest_dir)
        try:
            dest_dir.mkdir(parents=True, exist_ok=False)
            (dest_dir / "main.gom").write_bytes(tmp_main.read_bytes())
            (dest_dir / "main.gom.out").write_text("", encoding="utf-8")
            added += 1
        except Exception as e:
            failures.append(f"{c.src_rel.as_posix()}: {e}")

    shutil.rmtree(tmp_root, ignore_errors=True)

    print("migration_stats:")
    print(f"  source_total_gom: {total_src}")
    print(f"  skipped_existing_e2e_dedup: {skipped_existing}")
    print(f"  skipped_source_dedup: {skipped_dup}")
    print(f"  selected_after_dedup: {len(selected)}")
    print(f"  added: {added}")
    print(f"  classified_good: {good}")
    print(f"  classified_bad: {bad}")
    if failures:
        print(f"  failures: {len(failures)}")
        for s in failures[:20]:
            print(f"    - {s}")
        if len(failures) > 20:
            print("    - ...")

    if failures:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
