# bootstrap-goml

`bootstrap-goml` is the minimal GoML implementation of the project build and test driver. The Rust `goml` implementation remains available and is used to create the initial binary.

Build it with the Rust driver:

```sh
cargo run -p goml -- build bootstrap-goml/cmd/goml
```

Use the checked-in GoML compiler implementation:

```sh
bootstrap-goml/_artifact/bin/cmd/goml/goml check bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc
bootstrap-goml/_artifact/bin/cmd/goml/goml build bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc
bootstrap-goml/_artifact/bin/cmd/goml/goml test bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc --jobs 4
```

## 测试

### 自动执行

```sh
just test-bootstrap-goml
```

这个 recipe 做了三件事：

1. `cargo build -p goml -p gomlc` — 用 Rust 编译宿主 goml / gomlc
2. `target/debug/goml build bootstrap-goml/cmd/goml --compiler target/debug/gomlc` — 用宿主 goml 编译 bootstrap CLI
3. `target/debug/goml test bootstrap-goml/cmd/goml --compiler target/debug/gomlc --jobs 1` — 运行全部迁移测试

### 手动分步执行

```sh
# Step 1: 编译宿主工具（只需一次）
cargo build -p goml -p gomlc

# Step 2: 编译 bootstrap CLI
target/debug/goml build bootstrap-goml/cmd/goml --compiler target/debug/gomlc

# Step 3: 运行全部测试
target/debug/goml test bootstrap-goml/cmd/goml \
  --compiler target/debug/gomlc \
  --jobs 1

# 按名称过滤
target/debug/goml test bootstrap-goml/cmd/goml \
  --compiler target/debug/gomlc \
  wide_struct

# 列出所有测试
target/debug/goml test bootstrap-goml/cmd/goml \
  --compiler target/debug/gomlc \
  --list

# JSON 输出
target/debug/goml test bootstrap-goml/cmd/goml \
  --compiler target/debug/gomlc \
  --format json
```

### 环境变量

`test_support/support.gom` 通过以下环境变量定位工具和仓库：

| 变量 | 用途 | 默认值 |
|---|---|---|
| `GOML_TEST_GOML` | goml 二进制路径 | `_artifact/bin/cmd/goml/goml` |
| `GOML_TEST_GOMLC` | gomlc 二进制路径 | `../../target/debug/gomlc`（相对仓库根） |
| `GOML_REPO` | 仓库根目录 | `module_root()/..` |

### 测试基础设施

`test_support/support.gom` 提供：

- `run(args, directory)` — 执行 goml 子进程
- `project_command(command, target, extra, directory)` — 执行 goml 命令（自动注入 `--compiler`）
- `workspace(name)` — 创建隔离工作目录 `_artifact/test-work/<name>`
- `write_file(path, content)` / `read_file(path)` — 文件读写
- `create_dir(path)` — 目录创建
- `assert_success(output)` / `assert_failure(output)` — 退出码断言
- `assert_contains(text, expected)` / `assert_not_contains(text, unexpected)` — 字符串断言
- `stdout(output)` / `stderr(output)` — 输出捕获
- `repository_root()` / `module_root()` — 路径定位

测试文件在 `cmd/goml/cli_migration_test.gom`，测试输出写入 `_artifact/test-work/<case>/`。

### `--compiler` 参数

`goml` 是构建编排器，`gomlc` 是编译器后端。`goml` 的 check / build / test 都会
fork 调用 `gomlc` 做实际编译，`--compiler` 告诉 goml 去哪个路径找 gomlc。

不传 `--compiler` 时的查找顺序：
1. `GOMLC` 环境变量
2. 与 goml 同级目录下的 `gomlc`
3. `$GOML_HOME/bin/gomlc`
4. `PATH` 中的 `gomlc`

### 自举测试（bootstrap 自己测试自己）

完整的自举测试链：bootstrap 版 goml 驱动 bootstrap 版 gomlc 来编译和测试
它们自己的源码。

**前提**：bootstrap/gomlc 和 bootstrap-goml/cmd/goml 两个二进制已存在
（由 `just test-bootstrap-goml` 或 `just install-bootstrap` 产生）。

```sh
# 两条捷径
just test-bootstrap-self          # check + test（自举验证）
just test-bootstrap-self-full     # check + build + test（完整自举）
```

手动分步：

```sh
GOML=./bootstrap-goml/_artifact/bin/cmd/goml/goml
GOMLC=./bootstrap/_artifact/bin/cmd/gomlc/gomlc

# Step 1: bootstrap goml check 自己
$GOML check bootstrap-goml/cmd/goml --compiler $GOMLC

# Step 2: bootstrap goml build 自己
$GOML build bootstrap-goml/cmd/goml --compiler $GOMLC

# Step 3: bootstrap goml test 自己
$GOML test bootstrap-goml/cmd/goml --compiler $GOMLC --jobs 1
```

### 测试层级总览

```
                  ┌─────────────────────────────────┐
                  │   just test-bootstrap-goml       │  ← Rust goml 驱动
                  │   (rust goml + rust gomlc)       │
                  └──────────────┬──────────────────┘
                                 │ 产出
                                 ▼
                  ┌─────────────────────────────────┐
                  │  just test-bootstrap-self        │  ← bootstrap goml 驱动
                  │  (bootstrap goml + bootstrap gomlc) │   自己测试自己
                  └─────────────────────────────────┘
```

The current implementation provides deterministic local package discovery, topological check/build/link plans, executable runs, internal test discovery, filtering, ignored tests, text or JSON test events, and a bounded `Channel` worker pool.

Registry dependencies, external black-box tests, incremental fingerprints, test timeouts, and package-management commands remain in the Rust driver for now.
