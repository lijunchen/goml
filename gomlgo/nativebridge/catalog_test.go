package nativebridge

import (
	"io"
	"reflect"
	"sync"
	"testing"
)

func TestCatalogCoversPipelineFunctions(t *testing.T) {
	functions := []struct {
		path     string
		symbol   string
		receiver string
		pointer  bool
	}{
		{"bytes", "Bytes", "Buffer", true},
		{"bytes", "NewReader", "", false},
		{"context", "Background", "", false},
		{"context", "Done", "Context", false},
		{"context", "Err", "Context", false},
		{"context", "WithCancel", "", false},
		{"context", "WithTimeout", "", false},
		{"crypto/sha256", "New", "", false},
		{"errors", "Is", "", false},
		{"fmt", "Fprint", "", false},
		{"fmt", "Print", "", false},
		{"fmt", "Println", "", false},
		{"fmt", "Sprintf", "", false},
		{"hash", "Sum", "Hash", false},
		{"io", "Copy", "", false},
		{"io/fs", "Name", "DirEntry", false},
		{"io/fs", "IsDir", "FileInfo", false},
		{"io/fs", "Mode", "FileInfo", false},
		{"io/fs", "IsRegular", "FileMode", false},
		{"os", "Close", "File", true},
		{"os", "Environ", "", false},
		{"os", "Executable", "", false},
		{"os", "ExitCode", "ProcessState", true},
		{"os", "Getwd", "", false},
		{"os", "IsExist", "", false},
		{"os", "IsNotExist", "", false},
		{"os", "IsPermission", "", false},
		{"os", "IsTimeout", "", false},
		{"os", "Link", "", false},
		{"os", "LookupEnv", "", false},
		{"os", "Mkdir", "", false},
		{"os", "MkdirAll", "", false},
		{"os", "Open", "", false},
		{"os", "ReadDir", "", false},
		{"os", "ReadFile", "", false},
		{"os", "Remove", "", false},
		{"os", "RemoveAll", "", false},
		{"os", "Rename", "", false},
		{"os", "Stat", "", false},
		{"os", "Symlink", "", false},
		{"os", "WriteFile", "", false},
		{"os/exec", "Command", "", false},
		{"os/exec", "CommandContext", "", false},
		{"os/exec", "LookPath", "", false},
		{"os/exec", "Run", "Cmd", true},
		{"path/filepath", "Abs", "", false},
		{"path/filepath", "Base", "", false},
		{"path/filepath", "Clean", "", false},
		{"path/filepath", "Dir", "", false},
		{"path/filepath", "EvalSymlinks", "", false},
		{"path/filepath", "Ext", "", false},
		{"path/filepath", "IsAbs", "", false},
		{"path/filepath", "Join", "", false},
		{"reflect", "Pointer", "Value", false},
		{"reflect", "ValueOf", "", false},
		{"runtime", "Gosched", "", false},
		{"runtime", "Stack", "", false},
		{"strconv", "FormatFloat", "", false},
		{"strconv", "ParseFloat", "", false},
		{"strings", "Cut", "", false},
		{"strings", "Join", "", false},
		{"strings", "TrimPrefix", "", false},
		{"strings", "TrimSuffix", "", false},
		{"strings", "ToUpper", "", false},
		{"sync", "Broadcast", "Cond", true},
		{"sync", "Wait", "Cond", true},
		{"sync", "Lock", "Locker", false},
		{"sync", "Unlock", "Locker", false},
		{"sync", "Lock", "Mutex", true},
		{"sync", "Unlock", "Mutex", true},
		{"sync", "Add", "WaitGroup", true},
		{"sync", "Done", "WaitGroup", true},
		{"sync", "Wait", "WaitGroup", true},
		{"sync", "NewCond", "", false},
		{"syscall", "Kill", "", false},
		{"time", "Now", "", false},
		{"time", "UnixNano", "Time", false},
	}
	ResetCatalog()
	for index, function := range functions {
		if !CatalogBind(
			uint32(index),
			function.path,
			function.symbol,
			function.receiver,
			function.pointer,
			"",
			false,
		) {
			t.Fatalf("catalog rejected %s.%s.%s", function.path, function.receiver, function.symbol)
		}
	}
	if !CatalogBind(uint32(len(functions)), "os", "Exit", "", false, "", true) {
		t.Fatal("catalog rejected os.Exit")
	}
	if CatalogBind(uint32(len(functions)+1), "slices", "Grow", "", false, "[]int\nint", false) {
		t.Fatal("catalog accepted a generic function")
	}
	if CatalogBind(0, "fmt", "Print", "", false, "", false) {
		t.Fatal("catalog accepted a duplicate call ID")
	}
}

func TestProgramExecutableOverride(t *testing.T) {
	SetProgramExecutable("/toolchain/bin/gomlc")
	t.Cleanup(func() { SetProgramExecutable("") })
	value, err := ProgramExecutable()
	if err != nil {
		t.Fatal(err)
	}
	if value != "/toolchain/bin/gomlc" {
		t.Fatalf("program executable = %q", value)
	}
}

func TestCatalogCoversPipelineTypesAndGlobals(t *testing.T) {
	types := []struct {
		path string
		name string
	}{
		{"bytes", "Buffer"},
		{"bytes", "Reader"},
		{"os", "File"},
		{"os", "LinkError"},
		{"os", "PathError"},
		{"os", "ProcAttr"},
		{"os", "Process"},
		{"os", "ProcessState"},
		{"os", "Root"},
		{"os", "SyscallError"},
		{"os/exec", "Cmd"},
		{"reflect", "MapIter"},
		{"reflect", "Method"},
		{"reflect", "SelectCase"},
		{"reflect", "SliceHeader"},
		{"reflect", "StringHeader"},
		{"reflect", "StructField"},
		{"reflect", "Value"},
		{"reflect", "ValueError"},
		{"runtime", "BlockProfileRecord"},
		{"runtime", "Cleanup"},
		{"runtime", "Frame"},
		{"runtime", "Frames"},
		{"runtime", "Func"},
		{"runtime", "MemProfileRecord"},
		{"runtime", "MemStats"},
		{"runtime", "PanicNilError"},
		{"runtime", "Pinner"},
		{"runtime", "StackRecord"},
		{"runtime", "TypeAssertionError"},
		{"strconv", "NumError"},
		{"strings", "Builder"},
		{"strings", "Reader"},
		{"strings", "Replacer"},
		{"sync", "Cond"},
		{"sync", "Map"},
		{"sync", "Mutex"},
		{"sync", "Once"},
		{"sync", "Pool"},
		{"sync", "RWMutex"},
		{"sync", "WaitGroup"},
		{"syscall", "SysProcAttr"},
		{"time", "Time"},
	}
	ResetCatalog()
	for _, typeValue := range types {
		key := "native:" + typeValue.path + "." + typeValue.name
		if !CatalogRegisterType(key, typeValue.path, typeValue.name) {
			t.Fatalf("catalog rejected %s", key)
		}
	}
	if CatalogRegisterType("native:net.IP", "net", "IP") {
		t.Fatal("catalog accepted an unsupported type")
	}
	globals := []struct {
		path string
		name string
	}{
		{"context", "DeadlineExceeded"},
		{"io", "EOF"},
		{"io", "ErrShortWrite"},
		{"io", "ErrUnexpectedEOF"},
		{"os", "Stderr"},
	}
	for index, global := range globals {
		if !CatalogRegisterGlobal(index, global.path, global.name) {
			t.Fatalf("catalog rejected %s.%s", global.path, global.name)
		}
	}
	if CatalogRegisterGlobal(len(globals), "os", "Unsupported") {
		t.Fatal("catalog accepted an unsupported global")
	}
	for _, importPath := range []string{
		"bytes", "context", "crypto/sha256", "errors", "fmt", "hash", "io", "io/fs", "os", "os/exec", "path/filepath", "reflect", "runtime", "slices", "strconv", "strings", "sync", "syscall", "time",
	} {
		if !CatalogSupportsPackage(importPath) {
			t.Fatalf("catalog rejected package %s", importPath)
		}
	}
	if CatalogSupportsPackage("net") {
		t.Fatal("catalog accepted package net")
	}
}

func TestCatalogInstallsCallableRegistry(t *testing.T) {
	ResetCatalog()
	if !CatalogBind(4, "strings", "ToUpper", "", false, "", false) {
		t.Fatal("catalog rejected strings.ToUpper")
	}
	result := DefaultCall(4, Values("goml"))
	assertSuccessful(t, result)
	if got := result.Values[0].String(); got != "GOML" {
		t.Fatalf("result = %q, want GOML", got)
	}
}

func TestCatalogBuildsAggregateTypes(t *testing.T) {
	ResetCatalog()
	if !CatalogRegisterAggregate("pair", "string\n[]int32", "json:\"name\"\n") {
		t.Fatal("catalog rejected aggregate")
	}
	value := NewAggregateValue("pair")
	if !ValidValue(value) {
		t.Fatal("catalog aggregate is invalid")
	}
	if !SetAggregateValue(value, 0, NewStringValue("goml")) {
		t.Fatal("catalog aggregate string field is not settable")
	}
	if CatalogRegisterAggregate("invalid", "unsupported", "") {
		t.Fatal("catalog accepted an unsupported aggregate field")
	}
}

func TestCatalogPointerIdentityIsStable(t *testing.T) {
	ResetCatalog()
	first := NewPointerIdentity(3, "f1;")
	same := NewPointerIdentity(3, "f1;")
	different := NewPointerIdentity(3, "f2;")
	if ValueText(first) != ValueText(same) {
		t.Fatal("matching pointer identities differ")
	}
	firstPointer := first.(ValueRef).Pointer()
	if firstPointer != same.(ValueRef).Pointer() {
		t.Fatal("matching pointer addresses differ")
	}
	if firstPointer == different.(ValueRef).Pointer() {
		t.Fatal("different pointer addresses match")
	}
}

func TestCatalogInstallsLockerProxy(t *testing.T) {
	ResetCatalog()
	if !CatalogRegisterProxy("sync", "Locker") {
		t.Fatal("catalog rejected sync.Locker")
	}
	calls := make([]string, 0, 2)
	token, err := NewCallbackToken(nil, func(invocation CallbackInvocation) CallResult {
		calls = append(calls, invocation.Method)
		return CallResult{}
	})
	if err != nil {
		t.Fatal(err)
	}
	value, err := MakeInterfaceProxy(reflect.TypeFor[sync.Locker](), token)
	if err != nil {
		t.Fatal(err)
	}
	locker := value.Interface().(sync.Locker)
	locker.Lock()
	locker.Unlock()
	if !reflect.DeepEqual(calls, []string{"Lock", "Unlock"}) {
		t.Fatalf("calls = %v", calls)
	}
	if CatalogRegisterProxy("io", "Closer") {
		t.Fatal("catalog accepted an unsupported proxy")
	}
}

func TestCatalogInstallsWriterProxy(t *testing.T) {
	ResetCatalog()
	if !CatalogRegisterProxy("io", "Writer") {
		t.Fatal("catalog rejected io.Writer")
	}
	token, err := NewCallbackToken(nil, func(invocation CallbackInvocation) CallResult {
		if invocation.Method != "Write" || string(invocation.Arguments[0].Bytes()) != "goml" {
			t.Fatalf("invocation = %+v", invocation)
		}
		return CallResult{Values: []ValueRef{
			reflect.ValueOf(4),
			reflect.Zero(reflect.TypeFor[error]()),
		}}
	})
	if err != nil {
		t.Fatal(err)
	}
	value, err := MakeInterfaceProxy(reflect.TypeFor[io.Writer](), token)
	if err != nil {
		t.Fatal(err)
	}
	written, writeErr := value.Interface().(io.Writer).Write([]byte("goml"))
	if written != 4 || writeErr != nil {
		t.Fatalf("write = %d, %v", written, writeErr)
	}
}

func TestCatalogInstallsReaderProxy(t *testing.T) {
	ResetCatalog()
	if !CatalogRegisterProxy("io", "Reader") {
		t.Fatal("catalog rejected io.Reader")
	}
	token, err := NewCallbackToken(nil, func(invocation CallbackInvocation) CallResult {
		if invocation.Method != "Read" || len(invocation.Arguments[0].Bytes()) != 4 {
			t.Fatalf("invocation = %+v", invocation)
		}
		copy(invocation.Arguments[0].Bytes(), "goml")
		return CallResult{Values: []ValueRef{
			reflect.ValueOf(4),
			reflect.Zero(reflect.TypeFor[error]()),
		}}
	})
	if err != nil {
		t.Fatal(err)
	}
	value, err := MakeInterfaceProxy(reflect.TypeFor[io.Reader](), token)
	if err != nil {
		t.Fatal(err)
	}
	buffer := make([]byte, 4)
	read, readErr := value.Interface().(io.Reader).Read(buffer)
	if read != 4 || readErr != nil || string(buffer) != "goml" {
		t.Fatalf("read = %d, %v, %q", read, readErr, buffer)
	}
}
