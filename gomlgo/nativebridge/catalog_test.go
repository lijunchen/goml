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
		{"context", "Background", "", false},
		{"context", "WithCancel", "", false},
		{"fmt", "Fprint", "", false},
		{"fmt", "Print", "", false},
		{"fmt", "Println", "", false},
		{"fmt", "Sprintf", "", false},
		{"io/fs", "Name", "DirEntry", false},
		{"os", "MkdirAll", "", false},
		{"os", "ReadDir", "", false},
		{"os", "ReadFile", "", false},
		{"os", "Stat", "", false},
		{"os", "WriteFile", "", false},
		{"reflect", "Pointer", "Value", false},
		{"reflect", "ValueOf", "", false},
		{"runtime", "Gosched", "", false},
		{"runtime", "Stack", "", false},
		{"strconv", "FormatFloat", "", false},
		{"strconv", "ParseFloat", "", false},
		{"strings", "Cut", "", false},
		{"strings", "Join", "", false},
		{"strings", "ToUpper", "", false},
		{"sync", "Broadcast", "Cond", true},
		{"sync", "Wait", "Cond", true},
		{"sync", "Lock", "Mutex", true},
		{"sync", "Unlock", "Mutex", true},
		{"sync", "Add", "WaitGroup", true},
		{"sync", "Done", "WaitGroup", true},
		{"sync", "Wait", "WaitGroup", true},
		{"sync", "NewCond", "", false},
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

func TestCatalogCoversPipelineTypesAndGlobals(t *testing.T) {
	types := []struct {
		path string
		name string
	}{
		{"os", "File"},
		{"os", "LinkError"},
		{"os", "ProcAttr"},
		{"os", "Process"},
		{"os", "ProcessState"},
		{"os", "Root"},
		{"os", "SyscallError"},
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
	if !CatalogRegisterGlobal(1, "os", "Stderr") {
		t.Fatal("catalog rejected os.Stderr")
	}
	if CatalogRegisterGlobal(2, "os", "Unsupported") {
		t.Fatal("catalog accepted an unsupported global")
	}
	for _, importPath := range []string{
		"context", "fmt", "io", "io/fs", "os", "reflect", "runtime", "slices", "strconv", "strings", "sync",
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
	if CatalogRegisterProxy("io", "Reader") {
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
