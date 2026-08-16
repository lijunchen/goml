package nativebridge

import (
	"context"
	"fmt"
	"io"
	"io/fs"
	"os"
	"reflect"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"
)

type catalogPointer struct {
	value byte
}

type catalogLockerProxy struct {
	token *CallbackToken
}

type catalogWriterProxy struct {
	token *CallbackToken
}

var catalogPointers struct {
	sync.Mutex
	values map[string]*catalogPointer
}

func ResetCatalog() {
	defaultRegistry.Lock()
	defaultRegistry.value = &Registry{functions: make(map[CallID]reflect.Value)}
	defaultRegistry.Unlock()
	nativeGlobals.Lock()
	nativeGlobals.values = nil
	nativeGlobals.Unlock()
	nativeTypes.Lock()
	nativeTypes.values = nil
	nativeTypes.Unlock()
	aggregateTypes.Lock()
	aggregateTypes.values = nil
	aggregateTypes.Unlock()
	interfaceProxies.Lock()
	interfaceProxies.values = nil
	interfaceProxies.Unlock()
	catalogPointers.Lock()
	catalogPointers.values = nil
	catalogPointers.Unlock()
}

func NewPointerIdentity(root int, projection string) any {
	if root < 0 {
		return reflect.ValueOf((*catalogPointer)(nil))
	}
	key := strconv.Itoa(root) + ":" + projection
	catalogPointers.Lock()
	defer catalogPointers.Unlock()
	if catalogPointers.values == nil {
		catalogPointers.values = make(map[string]*catalogPointer)
	}
	value := catalogPointers.values[key]
	if value == nil {
		value = &catalogPointer{}
		catalogPointers.values[key] = value
	}
	return reflect.ValueOf(value)
}

func (value *catalogLockerProxy) Lock() {
	InvokeCallback(value.token, "Lock", nil, nil)
}

func (value *catalogLockerProxy) Unlock() {
	InvokeCallback(value.token, "Unlock", nil, nil)
}

func (value *catalogWriterProxy) Write(buffer []byte) (int, error) {
	results := InvokeCallback(
		value.token,
		"Write",
		Values(buffer),
		[]reflect.Type{reflect.TypeFor[int](), reflect.TypeFor[error]()},
	)
	return CallbackValue[int](results, 0), CallbackValue[error](results, 1)
}

func CatalogRegisterProxy(importPath string, name string) bool {
	switch importPath + "\x00" + name {
	case "io\x00Writer":
		return RegisterInterfaceProxy((*io.Writer)(nil), func(token *CallbackToken) (ValueRef, error) {
			return reflect.ValueOf(&catalogWriterProxy{token: token}), nil
		}) == nil
	case "sync\x00Locker":
		return RegisterInterfaceProxy((*sync.Locker)(nil), func(token *CallbackToken) (ValueRef, error) {
			return reflect.ValueOf(&catalogLockerProxy{token: token}), nil
		}) == nil
	default:
		return false
	}
}

func CatalogSupportsPackage(importPath string) bool {
	switch importPath {
	case "context", "fmt", "io", "io/fs", "os", "reflect", "runtime", "slices", "strconv", "strings", "sync", "time":
		return true
	default:
		return false
	}
}

func CatalogBind(
	id uint32,
	importPath string,
	symbol string,
	receiverName string,
	receiverPointer bool,
	typeArguments string,
	exit bool,
) bool {
	if typeArguments != "" {
		return false
	}
	var function any
	if exit {
		if importPath != "os" || symbol != "Exit" || receiverName != "" {
			return false
		}
		function = exitAdapter
	} else if receiverName == "" {
		function = catalogFunction(importPath, symbol)
	} else {
		function = catalogMethod(importPath, receiverName, receiverPointer, symbol)
	}
	if function == nil {
		return false
	}
	value := reflect.ValueOf(function)
	defaultRegistry.Lock()
	defer defaultRegistry.Unlock()
	if defaultRegistry.value == nil {
		defaultRegistry.value = &Registry{functions: make(map[CallID]reflect.Value)}
	}
	if defaultRegistry.value.functions == nil {
		defaultRegistry.value.functions = make(map[CallID]reflect.Value)
	}
	callID := CallID(id)
	if _, exists := defaultRegistry.value.functions[callID]; exists {
		return false
	}
	defaultRegistry.value.functions[callID] = value
	return true
}

func catalogFunction(importPath string, symbol string) any {
	switch importPath + "\x00" + symbol {
	case "context\x00Background":
		return context.Background
	case "context\x00WithCancel":
		return context.WithCancel
	case "fmt\x00Fprint":
		return fmt.Fprint
	case "fmt\x00Print":
		return fmt.Print
	case "fmt\x00Println":
		return fmt.Println
	case "fmt\x00Sprintf":
		return fmt.Sprintf
	case "os\x00MkdirAll":
		return os.MkdirAll
	case "os\x00ReadDir":
		return os.ReadDir
	case "os\x00ReadFile":
		return os.ReadFile
	case "os\x00Stat":
		return os.Stat
	case "os\x00WriteFile":
		return os.WriteFile
	case "reflect\x00ValueOf":
		return reflect.ValueOf
	case "runtime\x00Gosched":
		return runtime.Gosched
	case "runtime\x00Stack":
		return runtime.Stack
	case "strconv\x00FormatFloat":
		return strconv.FormatFloat
	case "strconv\x00ParseFloat":
		return strconv.ParseFloat
	case "strings\x00Cut":
		return strings.Cut
	case "strings\x00Join":
		return strings.Join
	case "strings\x00ToUpper":
		return strings.ToUpper
	case "sync\x00NewCond":
		return sync.NewCond
	default:
		return nil
	}
}

func catalogMethod(importPath string, receiverName string, receiverPointer bool, symbol string) any {
	key := importPath + "\x00" + receiverName + "\x00" + symbol
	switch key {
	case "io/fs\x00DirEntry\x00Name":
		if !receiverPointer {
			return fs.DirEntry.Name
		}
	case "reflect\x00Value\x00Pointer":
		if !receiverPointer {
			return reflect.Value.Pointer
		}
	case "sync\x00Cond\x00Broadcast":
		if receiverPointer {
			return (*sync.Cond).Broadcast
		}
	case "sync\x00Cond\x00Wait":
		if receiverPointer {
			return (*sync.Cond).Wait
		}
	case "sync\x00Locker\x00Lock":
		if !receiverPointer {
			return sync.Locker.Lock
		}
	case "sync\x00Locker\x00Unlock":
		if !receiverPointer {
			return sync.Locker.Unlock
		}
	case "sync\x00Mutex\x00Lock":
		if receiverPointer {
			return (*sync.Mutex).Lock
		}
	case "sync\x00Mutex\x00Unlock":
		if receiverPointer {
			return (*sync.Mutex).Unlock
		}
	case "sync\x00WaitGroup\x00Add":
		if receiverPointer {
			return (*sync.WaitGroup).Add
		}
	case "sync\x00WaitGroup\x00Done":
		if receiverPointer {
			return (*sync.WaitGroup).Done
		}
	case "sync\x00WaitGroup\x00Wait":
		if receiverPointer {
			return (*sync.WaitGroup).Wait
		}
	}
	return nil
}

func CatalogRegisterType(key string, importPath string, name string) bool {
	prototype := catalogType(importPath, name)
	if prototype == nil {
		return false
	}
	return RegisterNativeType(key, prototype) == nil
}

func CatalogRegisterAggregate(key string, typeKeys string, tags string) bool {
	typeValues := strings.Split(typeKeys, "\n")
	tagValues := strings.Split(tags, "\n")
	if len(typeValues) == 0 || len(typeValues) != len(tagValues) {
		return false
	}
	fields := make([]reflect.StructField, len(typeValues))
	for index, typeKey := range typeValues {
		typeValue, err := typeForKey(typeKey)
		if err != nil {
			return false
		}
		fields[index] = reflect.StructField{
			Name: fmt.Sprintf("F%d", index),
			Type: typeValue,
			Tag:  reflect.StructTag(tagValues[index]),
		}
	}
	typeValue := reflect.StructOf(fields)
	return RegisterAggregateType(key, reflect.Zero(typeValue).Interface()) == nil
}

func catalogType(importPath string, name string) any {
	switch importPath + "\x00" + name {
	case "os\x00File":
		return os.File{}
	case "os\x00LinkError":
		return os.LinkError{}
	case "os\x00ProcAttr":
		return os.ProcAttr{}
	case "os\x00Process":
		return os.Process{}
	case "os\x00ProcessState":
		return os.ProcessState{}
	case "os\x00Root":
		return os.Root{}
	case "os\x00SyscallError":
		return os.SyscallError{}
	case "reflect\x00MapIter":
		return reflect.MapIter{}
	case "reflect\x00Method":
		return reflect.Method{}
	case "reflect\x00SelectCase":
		return reflect.SelectCase{}
	case "reflect\x00SliceHeader":
		return reflect.SliceHeader{}
	case "reflect\x00StringHeader":
		return reflect.StringHeader{}
	case "reflect\x00StructField":
		return reflect.StructField{}
	case "reflect\x00Value":
		return reflect.Value{}
	case "reflect\x00ValueError":
		return reflect.ValueError{}
	case "runtime\x00BlockProfileRecord":
		return runtime.BlockProfileRecord{}
	case "runtime\x00Cleanup":
		return runtime.Cleanup{}
	case "runtime\x00Frame":
		return runtime.Frame{}
	case "runtime\x00Frames":
		return runtime.Frames{}
	case "runtime\x00Func":
		return runtime.Func{}
	case "runtime\x00MemProfileRecord":
		return runtime.MemProfileRecord{}
	case "runtime\x00MemStats":
		return runtime.MemStats{}
	case "runtime\x00PanicNilError":
		return runtime.PanicNilError{}
	case "runtime\x00Pinner":
		return runtime.Pinner{}
	case "runtime\x00StackRecord":
		return runtime.StackRecord{}
	case "runtime\x00TypeAssertionError":
		return runtime.TypeAssertionError{}
	case "strconv\x00NumError":
		return strconv.NumError{}
	case "strings\x00Builder":
		return strings.Builder{}
	case "strings\x00Reader":
		return strings.Reader{}
	case "strings\x00Replacer":
		return strings.Replacer{}
	case "sync\x00Cond":
		return sync.Cond{}
	case "sync\x00Map":
		return sync.Map{}
	case "sync\x00Mutex":
		return sync.Mutex{}
	case "sync\x00Once":
		return sync.Once{}
	case "sync\x00Pool":
		return sync.Pool{}
	case "sync\x00RWMutex":
		return sync.RWMutex{}
	case "sync\x00WaitGroup":
		return sync.WaitGroup{}
	case "time\x00Time":
		return time.Time{}
	default:
		return nil
	}
}

func CatalogRegisterGlobal(id int, importPath string, name string) bool {
	var value any
	switch importPath + "\x00" + name {
	case "os\x00Args":
		value = os.Args
	case "os\x00Stderr":
		value = os.Stderr
	case "os\x00Stdin":
		value = os.Stdin
	case "os\x00Stdout":
		value = os.Stdout
	default:
		return false
	}
	return RegisterNativeGlobal(id, value) == nil
}
