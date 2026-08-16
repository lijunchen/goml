package nativebridge

import (
	"bytes"
	"context"
	"crypto/sha256"
	"errors"
	"fmt"
	"hash"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"
)

type catalogPointer struct {
	value byte
}

type catalogLockerProxy struct {
	token *CallbackToken
}

type catalogReaderProxy struct {
	token *CallbackToken
}

type catalogWriterProxy struct {
	token *CallbackToken
}

var catalogPointers struct {
	sync.Mutex
	values map[string]*catalogPointer
}

var programExecutable struct {
	sync.RWMutex
	value string
}

func SetProgramExecutable(value string) {
	programExecutable.Lock()
	programExecutable.value = value
	programExecutable.Unlock()
}

func ProgramExecutable() (string, error) {
	programExecutable.RLock()
	value := programExecutable.value
	programExecutable.RUnlock()
	if value != "" {
		return value, nil
	}
	return os.Executable()
}

func ResetCatalog() {
	defaultRegistry.Lock()
	defaultRegistry.value = &Registry{
		functions: make(map[CallID]reflect.Value),
		names:     make(map[CallID]string),
	}
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

func (value *catalogReaderProxy) Read(buffer []byte) (int, error) {
	results := InvokeCallback(
		value.token,
		"Read",
		Values(buffer),
		[]reflect.Type{reflect.TypeFor[int](), reflect.TypeFor[error]()},
	)
	return CallbackValue[int](results, 0), CallbackValue[error](results, 1)
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
	case "io\x00Reader":
		return RegisterInterfaceProxy((*io.Reader)(nil), func(token *CallbackToken) (ValueRef, error) {
			return reflect.ValueOf(&catalogReaderProxy{token: token}), nil
		}) == nil
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
	case "bytes", "context", "crypto/sha256", "errors", "fmt", "hash", "io", "io/fs", "os", "os/exec", "path/filepath", "reflect", "runtime", "slices", "strconv", "strings", "sync", "syscall", "time":
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
		defaultRegistry.value = &Registry{
			functions: make(map[CallID]reflect.Value),
			names:     make(map[CallID]string),
		}
	}
	if defaultRegistry.value.functions == nil {
		defaultRegistry.value.functions = make(map[CallID]reflect.Value)
	}
	if defaultRegistry.value.names == nil {
		defaultRegistry.value.names = make(map[CallID]string)
	}
	callID := CallID(id)
	if _, exists := defaultRegistry.value.functions[callID]; exists {
		return false
	}
	defaultRegistry.value.functions[callID] = value
	receiver := ""
	if receiverName != "" {
		receiver = receiverName + "."
	}
	defaultRegistry.value.names[callID] = importPath + "." + receiver + symbol
	return true
}

func catalogFunction(importPath string, symbol string) any {
	switch importPath + "\x00" + symbol {
	case "bytes\x00NewReader":
		return bytes.NewReader
	case "context\x00Background":
		return context.Background
	case "context\x00WithCancel":
		return context.WithCancel
	case "context\x00WithTimeout":
		return context.WithTimeout
	case "crypto/sha256\x00New":
		return sha256.New
	case "errors\x00Is":
		return errors.Is
	case "fmt\x00Fprint":
		return fmt.Fprint
	case "fmt\x00Print":
		return fmt.Print
	case "fmt\x00Println":
		return fmt.Println
	case "fmt\x00Sprintf":
		return fmt.Sprintf
	case "io\x00Copy":
		return io.Copy
	case "os\x00Environ":
		return os.Environ
	case "os\x00Executable":
		return ProgramExecutable
	case "os\x00Getwd":
		return os.Getwd
	case "os\x00IsExist":
		return os.IsExist
	case "os\x00IsNotExist":
		return os.IsNotExist
	case "os\x00IsPermission":
		return os.IsPermission
	case "os\x00IsTimeout":
		return os.IsTimeout
	case "os\x00Link":
		return os.Link
	case "os\x00LookupEnv":
		return os.LookupEnv
	case "os\x00Mkdir":
		return os.Mkdir
	case "os\x00MkdirAll":
		return os.MkdirAll
	case "os\x00Open":
		return os.Open
	case "os\x00ReadDir":
		return os.ReadDir
	case "os\x00ReadFile":
		return os.ReadFile
	case "os\x00Remove":
		return os.Remove
	case "os\x00RemoveAll":
		return os.RemoveAll
	case "os\x00Rename":
		return os.Rename
	case "os\x00Stat":
		return os.Stat
	case "os\x00Symlink":
		return os.Symlink
	case "os\x00WriteFile":
		return os.WriteFile
	case "os/exec\x00Command":
		return exec.Command
	case "os/exec\x00CommandContext":
		return exec.CommandContext
	case "os/exec\x00LookPath":
		return exec.LookPath
	case "path/filepath\x00Abs":
		return filepath.Abs
	case "path/filepath\x00Base":
		return filepath.Base
	case "path/filepath\x00Clean":
		return filepath.Clean
	case "path/filepath\x00Dir":
		return filepath.Dir
	case "path/filepath\x00EvalSymlinks":
		return filepath.EvalSymlinks
	case "path/filepath\x00Ext":
		return filepath.Ext
	case "path/filepath\x00IsAbs":
		return filepath.IsAbs
	case "path/filepath\x00Join":
		return filepath.Join
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
	case "strings\x00TrimPrefix":
		return strings.TrimPrefix
	case "strings\x00TrimSuffix":
		return strings.TrimSuffix
	case "strings\x00ToUpper":
		return strings.ToUpper
	case "sync\x00NewCond":
		return sync.NewCond
	case "syscall\x00Kill":
		return syscall.Kill
	case "time\x00Now":
		return time.Now
	default:
		return nil
	}
}

func catalogMethod(importPath string, receiverName string, receiverPointer bool, symbol string) any {
	key := importPath + "\x00" + receiverName + "\x00" + symbol
	switch key {
	case "bytes\x00Buffer\x00Bytes":
		if receiverPointer {
			return (*bytes.Buffer).Bytes
		}
	case "context\x00Context\x00Done":
		if !receiverPointer {
			return context.Context.Done
		}
	case "context\x00Context\x00Err":
		if !receiverPointer {
			return context.Context.Err
		}
	case "hash\x00Hash\x00Sum":
		if !receiverPointer {
			return hash.Hash.Sum
		}
	case "io/fs\x00DirEntry\x00Name":
		if !receiverPointer {
			return fs.DirEntry.Name
		}
	case "io/fs\x00FileInfo\x00IsDir":
		if !receiverPointer {
			return fs.FileInfo.IsDir
		}
	case "io/fs\x00FileInfo\x00Mode":
		if !receiverPointer {
			return fs.FileInfo.Mode
		}
	case "io/fs\x00FileMode\x00IsRegular":
		if !receiverPointer {
			return fs.FileMode.IsRegular
		}
	case "os\x00File\x00Close":
		if receiverPointer {
			return (*os.File).Close
		}
	case "os\x00ProcessState\x00ExitCode":
		if receiverPointer {
			return (*os.ProcessState).ExitCode
		}
	case "os/exec\x00Cmd\x00Run":
		if receiverPointer {
			return (*exec.Cmd).Run
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
	case "time\x00Time\x00UnixNano":
		if !receiverPointer {
			return time.Time.UnixNano
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
	case "bytes\x00Buffer":
		return bytes.Buffer{}
	case "bytes\x00Reader":
		return bytes.Reader{}
	case "os\x00File":
		return os.File{}
	case "os\x00LinkError":
		return os.LinkError{}
	case "os\x00PathError":
		return os.PathError{}
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
	case "os/exec\x00Cmd":
		return exec.Cmd{}
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
	case "syscall\x00SysProcAttr":
		return syscall.SysProcAttr{}
	case "time\x00Time":
		return time.Time{}
	default:
		return nil
	}
}

func CatalogRegisterGlobal(id int, importPath string, name string) bool {
	var value any
	switch importPath + "\x00" + name {
	case "context\x00DeadlineExceeded":
		value = context.DeadlineExceeded
	case "io\x00EOF":
		value = io.EOF
	case "io\x00ErrShortWrite":
		value = io.ErrShortWrite
	case "io\x00ErrUnexpectedEOF":
		value = io.ErrUnexpectedEOF
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
