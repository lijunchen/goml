package nativebridge

import (
	"errors"
	"os"
	"reflect"
	"runtime"
	"testing"
)

type counter struct {
	value int
}

type record struct {
	Name  string
	Count int
}

type panicPayload struct {
	code int
}

func (value counter) Add(delta int) int {
	return value.value + delta
}

func (value counter) Join(prefix string, parts ...string) string {
	result := prefix
	for _, part := range parts {
		result += part
	}
	return result
}

func TestRegistryCallsFunctions(t *testing.T) {
	registry, err := Install(
		Bind(1, func(left int, right int) int { return left + right }),
		BindValue(2, reflect.ValueOf(func(value int) (int, string) { return value * 2, "done" })),
	)
	if err != nil {
		t.Fatal(err)
	}

	sum := registry.Call(1, Values(2, 5))
	assertSuccessful(t, sum)
	if got := sum.Values[0].Interface().(int); got != 7 {
		t.Fatalf("sum = %d, want 7", got)
	}

	multiple := registry.Call(2, Values(4))
	assertSuccessful(t, multiple)
	if len(multiple.Values) != 2 {
		t.Fatalf("result count = %d, want 2", len(multiple.Values))
	}
	if got := multiple.Values[0].Interface().(int); got != 8 {
		t.Fatalf("first result = %d, want 8", got)
	}
	if got := multiple.Values[1].Interface().(string); got != "done" {
		t.Fatalf("second result = %q, want done", got)
	}
}

func TestDefaultRegistryInvocationConvertsValues(t *testing.T) {
	_, err := InstallDefault(
		Bind(11, func(value string) string { return value + "!" }),
		Bind(12, func(prefix string, values ...int) int {
			total := len(prefix)
			for _, value := range values {
				total += value
			}
			return total
		}),
	)
	if err != nil {
		t.Fatal(err)
	}
	call := NewInvocation(11, false)
	InvocationAppend(call, NewStringValue("go"))
	InvocationRun(call)
	if err := InvocationError(call); err != "" {
		t.Fatal(err)
	}
	if got := ValueStringText(InvocationResult(call, 0)); got != "go!" {
		t.Fatalf("got %q", got)
	}
	direct := NewInvocation(12, false)
	InvocationAppend(direct, NewStringValue("go"))
	InvocationAppend(direct, NewIntegerValue("int", 3))
	InvocationAppend(direct, NewIntegerValue("int", 4))
	InvocationRun(direct)
	if err := InvocationError(direct); err != "" {
		t.Fatal(err)
	}
	if got := ValueIntegerBits(InvocationResult(direct, 0)); got != 9 {
		t.Fatalf("got %d", got)
	}
	variadic := NewInvocation(12, true)
	InvocationAppend(variadic, NewStringValue("go"))
	values := NewSliceValue("[]int", 2, false)
	if !SetSliceValue(values, 0, NewIntegerValue("int", 3)) {
		t.Fatal("first slice value was rejected")
	}
	if !SetSliceValue(values, 1, NewIntegerValue("int", 4)) {
		t.Fatal("second slice value was rejected")
	}
	InvocationAppend(variadic, values)
	InvocationRun(variadic)
	if err := InvocationError(variadic); err != "" {
		t.Fatal(err)
	}
	if got := ValueIntegerBits(InvocationResult(variadic, 0)); got != 9 {
		t.Fatalf("got %d", got)
	}
}

func TestInterfaceSliceValues(t *testing.T) {
	values := NewSliceValue("[]interface", 2, false)
	if !SetSliceValue(values, 0, NewStringValue("goml")) {
		t.Fatal("string interface element was rejected")
	}
	if !SetSliceValue(values, 1, NewIntegerValue("int", 26)) {
		t.Fatal("integer interface element was rejected")
	}
	ref, ok := values.(reflect.Value)
	if !ok {
		t.Fatalf("slice value has type %T", values)
	}
	got := ref.Interface().([]any)
	if got[0] != "goml" || got[1] != 26 {
		t.Fatalf("slice value = %#v", got)
	}
}

func TestErrorSliceValues(t *testing.T) {
	values := NewSliceValue("[]error", 1, false)
	if !SetSliceValue(values, 0, reflect.ValueOf(errors.New("failure"))) {
		t.Fatal("error interface element was rejected")
	}
	ref, ok := values.(reflect.Value)
	if !ok {
		t.Fatalf("slice value has type %T", values)
	}
	got := ref.Interface().([]error)
	if got[0].Error() != "failure" {
		t.Fatalf("slice value = %#v", got)
	}
}

func TestInvocationPreservesPanicAndExit(t *testing.T) {
	payload := &counter{value: 7}
	_, err := InstallDefault(
		Bind(21, func() { panic(payload) }),
		ExitBinding(22),
	)
	if err != nil {
		t.Fatal(err)
	}
	panicked := NewInvocation(21, false)
	InvocationRun(panicked)
	if !InvocationPanicked(panicked) {
		t.Fatal("panic was not preserved")
	}
	panicValue, ok := InvocationPanic(panicked).(reflect.Value)
	if !ok || panicValue.Interface() != payload {
		t.Fatal("panic payload was not preserved")
	}
	exited := NewInvocation(22, false)
	InvocationAppend(exited, NewIntegerValue("int", 17))
	InvocationRun(exited)
	if !InvocationExited(exited) || InvocationExitCode(exited) != 17 {
		t.Fatalf("exit was not preserved: %+v", exited)
	}
}

func TestValueInvocationCallsDirectFunction(t *testing.T) {
	invocation := NewValueInvocation(reflect.ValueOf(func(value int) int { return value + 1 }), false)
	InvocationAppend(invocation, NewIntegerValue("int", 41))
	InvocationRun(invocation)
	if message := InvocationError(invocation); message != "" {
		t.Fatal(message)
	}
	result := InvocationResult(invocation, 0).(reflect.Value)
	if result.Int() != 42 {
		t.Fatalf("result = %d", result.Int())
	}
}

func TestInvocationArgumentRetainsSliceMutations(t *testing.T) {
	invocation := NewValueInvocation(reflect.ValueOf(func(values []byte) {
		values[0] = 42
	}), false)
	values := NewSliceValue("[]uint8", 1, false)
	InvocationAppend(invocation, values)
	InvocationRun(invocation)
	if message := InvocationError(invocation); message != "" {
		t.Fatal(message)
	}
	argument := InvocationArgument(invocation, 0).(reflect.Value)
	if argument.Index(0).Uint() != 42 {
		t.Fatalf("argument = %d", argument.Index(0).Uint())
	}
}

func TestInvocationRuntimeStackUsesInterpretedGoroutine(t *testing.T) {
	invocation := NewValueInvocation(reflect.ValueOf(runtime.Stack), false)
	buffer := reflect.ValueOf(make([]byte, 64))
	InvocationAppend(invocation, buffer)
	InvocationAppend(invocation, reflect.ValueOf(false))
	if !InvocationUseRuntimeStack(invocation, 37) {
		t.Fatal("runtime stack context was rejected")
	}
	InvocationRun(invocation)
	if message := InvocationError(invocation); message != "" {
		t.Fatal(message)
	}
	length := InvocationResult(invocation, 0).(reflect.Value).Int()
	if got := string(buffer.Bytes()[:length]); got != "goroutine 37 [running]:\n" {
		t.Fatalf("stack = %q", got)
	}
}

func TestRegistryCallsVariadicFunctions(t *testing.T) {
	registry, err := Install(Bind(1, func(prefix string, values ...int) int {
		result := len(prefix)
		for _, value := range values {
			result += value
		}
		return result
	}))
	if err != nil {
		t.Fatal(err)
	}

	direct := registry.Call(1, Values("go", 3, 4))
	assertSuccessful(t, direct)
	if got := direct.Values[0].Interface().(int); got != 9 {
		t.Fatalf("direct variadic result = %d, want 9", got)
	}

	sliced := registry.CallSlice(1, Values("go", []int{5, 6}))
	assertSuccessful(t, sliced)
	if got := sliced.Values[0].Interface().(int); got != 13 {
		t.Fatalf("slice variadic result = %d, want 13", got)
	}
}

func TestCallMethod(t *testing.T) {
	receiver := ValueOf(counter{value: 4})
	direct := CallMethod(receiver, "Add", Values(3))
	assertSuccessful(t, direct)
	if got := direct.Values[0].Interface().(int); got != 7 {
		t.Fatalf("method result = %d, want 7", got)
	}

	sliced := CallMethodSlice(receiver, "Join", Values("x", []string{"y", "z"}))
	assertSuccessful(t, sliced)
	if got := sliced.Values[0].Interface().(string); got != "xyz" {
		t.Fatalf("variadic method result = %q, want xyz", got)
	}
}

func TestCallPreservesPanicValue(t *testing.T) {
	payload := &panicPayload{code: 23}
	registry, err := Install(Bind(1, func() { panic(payload) }))
	if err != nil {
		t.Fatal(err)
	}

	result := registry.Call(1, nil)
	if !result.Panicked {
		t.Fatal("call did not report panic")
	}
	if result.PanicValue != payload {
		t.Fatalf("panic value = %#v, want original payload", result.PanicValue)
	}
	if result.Err != nil || result.Exited || len(result.Values) != 0 {
		t.Fatalf("unexpected panic result: %#v", result)
	}
}

func TestCallPreservesTypedNil(t *testing.T) {
	registry, err := Install(Bind(1, func(value *counter) *counter { return value }))
	if err != nil {
		t.Fatal(err)
	}
	var input *counter

	result := registry.Call(1, Values(input))
	assertSuccessful(t, result)
	if len(result.Values) != 1 {
		t.Fatalf("result count = %d, want 1", len(result.Values))
	}
	value := result.Values[0]
	if value.Type() != reflect.TypeOf(input) {
		t.Fatalf("result type = %s, want %s", value.Type(), reflect.TypeOf(input))
	}
	if !value.IsNil() {
		t.Fatalf("result = %#v, want typed nil", value.Interface())
	}
}

func TestCloneCopiesArraysAndStructs(t *testing.T) {
	array := Clone(ValueOf([2]int{1, 2}))
	arrayClone := Clone(array)
	arrayClone.Index(0).SetInt(9)
	if got := array.Interface().([2]int); got != [2]int{1, 2} {
		t.Fatalf("array source = %#v, want [1 2]", got)
	}
	if got := arrayClone.Interface().([2]int); got != [2]int{9, 2} {
		t.Fatalf("array clone = %#v, want [9 2]", got)
	}

	structValue := Clone(ValueOf(record{Name: "a", Count: 1}))
	structClone := Clone(structValue)
	structClone.FieldByName("Count").SetInt(8)
	if got := structValue.Interface().(record); got != (record{Name: "a", Count: 1}) {
		t.Fatalf("struct source = %#v", got)
	}
	if got := structClone.Interface().(record); got != (record{Name: "a", Count: 8}) {
		t.Fatalf("struct clone = %#v", got)
	}
}

func TestCloneSharesReferenceDescriptors(t *testing.T) {
	number := 3
	pointer := Clone(ValueOf(&number))
	pointerClone := Clone(pointer)
	pointerClone.Elem().SetInt(7)
	if number != 7 || pointer.Interface().(*int) != pointerClone.Interface().(*int) {
		t.Fatalf("pointer clone did not preserve identity")
	}

	sourceSlice := []int{1, 2, 3}
	sliceValue := Clone(ValueOf(sourceSlice))
	sliceClone := Clone(sliceValue)
	sliceClone.Index(0).SetInt(9)
	sliceClone.SetLen(2)
	if sourceSlice[0] != 9 {
		t.Fatalf("slice clone did not share backing array")
	}
	if sliceValue.Len() != 3 || sliceClone.Len() != 2 {
		t.Fatalf("slice descriptors have lengths %d and %d", sliceValue.Len(), sliceClone.Len())
	}

	sourceMap := map[string]int{"a": 1}
	mapValue := Clone(ValueOf(sourceMap))
	mapClone := Clone(mapValue)
	mapClone.SetMapIndex(ValueOf("b"), ValueOf(2))
	if sourceMap["b"] != 2 {
		t.Fatalf("map clone did not share map storage")
	}
}

func TestOpaqueValueHelpersPreserveReflectValues(t *testing.T) {
	original := reflect.ValueOf([2]int{1, 2})
	cloned := CloneValue(original).(reflect.Value)
	if !ValidValue(cloned) || NilValue(cloned) || ValueType(cloned) != "[2]int" {
		t.Fatalf("opaque clone metadata is invalid")
	}
	cloned.Index(0).SetInt(9)
	if original.Interface().([2]int) != [2]int{1, 2} {
		t.Fatalf("opaque clone did not preserve assignment semantics")
	}
	var pointer *int
	typedNil := reflect.ValueOf(pointer)
	if !ValidValue(typedNil) || !NilValue(typedNil) || ValueType(typedNil) != "*int" {
		t.Fatalf("typed nil metadata is invalid")
	}
	if ValidValue(struct{}{}) || !NilValue(struct{}{}) || ValueType(struct{}{}) != "" {
		t.Fatalf("non-reflect values were accepted")
	}
}

func TestReplaceProcessRestoresEnvironmentAfterFailure(t *testing.T) {
	const key = "GOMLGO_TEST_REPLACE_PROCESS"
	t.Setenv(key, "before")
	message := ReplaceProcess("/gomlgo/missing-executable", []string{"missing-executable"}, key, "after")
	if message == "" {
		t.Fatal("missing executable returned no error")
	}
	if value := os.Getenv(key); value != "before" {
		t.Fatalf("environment value = %q, want before", value)
	}
}

func TestExitBindingReturnsExitSignal(t *testing.T) {
	registry, err := Install(ExitBinding(7))
	if err != nil {
		t.Fatal(err)
	}

	result := registry.Call(7, Values(42))
	if !result.Exited || result.ExitCode != 42 {
		t.Fatalf("exit result = %#v, want code 42", result)
	}
	if result.Err != nil || result.Panicked || len(result.Values) != 0 {
		t.Fatalf("unexpected exit result: %#v", result)
	}
}

func TestRegistryRejectsInvalidBindingsAndCalls(t *testing.T) {
	if _, err := Install(Bind(1, 3)); err == nil {
		t.Fatal("non-function binding was accepted")
	}
	if _, err := Install(Bind(1, func() {}), Bind(1, func() {})); err == nil {
		t.Fatal("duplicate binding was accepted")
	}
	registry, err := Install(Bind(1, func(value int) int { return value }))
	if err != nil {
		t.Fatal(err)
	}
	if result := registry.Call(2, nil); result.Err == nil {
		t.Fatal("missing call ID was accepted")
	}
	if result := registry.Call(1, Values("wrong")); result.Err == nil {
		t.Fatal("invalid argument was accepted")
	}
}

func TestValueLengthAndCapacityPreserveSliceShape(t *testing.T) {
	value := reflect.MakeSlice(reflect.TypeFor[[]int](), 2, 7)
	if ValueLength(value) != 2 || ValueCapacity(value) != 7 {
		t.Fatalf("unexpected slice shape: len=%d cap=%d", ValueLength(value), ValueCapacity(value))
	}
}

func TestMethodValueBindsReceiver(t *testing.T) {
	method := MethodValue(reflect.ValueOf(counter{value: 3}), "Add").(reflect.Value)
	if !method.IsValid() {
		t.Fatal("method value is invalid")
	}
	result := method.Call(Values(4))
	if len(result) != 1 || result[0].Int() != 7 {
		t.Fatalf("unexpected method result: %v", result)
	}
}

func assertSuccessful(t *testing.T, result CallResult) {
	t.Helper()
	if result.Err != nil || result.Panicked || result.Exited {
		t.Fatalf("call failed: %#v", result)
	}
}
