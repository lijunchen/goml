package nativebridge

import (
	"errors"
	"reflect"
	"sync"
	"sync/atomic"
	"testing"
)

type advancedCallbackTarget struct {
	prefix string
}

type advancedTransformer interface {
	Transform(string) (string, error)
}

type advancedProxy struct {
	token *CallbackToken
}

type advancedShell struct {
	Name   string `json:"name"`
	hidden int
}

func (proxy *advancedProxy) Transform(value string) (string, error) {
	values := InvokeCallback(
		proxy.token,
		"Transform",
		Values(value),
		[]reflect.Type{reflect.TypeFor[string](), reflect.TypeFor[error]()},
	)
	var err error
	if !values[1].IsNil() {
		err = values[1].Interface().(error)
	}
	return values[0].String(), err
}

func TestMakeCallbackUsesDirectTargetAndPreservesCopies(t *testing.T) {
	target := &advancedCallbackTarget{prefix: "go"}
	var calls atomic.Int32
	token, err := NewCallbackToken(target, func(invocation CallbackInvocation) CallResult {
		if invocation.Target != target || invocation.Method != "" {
			t.Fatalf("unexpected invocation target or method")
		}
		calls.Add(1)
		left := invocation.Arguments[0].Int()
		right := invocation.Arguments[1].Int()
		invocation.Arguments[0].SetInt(100)
		return CallResult{Values: Values(int(left+right), target.prefix)}
	})
	if err != nil {
		t.Fatal(err)
	}
	callback, err := MakeCallbackValue((func(int, int) (int, string))(nil), token)
	if err != nil {
		t.Fatal(err)
	}
	value := callback.Interface().(func(int, int) (int, string))
	result, text := value(2, 5)
	if result != 7 || text != "go" || calls.Load() != 1 {
		t.Fatalf("callback returned %d, %q after %d calls", result, text, calls.Load())
	}
}

func TestCallbackCanBeCalledConcurrently(t *testing.T) {
	var calls atomic.Int32
	token, err := NewCallbackToken(&calls, func(invocation CallbackInvocation) CallResult {
		invocation.Target.(*atomic.Int32).Add(1)
		return CallResult{Values: Values(int(invocation.Arguments[0].Int()) + 1)}
	})
	if err != nil {
		t.Fatal(err)
	}
	callback, err := MakeCallbackValue((func(int) int)(nil), token)
	if err != nil {
		t.Fatal(err)
	}
	function := callback.Interface().(func(int) int)
	var group sync.WaitGroup
	for index := 0; index < 32; index++ {
		group.Add(1)
		go func(value int) {
			defer group.Done()
			if function(value) != value+1 {
				t.Errorf("callback result for %d is wrong", value)
			}
		}(index)
	}
	group.Wait()
	if calls.Load() != 32 {
		t.Fatalf("callback count = %d", calls.Load())
	}
}

func TestCallbackPanicAndExitCrossNativeFrames(t *testing.T) {
	payload := &panicPayload{code: 41}
	panicToken, err := NewCallbackToken(payload, func(invocation CallbackInvocation) CallResult {
		return CallResult{Panicked: true, PanicValue: invocation.Target}
	})
	if err != nil {
		t.Fatal(err)
	}
	panicCallback, err := MakeCallbackValue((func())(nil), panicToken)
	if err != nil {
		t.Fatal(err)
	}
	exitToken, err := NewCallbackToken(29, func(invocation CallbackInvocation) CallResult {
		return CallResult{Exited: true, ExitCode: invocation.Target.(int)}
	})
	if err != nil {
		t.Fatal(err)
	}
	exitCallback, err := MakeCallbackValue((func())(nil), exitToken)
	if err != nil {
		t.Fatal(err)
	}
	registry, err := Install(
		Bind(1, func(callback func()) { callback() }),
		Bind(2, func(callback func()) { callback() }),
	)
	if err != nil {
		t.Fatal(err)
	}
	panicked := registry.Call(1, []ValueRef{panicCallback})
	if !panicked.Panicked || panicked.PanicValue != payload {
		t.Fatalf("panic result = %#v", panicked)
	}
	exited := registry.Call(2, []ValueRef{exitCallback})
	if !exited.Exited || exited.ExitCode != 29 {
		t.Fatalf("exit result = %#v", exited)
	}
}

func TestCallbackRejectsInvalidResults(t *testing.T) {
	token, err := NewCallbackToken(nil, func(CallbackInvocation) CallResult {
		return CallResult{Values: Values("wrong")}
	})
	if err != nil {
		t.Fatal(err)
	}
	callback, err := MakeCallbackValue((func() int)(nil), token)
	if err != nil {
		t.Fatal(err)
	}
	registry, err := Install(Bind(1, func(callback func() int) int { return callback() }))
	if err != nil {
		t.Fatal(err)
	}
	result := registry.Call(1, []ValueRef{callback})
	if !result.Panicked {
		t.Fatal("invalid callback result did not panic")
	}
	if _, ok := result.PanicValue.(error); !ok {
		t.Fatalf("panic payload = %#v", result.PanicValue)
	}
}

func TestNativeTypeCreatesAddressableIndependentValues(t *testing.T) {
	type state struct {
		Value int
	}
	key := t.Name()
	if err := RegisterNativeType(key, state{}); err != nil {
		t.Fatal(err)
	}
	first := NewNativeValue(key).(reflect.Value)
	second := NewNativeValue(key).(reflect.Value)
	if !first.CanAddr() || !AddressValue(first).(reflect.Value).IsValid() {
		t.Fatal("native zero value is not addressable")
	}
	first.Field(0).SetInt(7)
	if second.Field(0).Int() != 0 {
		t.Fatal("native zero values share storage")
	}
}

func TestCallbackQueueCarriesDirectRequestsAndReplies(t *testing.T) {
	queue, err := NewCallbackQueue(0)
	if err != nil {
		t.Fatal(err)
	}
	target := &advancedCallbackTarget{prefix: "queued"}
	token, err := queue.NewToken(target)
	if err != nil {
		t.Fatal(err)
	}
	callback, err := MakeCallbackValue((func(int) int)(nil), token)
	if err != nil {
		t.Fatal(err)
	}
	result := make(chan int, 1)
	go func() {
		result <- callback.Interface().(func(int) int)(8)
	}()
	request, ok := queue.Next()
	if !ok {
		t.Fatal("callback queue closed early")
	}
	invocation := request.Invocation()
	if invocation.Target != target || invocation.Arguments[0].Int() != 8 {
		t.Fatalf("queued invocation = %#v", invocation)
	}
	if !request.Complete(CallResult{Values: Values(13)}) {
		t.Fatal("callback response was rejected")
	}
	if request.Complete(CallResult{Values: Values(21)}) {
		t.Fatal("duplicate callback response was accepted")
	}
	if value := <-result; value != 13 {
		t.Fatalf("queued callback result = %d", value)
	}
	queue.Close()
	if !queue.Closed() || queue.Dispatch(CallbackInvocation{}).Err == nil {
		t.Fatal("closed callback queue accepted work")
	}
	canceledQueue, err := NewCallbackQueue(1)
	if err != nil {
		t.Fatal(err)
	}
	dispatched := make(chan CallResult, 1)
	go func() {
		dispatched <- canceledQueue.Dispatch(CallbackInvocation{Target: target})
	}()
	canceledRequest, ok := canceledQueue.Next()
	if !ok {
		t.Fatal("callback request was not queued")
	}
	canceledQueue.Close()
	if result := <-dispatched; result.Err == nil {
		t.Fatal("closing the queue did not cancel the callback")
	}
	if !canceledRequest.Canceled() || canceledRequest.Complete(CallResult{}) {
		t.Fatal("canceled callback accepted a response")
	}
}

func TestCompletionQueueRunsCallsAndPreservesTargets(t *testing.T) {
	registry, err := Install(Bind(1, func(left int, right int) int { return left + right }))
	if err != nil {
		t.Fatal(err)
	}
	queue, err := NewCompletionQueue(1)
	if err != nil {
		t.Fatal(err)
	}
	target := &advancedCallbackTarget{prefix: "completion"}
	if err := queue.StartCall(target, registry, 1, Values(4, 7), false); err != nil {
		t.Fatal(err)
	}
	completion, ok := queue.Next()
	if !ok || completion.Target != target {
		t.Fatalf("completion target = %#v", completion.Target)
	}
	assertSuccessful(t, completion.Result)
	if value := completion.Result.Values[0].Int(); value != 11 {
		t.Fatalf("completion value = %d", value)
	}
	payload := errors.New("async panic")
	if err := queue.Start(target, func() CallResult { panic(payload) }); err != nil {
		t.Fatal(err)
	}
	completion, ok = queue.Next()
	if !ok || !completion.Result.Panicked || completion.Result.PanicValue != payload {
		t.Fatalf("panic completion = %#v", completion)
	}
	queue.Close()
	if !queue.Closed() || queue.Start(target, func() CallResult { return CallResult{} }) == nil {
		t.Fatal("closed completion queue accepted work")
	}
}

func TestAggregateDescriptorsPackNamedShells(t *testing.T) {
	typeValue := reflect.TypeFor[advancedShell]()
	descriptor, err := NewAggregateDescriptor(
		typeValue,
		func(values []ValueRef) (ValueRef, error) {
			return ValueOf(advancedShell{Name: values[0].String(), hidden: int(values[1].Int())}), nil
		},
		func(value ValueRef) ([]ValueRef, error) {
			item := value.Interface().(advancedShell)
			return Values(item.Name, item.hidden), nil
		},
	)
	if err != nil {
		t.Fatal(err)
	}
	packed, err := descriptor.Pack(Values("goml", 26))
	if err != nil {
		t.Fatal(err)
	}
	if packed.Type() != typeValue || packed.Interface().(advancedShell) != (advancedShell{Name: "goml", hidden: 26}) {
		t.Fatalf("packed aggregate = %#v", packed.Interface())
	}
	if tag := typeValue.Field(0).Tag.Get("json"); tag != "name" {
		t.Fatalf("field tag = %q", tag)
	}
	boxed := reflect.New(reflect.TypeFor[any]()).Elem()
	boxed.Set(packed)
	values, err := descriptor.Unpack(boxed)
	if err != nil {
		t.Fatal(err)
	}
	if values[0].String() != "goml" || values[1].Int() != 26 {
		t.Fatalf("unpacked aggregate = %#v", values)
	}
	packed.FieldByName("Name").SetString("changed")
	if values[0].String() != "goml" {
		t.Fatal("aggregate unpack did not copy value fields")
	}
	if _, err := NewReflectAggregateDescriptor(typeValue); err == nil {
		t.Fatal("reflect codec accepted an unexported field")
	}
}

func TestReflectAggregateDescriptorCopiesArraysAndStructs(t *testing.T) {
	arrayDescriptor, err := NewReflectAggregateDescriptor(reflect.TypeFor[[2]int]())
	if err != nil {
		t.Fatal(err)
	}
	array, err := arrayDescriptor.Pack(Values(3, 5))
	if err != nil {
		t.Fatal(err)
	}
	items, err := arrayDescriptor.Unpack(array)
	if err != nil {
		t.Fatal(err)
	}
	array.Index(0).SetInt(9)
	if items[0].Int() != 3 || items[1].Int() != 5 {
		t.Fatalf("array elements = %#v", items)
	}
	structDescriptor, err := NewReflectAggregateDescriptor(reflect.TypeFor[record]())
	if err != nil {
		t.Fatal(err)
	}
	structure, err := structDescriptor.Pack(Values("value", 4))
	if err != nil {
		t.Fatal(err)
	}
	if structure.Interface().(record) != (record{Name: "value", Count: 4}) {
		t.Fatalf("struct value = %#v", structure.Interface())
	}
}

func TestInterfaceProxyDescriptorBuildsAOTProxy(t *testing.T) {
	target := &advancedCallbackTarget{prefix: "proxy:"}
	token, err := NewCallbackToken(target, func(invocation CallbackInvocation) CallResult {
		if invocation.Method != "Transform" || invocation.Target != target {
			return CallResult{Err: errors.New("unexpected proxy invocation")}
		}
		return CallResult{Values: []ValueRef{
			ValueOf(target.prefix + invocation.Arguments[0].String()),
			reflect.Zero(reflect.TypeFor[error]()),
		}}
	})
	if err != nil {
		t.Fatal(err)
	}
	descriptor, err := NewInterfaceProxyDescriptor(
		reflect.TypeFor[advancedTransformer](),
		func(token *CallbackToken) (ValueRef, error) {
			return ValueOf(&advancedProxy{token: token}), nil
		},
	)
	if err != nil {
		t.Fatal(err)
	}
	proxy, err := descriptor.Build(token)
	if err != nil {
		t.Fatal(err)
	}
	registry, err := Install(Bind(1, func(value advancedTransformer) string {
		result, callErr := value.Transform("goml")
		if callErr != nil {
			return callErr.Error()
		}
		return result
	}))
	if err != nil {
		t.Fatal(err)
	}
	result := registry.Call(1, []ValueRef{proxy})
	assertSuccessful(t, result)
	if value := result.Values[0].String(); value != "proxy:goml" {
		t.Fatalf("proxy result = %q", value)
	}
}

func TestChannelBridgeSupportsPollingCloseAndSelect(t *testing.T) {
	channel, err := MakeChannel(reflect.TypeFor[chan int](), 1)
	if err != nil {
		t.Fatal(err)
	}
	ready, send := TryChannelSend(channel, ValueOf(int32(7)))
	if !ready {
		t.Fatalf("send was not ready: %#v", send)
	}
	assertSuccessful(t, send)
	ready, send = TryChannelSend(channel, ValueOf(8))
	if ready || send.Err != nil || send.Panicked {
		t.Fatalf("full channel send = %t, %#v", ready, send)
	}
	received := TryChannelReceive(channel)
	if !received.Ready || !received.Open || received.Value.Int() != 7 {
		t.Fatalf("receive result = %#v", received)
	}
	selected := TryChannelSelect([]ChannelSelectCase{{Direction: ChannelCaseReceive, Channel: channel}})
	if selected.Ready || selected.Index != -1 {
		t.Fatalf("empty select result = %#v", selected)
	}
	selected = TryChannelSelect([]ChannelSelectCase{{Direction: ChannelCaseSend, Channel: channel, Send: ValueOf(11)}})
	if !selected.Ready || !selected.Open || selected.Index != 0 {
		t.Fatalf("send select result = %#v", selected)
	}
	received = TryChannelReceive(channel)
	if !received.Ready || received.Value.Int() != 11 {
		t.Fatalf("selected value = %#v", received)
	}
	first, err := MakeChannel(reflect.TypeFor[chan int](), 1)
	if err != nil {
		t.Fatal(err)
	}
	second, err := MakeChannel(reflect.TypeFor[chan int](), 1)
	if err != nil {
		t.Fatal(err)
	}
	if ready, result := TryChannelSend(first, ValueOf(1)); !ready || result.Err != nil {
		t.Fatalf("first setup send = %t, %#v", ready, result)
	}
	if ready, result := TryChannelSend(second, ValueOf(2)); !ready || result.Err != nil {
		t.Fatalf("second setup send = %t, %#v", ready, result)
	}
	selected = TryChannelSelect([]ChannelSelectCase{
		{Direction: ChannelCaseReceive, Channel: second},
		{Direction: ChannelCaseReceive, Channel: first},
	})
	if !selected.Ready || selected.Index != 0 || selected.Value.Int() != 2 {
		t.Fatalf("ordered select result = %#v", selected)
	}
	closed := CloseChannel(channel)
	assertSuccessful(t, closed)
	received = TryChannelReceive(channel)
	if !received.Ready || received.Open || received.Value.Int() != 0 {
		t.Fatalf("closed receive result = %#v", received)
	}
	ready, send = TryChannelSend(channel, ValueOf(1))
	if ready || !send.Panicked {
		t.Fatalf("closed send result = %t, %#v", ready, send)
	}
	closed = CloseChannel(channel)
	if !closed.Panicked {
		t.Fatalf("second close result = %#v", closed)
	}
	var nilChannel chan int
	nilValue := ValueOf(nilChannel)
	ready, send = TryChannelSend(nilValue, ValueOf(1))
	if ready || send.Err != nil || send.Panicked {
		t.Fatalf("nil send result = %t, %#v", ready, send)
	}
	received = TryChannelReceive(nilValue)
	if received.Ready || received.Err != nil || received.Panicked {
		t.Fatalf("nil receive result = %#v", received)
	}
}

func TestInvocationQueueCompletesCallsWithoutBlockingTheCaller(t *testing.T) {
	gate := make(chan struct{})
	_, err := InstallDefault(Bind(51, func() int {
		<-gate
		return 7
	}))
	if err != nil {
		t.Fatal(err)
	}
	queue := NewInvocationQueue(1)
	invocation := NewInvocation(51, false)
	if !InvocationQueueStart(queue, invocation) {
		t.Fatal("invocation did not start")
	}
	if InvocationReady(invocation) {
		t.Fatal("blocked invocation completed early")
	}
	close(gate)
	if !InvocationQueueWait(queue) {
		t.Fatal("completion queue closed")
	}
	if !InvocationReady(invocation) {
		t.Fatal("completed invocation is not ready")
	}
	if InvocationResult(invocation, 0).(reflect.Value).Int() != 7 {
		t.Fatal("invocation result mismatch")
	}
	InvocationQueueClose(queue)
}

func TestRegisteredProxyAndAggregateAdaptersUseDirectValues(t *testing.T) {
	interfaceProxies.Lock()
	interfaceProxies.values = nil
	interfaceProxies.Unlock()
	aggregateTypes.Lock()
	aggregateTypes.values = nil
	aggregateTypes.Unlock()
	if err := RegisterInterfaceProxy((*advancedTransformer)(nil), func(token *CallbackToken) (ValueRef, error) {
		return ValueOf(&advancedProxy{token: token}), nil
	}); err != nil {
		t.Fatal(err)
	}
	token, err := NewCallbackToken(nil, func(invocation CallbackInvocation) CallResult {
		return CallResult{Values: []ValueRef{ValueOf(invocation.Arguments[0].String() + "!"), reflect.Zero(reflect.TypeFor[error]())}}
	})
	if err != nil {
		t.Fatal(err)
	}
	proxy, err := MakeInterfaceProxy(reflect.TypeFor[advancedTransformer](), token)
	if err != nil {
		t.Fatal(err)
	}
	value, callErr := proxy.Interface().(advancedTransformer).Transform("go")
	if value != "go!" || callErr != nil {
		t.Fatalf("proxy returned %q, %v", value, callErr)
	}
	if err := RegisterAggregateType("shell", advancedShell{}); err != nil {
		t.Fatal(err)
	}
	aggregate := NewAggregateValue("shell")
	if !SetAggregateValue(aggregate, 0, ValueOf("goml")) {
		t.Fatal("aggregate field was not set")
	}
	if field := ValueAggregateField(aggregate, 0).(reflect.Value).String(); field != "goml" {
		t.Fatalf("aggregate field = %q", field)
	}
}

func TestNativeReferencesPreserveOpaqueFields(t *testing.T) {
	type nested struct {
		Value int
	}
	type holder struct {
		Nested *nested
		Text   string
	}
	value := holder{Nested: &nested{Value: 7}, Text: "before"}
	root := ValueOf(&value)
	indirect := IndirectValue(root)
	if !ValidValue(indirect) {
		t.Fatal("pointer did not produce an indirect value")
	}
	text := FieldReference(indirect, 1)
	if !SetValue(text, ValueOf("after")) || value.Text != "after" {
		t.Fatalf("field assignment produced %#v", value)
	}
	nestedPointer := FieldReference(indirect, 0)
	nestedValue := IndirectValue(nestedPointer)
	field := FieldReference(nestedValue, 0)
	if !SetValue(field, ValueOf(11)) || value.Nested.Value != 11 {
		t.Fatalf("nested field assignment produced %#v", value)
	}
}

func TestNativeGlobalsAreRegisteredByObjectID(t *testing.T) {
	nativeGlobals.Lock()
	nativeGlobals.values = nil
	nativeGlobals.Unlock()
	if err := RegisterNativeGlobal(17, "goml"); err != nil {
		t.Fatal(err)
	}
	value := NativeGlobalValue(17).(reflect.Value)
	if value.String() != "goml" {
		t.Fatalf("native global = %q", value.String())
	}
	if err := RegisterNativeGlobal(17, 3); err == nil {
		t.Fatal("conflicting native global registration succeeded")
	}
}

func TestChannelInvocationWakesCompletionQueue(t *testing.T) {
	queue := NewInvocationQueue(1)
	invocation := NewChannelInvocation()
	channel := make(chan int, 1)
	if !ChannelInvocationAppendReceive(invocation, ValueOf(channel)) {
		t.Fatal("channel receive was rejected")
	}
	if !ChannelInvocationStart(queue, invocation) {
		t.Fatal("channel invocation did not start")
	}
	channel <- 17
	if !InvocationQueueWait(queue) || !ChannelInvocationReady(invocation) {
		t.Fatal("channel invocation did not complete")
	}
	if ChannelInvocationIndex(invocation) != 0 || !ChannelInvocationOpen(invocation) {
		t.Fatal("channel invocation metadata is wrong")
	}
	if value := ChannelInvocationValue(invocation).(reflect.Value).Int(); value != 17 {
		t.Fatalf("channel invocation value = %d", value)
	}
	InvocationQueueClose(queue)
}
