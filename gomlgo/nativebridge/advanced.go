package nativebridge

import (
	"fmt"
	"reflect"
	"sync"
)

type CallbackInvocation struct {
	Target    any
	Method    string
	Arguments []ValueRef
}

type CallbackDispatch func(CallbackInvocation) CallResult

type CallbackToken struct {
	target   any
	dispatch CallbackDispatch
}

type PendingCallback struct {
	invocation CallbackInvocation
	finished   chan struct{}
	result     CallResult
	canceled   bool
	once       sync.Once
}

type CallbackQueue struct {
	requests chan *PendingCallback
	closed   chan struct{}
	once     sync.Once
	notify   *CompletionQueue
}

type AsyncCompletion struct {
	Target any
	Result CallResult
}

type CompletionQueue struct {
	completions chan AsyncCompletion
	closed      chan struct{}
	once        sync.Once
}

func NewInvocationQueue(capacity int) any {
	queue, err := NewCompletionQueue(capacity)
	if err != nil {
		return reflect.Value{}
	}
	return reflect.ValueOf(queue)
}

func invocationQueueValue(value any) (*CompletionQueue, bool) {
	ref, ok := value.(reflect.Value)
	if !ok || !ref.IsValid() || ref.Kind() != reflect.Pointer || ref.IsNil() {
		return nil, false
	}
	queue, ok := ref.Interface().(*CompletionQueue)
	return queue, ok
}

func InvocationQueueStart(queueValue any, invocation any) bool {
	queue, ok := invocationQueueValue(queueValue)
	if !ok {
		return false
	}
	call, err := invocationValue(invocation)
	if err != nil {
		return false
	}
	started := false
	call.start.Do(func() {
		started = true
		_ = queue.Start(call, func() CallResult {
			InvocationRun(reflect.ValueOf(call))
			close(call.done)
			return call.result
		})
	})
	return started
}

func InvocationQueueTry(queueValue any) bool {
	queue, ok := invocationQueueValue(queueValue)
	if !ok {
		return false
	}
	_, ready := queue.TryNext()
	return ready
}

func InvocationQueueWait(queueValue any) bool {
	queue, ok := invocationQueueValue(queueValue)
	if !ok {
		return false
	}
	_, ready := queue.Next()
	return ready
}

func InvocationQueueClose(queueValue any) {
	queue, ok := invocationQueueValue(queueValue)
	if ok {
		queue.Close()
	}
}

type AggregatePack func([]ValueRef) (ValueRef, error)

type AggregateUnpack func(ValueRef) ([]ValueRef, error)

type AggregateDescriptor struct {
	typeValue reflect.Type
	elements  []reflect.Type
	pack      AggregatePack
	unpack    AggregateUnpack
}

type ProxyFactory func(*CallbackToken) (ValueRef, error)

type InterfaceProxyDescriptor struct {
	interfaceType reflect.Type
	factory       ProxyFactory
}

var interfaceProxies struct {
	sync.RWMutex
	values map[reflect.Type]*InterfaceProxyDescriptor
}

var aggregateTypes struct {
	sync.RWMutex
	values map[string]reflect.Type
}

var nativeTypes struct {
	sync.RWMutex
	values map[string]reflect.Type
}

type CallbackCompletion struct {
	request *PendingCallback
	result  CallResult
}

type ChannelReceiveResult struct {
	Value      ValueRef
	Ready      bool
	Open       bool
	Err        error
	PanicValue any
	Panicked   bool
}

type ChannelCaseDirection uint8

const (
	ChannelCaseReceive ChannelCaseDirection = iota
	ChannelCaseSend
)

type ChannelSelectCase struct {
	Direction ChannelCaseDirection
	Channel   ValueRef
	Send      ValueRef
}

type ChannelSelectResult struct {
	Index      int
	Value      ValueRef
	Ready      bool
	Open       bool
	Err        error
	PanicValue any
	Panicked   bool
}

type ChannelInvocation struct {
	cases      []reflect.SelectCase
	result     ChannelSelectResult
	done       chan struct{}
	cancel     chan struct{}
	cancelOnce sync.Once
}

func NewChannelInvocation() any {
	return reflect.ValueOf(&ChannelInvocation{done: make(chan struct{}), cancel: make(chan struct{})})
}

func channelInvocationValue(value any) (*ChannelInvocation, bool) {
	ref, ok := value.(reflect.Value)
	if !ok || !ref.IsValid() || ref.Kind() != reflect.Pointer || ref.IsNil() {
		return nil, false
	}
	invocation, ok := ref.Interface().(*ChannelInvocation)
	return invocation, ok
}

func ChannelInvocationAppendReceive(invocationValue any, channelValue any) bool {
	invocation, ok := channelInvocationValue(invocationValue)
	channel, valid := reflectedValue(channelValue)
	if !ok || !valid || channel.Kind() != reflect.Chan || channel.Type().ChanDir() == reflect.SendDir {
		return false
	}
	invocation.cases = append(invocation.cases, reflect.SelectCase{Dir: reflect.SelectRecv, Chan: channel})
	return true
}

func ChannelInvocationAppendDefault(invocationValue any) bool {
	invocation, ok := channelInvocationValue(invocationValue)
	if !ok {
		return false
	}
	invocation.cases = append(invocation.cases, reflect.SelectCase{Dir: reflect.SelectDefault})
	return true
}

func ChannelInvocationStart(queueValue any, invocationValue any) bool {
	queue, ok := invocationQueueValue(queueValue)
	invocation, valid := channelInvocationValue(invocationValue)
	if !ok || !valid || len(invocation.cases) == 0 {
		return false
	}
	if queue.Closed() {
		return false
	}
	go func() {
		cases := append([]reflect.SelectCase{}, invocation.cases...)
		cases = append(cases, reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(invocation.cancel)})
		chosen, value, open := reflect.Select(cases)
		if chosen == len(invocation.cases) {
			invocation.result.Index = -1
		} else {
			invocation.result.Index = chosen
			invocation.result.Value = value
			invocation.result.Ready = true
			invocation.result.Open = open
		}
		_ = queue.Submit(AsyncCompletion{Target: invocation})
		close(invocation.done)
	}()
	return true
}

func ChannelInvocationCancel(invocationValue any) bool {
	invocation, ok := channelInvocationValue(invocationValue)
	if !ok {
		return false
	}
	invocation.cancelOnce.Do(func() {
		close(invocation.cancel)
	})
	return true
}

func ChannelInvocationWait(invocationValue any) bool {
	invocation, ok := channelInvocationValue(invocationValue)
	if !ok {
		return false
	}
	<-invocation.done
	return true
}

func ChannelInvocationReady(invocationValue any) bool {
	invocation, ok := channelInvocationValue(invocationValue)
	if !ok {
		return false
	}
	select {
	case <-invocation.done:
		return true
	default:
		return false
	}
}

func ChannelInvocationIndex(invocationValue any) int {
	invocation, ok := channelInvocationValue(invocationValue)
	if !ok {
		return -1
	}
	return invocation.result.Index
}

func ChannelInvocationValue(invocationValue any) any {
	invocation, ok := channelInvocationValue(invocationValue)
	if !ok {
		return reflect.Value{}
	}
	return invocation.result.Value
}

func ChannelInvocationOpen(invocationValue any) bool {
	invocation, ok := channelInvocationValue(invocationValue)
	return ok && invocation.result.Open
}

func NewCallbackToken(target any, dispatch CallbackDispatch) (*CallbackToken, error) {
	if dispatch == nil {
		return nil, fmt.Errorf("native callback dispatcher is nil")
	}
	return &CallbackToken{target: target, dispatch: dispatch}, nil
}

func (token *CallbackToken) Target() any {
	if token == nil {
		return nil
	}
	return token.target
}

func clonedValues(values []ValueRef) []ValueRef {
	result := make([]ValueRef, len(values))
	for index, value := range values {
		result[index] = Clone(value)
	}
	return result
}

func callbackReturnedValues(result CallResult, expected []reflect.Type) []ValueRef {
	states := 0
	if result.Err != nil {
		states++
	}
	if result.Panicked {
		states++
	}
	if result.Exited {
		states++
	}
	if states > 1 {
		panic(fmt.Errorf("native callback returned conflicting outcomes"))
	}
	if result.Err != nil {
		panic(result.Err)
	}
	if result.Panicked {
		panic(result.PanicValue)
	}
	if result.Exited {
		panic(exitSignal{code: result.ExitCode})
	}
	if len(result.Values) != len(expected) {
		panic(fmt.Errorf("native callback returned %d values, want %d", len(result.Values), len(expected)))
	}
	values := make([]ValueRef, len(expected))
	for index, value := range result.Values {
		if !value.IsValid() {
			panic(fmt.Errorf("native callback result %d is invalid", index))
		}
		converted, err := convertedValue(value, expected[index], index)
		if err != nil {
			panic(fmt.Errorf("native callback result %d has type %s, want %s", index, value.Type(), expected[index]))
		}
		values[index] = Clone(converted)
	}
	return values
}

func InvokeCallback(token *CallbackToken, method string, arguments []ValueRef, results []reflect.Type) []ValueRef {
	if token == nil || token.dispatch == nil {
		panic(fmt.Errorf("native callback token is invalid"))
	}
	invocation := CallbackInvocation{
		Target:    token.target,
		Method:    method,
		Arguments: clonedValues(arguments),
	}
	return callbackReturnedValues(token.dispatch(invocation), results)
}

func MakeCallback(functionType reflect.Type, token *CallbackToken) (ValueRef, error) {
	if functionType == nil || functionType.Kind() != reflect.Func {
		return reflect.Value{}, fmt.Errorf("native callback type is not func")
	}
	if token == nil || token.dispatch == nil {
		return reflect.Value{}, fmt.Errorf("native callback token is invalid")
	}
	results := make([]reflect.Type, functionType.NumOut())
	for index := range results {
		results[index] = functionType.Out(index)
	}
	callback := reflect.MakeFunc(functionType, func(arguments []reflect.Value) []reflect.Value {
		return InvokeCallback(token, "", arguments, results)
	})
	return callback, nil
}

func MakeCallbackValue(prototype any, token *CallbackToken) (ValueRef, error) {
	return MakeCallback(reflect.TypeOf(prototype), token)
}

func NewCallbackQueue(capacity int) (*CallbackQueue, error) {
	if capacity < 0 {
		return nil, fmt.Errorf("native callback queue capacity is negative")
	}
	return &CallbackQueue{
		requests: make(chan *PendingCallback, capacity),
		closed:   make(chan struct{}),
	}, nil
}

func NewCallbackQueueWithCompletion(completionValue any, capacity int) any {
	completion, ok := invocationQueueValue(completionValue)
	if !ok {
		return reflect.Value{}
	}
	queue, err := NewCallbackQueue(capacity)
	if err != nil {
		return reflect.Value{}
	}
	queue.notify = completion
	return reflect.ValueOf(queue)
}

func callbackQueueValue(value any) (*CallbackQueue, bool) {
	ref, ok := value.(reflect.Value)
	if !ok || !ref.IsValid() || ref.Kind() != reflect.Pointer || ref.IsNil() {
		return nil, false
	}
	queue, ok := ref.Interface().(*CallbackQueue)
	return queue, ok
}

func CallbackQueueToken(queueValue any) any {
	queue, ok := callbackQueueValue(queueValue)
	if !ok {
		return reflect.Value{}
	}
	token, err := queue.NewToken(nil)
	if err != nil {
		return reflect.Value{}
	}
	return reflect.ValueOf(token)
}

func CallbackQueueTryRequest(queueValue any) any {
	queue, ok := callbackQueueValue(queueValue)
	if !ok {
		return reflect.Value{}
	}
	request, ready := queue.TryNext()
	if !ready {
		return reflect.Value{}
	}
	return reflect.ValueOf(request)
}

func CallbackRequestArgumentCount(requestValue any) int {
	request, ok := pendingCallbackValue(requestValue)
	if !ok {
		return 0
	}
	return len(request.invocation.Arguments)
}

func CallbackRequestMethod(requestValue any) string {
	request, ok := pendingCallbackValue(requestValue)
	if !ok {
		return ""
	}
	return request.invocation.Method
}

func CallbackRequestArgument(requestValue any, index int) any {
	request, ok := pendingCallbackValue(requestValue)
	if !ok || index < 0 || index >= len(request.invocation.Arguments) {
		return reflect.Value{}
	}
	return request.invocation.Arguments[index]
}

func CallbackRequestWriteArgument(requestValue any, index int, value any) bool {
	request, ok := pendingCallbackValue(requestValue)
	if !ok || index < 0 || index >= len(request.invocation.Arguments) {
		return false
	}
	source, ok := reflectedValue(value)
	if !ok {
		return false
	}
	target := request.invocation.Arguments[index]
	if target.Kind() == reflect.Slice && source.Kind() == reflect.Slice {
		converted, err := convertedValue(source, target.Type(), index)
		if err != nil {
			return false
		}
		reflect.Copy(target, converted)
		return true
	}
	return true
}

func NewCallbackCompletion(requestValue any) any {
	request, ok := pendingCallbackValue(requestValue)
	if !ok {
		return reflect.Value{}
	}
	return reflect.ValueOf(&CallbackCompletion{request: request})
}

func CallbackCompletionAppend(completionValue any, value any) {
	completion, ok := callbackCompletionValue(completionValue)
	if !ok {
		return
	}
	ref, ok := reflectedValue(value)
	if !ok {
		completion.result.Err = fmt.Errorf("native callback result is invalid")
		return
	}
	completion.result.Values = append(completion.result.Values, Clone(ref))
}

func CallbackCompletionPanic(completionValue any, value any) {
	completion, ok := callbackCompletionValue(completionValue)
	if !ok {
		return
	}
	ref, valid := reflectedValue(value)
	if valid {
		completion.result.PanicValue = ref.Interface()
	}
	completion.result.Panicked = true
}

func CallbackCompletionExit(completionValue any, code int) {
	completion, ok := callbackCompletionValue(completionValue)
	if !ok {
		return
	}
	completion.result.Exited = true
	completion.result.ExitCode = code
}

func CallbackCompletionError(completionValue any, message string) {
	completion, ok := callbackCompletionValue(completionValue)
	if !ok {
		return
	}
	completion.result.Err = fmt.Errorf("%s", message)
}

func CallbackCompletionFinish(completionValue any) bool {
	completion, ok := callbackCompletionValue(completionValue)
	return ok && completion.request.Complete(completion.result)
}

func pendingCallbackValue(value any) (*PendingCallback, bool) {
	ref, ok := value.(reflect.Value)
	if !ok || !ref.IsValid() || ref.Kind() != reflect.Pointer || ref.IsNil() {
		return nil, false
	}
	request, ok := ref.Interface().(*PendingCallback)
	return request, ok
}

func callbackCompletionValue(value any) (*CallbackCompletion, bool) {
	ref, ok := value.(reflect.Value)
	if !ok || !ref.IsValid() || ref.Kind() != reflect.Pointer || ref.IsNil() {
		return nil, false
	}
	completion, ok := ref.Interface().(*CallbackCompletion)
	return completion, ok
}

func (queue *CallbackQueue) Dispatch(invocation CallbackInvocation) CallResult {
	if queue == nil {
		return CallResult{Err: fmt.Errorf("native callback queue is nil")}
	}
	request := &PendingCallback{
		invocation: CallbackInvocation{
			Target:    invocation.Target,
			Method:    invocation.Method,
			Arguments: clonedValues(invocation.Arguments),
		},
		finished: make(chan struct{}),
	}
	select {
	case queue.requests <- request:
		if queue.notify != nil {
			_ = queue.notify.Submit(AsyncCompletion{Target: request})
		}
	case <-queue.closed:
		request.cancel()
		<-request.finished
		return request.result
	}
	select {
	case <-request.finished:
		return request.result
	case <-queue.closed:
		request.cancel()
		<-request.finished
		return request.result
	}
}

func (queue *CallbackQueue) NewToken(target any) (*CallbackToken, error) {
	if queue == nil {
		return nil, fmt.Errorf("native callback queue is nil")
	}
	return NewCallbackToken(target, queue.Dispatch)
}

func (queue *CallbackQueue) Next() (*PendingCallback, bool) {
	if queue == nil {
		return nil, false
	}
	select {
	case request := <-queue.requests:
		return request, true
	default:
	}
	select {
	case request := <-queue.requests:
		return request, true
	case <-queue.closed:
		return nil, false
	}
}

func (queue *CallbackQueue) TryNext() (*PendingCallback, bool) {
	if queue == nil {
		return nil, false
	}
	select {
	case request := <-queue.requests:
		return request, true
	default:
		return nil, false
	}
}

func (queue *CallbackQueue) Closed() bool {
	if queue == nil {
		return true
	}
	select {
	case <-queue.closed:
		return true
	default:
		return false
	}
}

func (queue *CallbackQueue) Close() {
	if queue == nil {
		return
	}
	queue.once.Do(func() {
		close(queue.closed)
	})
}

func CallbackQueueClose(queueValue any) {
	queue, ok := callbackQueueValue(queueValue)
	if ok {
		queue.Close()
	}
}

func (request *PendingCallback) Invocation() CallbackInvocation {
	if request == nil {
		return CallbackInvocation{}
	}
	return CallbackInvocation{
		Target:    request.invocation.Target,
		Method:    request.invocation.Method,
		Arguments: clonedValues(request.invocation.Arguments),
	}
}

func (request *PendingCallback) Complete(result CallResult) bool {
	if request == nil {
		return false
	}
	completed := false
	request.once.Do(func() {
		request.result = result
		completed = true
		close(request.finished)
	})
	return completed
}

func (request *PendingCallback) Canceled() bool {
	if request == nil {
		return true
	}
	select {
	case <-request.finished:
		return request.canceled
	default:
		return false
	}
}

func (request *PendingCallback) cancel() {
	request.once.Do(func() {
		request.result = CallResult{Err: fmt.Errorf("native callback queue is closed")}
		request.canceled = true
		close(request.finished)
	})
}

func NewCompletionQueue(capacity int) (*CompletionQueue, error) {
	if capacity < 0 {
		return nil, fmt.Errorf("native completion queue capacity is negative")
	}
	return &CompletionQueue{
		completions: make(chan AsyncCompletion, capacity),
		closed:      make(chan struct{}),
	}, nil
}

func (queue *CompletionQueue) Submit(completion AsyncCompletion) error {
	if queue == nil {
		return fmt.Errorf("native completion queue is nil")
	}
	select {
	case queue.completions <- completion:
		return nil
	case <-queue.closed:
		return fmt.Errorf("native completion queue is closed")
	}
}

func captureOperation(operation func() CallResult) (result CallResult) {
	completed := false
	defer func() {
		if completed {
			return
		}
		recovered := recover()
		if signal, ok := recovered.(exitSignal); ok {
			result.Exited = true
			result.ExitCode = signal.code
			return
		}
		result.Panicked = true
		result.PanicValue = recovered
	}()
	result = operation()
	completed = true
	return result
}

func (queue *CompletionQueue) Start(target any, operation func() CallResult) error {
	if queue == nil {
		return fmt.Errorf("native completion queue is nil")
	}
	if operation == nil {
		return fmt.Errorf("native asynchronous operation is nil")
	}
	if queue.Closed() {
		return fmt.Errorf("native completion queue is closed")
	}
	go func() {
		_ = queue.Submit(AsyncCompletion{Target: target, Result: captureOperation(operation)})
	}()
	return nil
}

func (queue *CompletionQueue) StartCall(target any, registry *Registry, id CallID, arguments []ValueRef, slice bool) error {
	if registry == nil {
		return fmt.Errorf("native registry is nil")
	}
	values := clonedValues(arguments)
	return queue.Start(target, func() CallResult {
		if slice {
			return registry.CallSlice(id, values)
		}
		return registry.Call(id, values)
	})
}

func (queue *CompletionQueue) Next() (AsyncCompletion, bool) {
	if queue == nil {
		return AsyncCompletion{}, false
	}
	select {
	case completion := <-queue.completions:
		return completion, true
	default:
	}
	select {
	case completion := <-queue.completions:
		return completion, true
	case <-queue.closed:
		return AsyncCompletion{}, false
	}
}

func (queue *CompletionQueue) TryNext() (AsyncCompletion, bool) {
	if queue == nil {
		return AsyncCompletion{}, false
	}
	select {
	case completion := <-queue.completions:
		return completion, true
	default:
		return AsyncCompletion{}, false
	}
}

func (queue *CompletionQueue) Closed() bool {
	if queue == nil {
		return true
	}
	select {
	case <-queue.closed:
		return true
	default:
		return false
	}
}

func (queue *CompletionQueue) Close() {
	if queue == nil {
		return
	}
	queue.once.Do(func() {
		close(queue.closed)
	})
}

func aggregateElementTypes(typeValue reflect.Type) ([]reflect.Type, error) {
	switch typeValue.Kind() {
	case reflect.Array:
		result := make([]reflect.Type, typeValue.Len())
		for index := range result {
			result[index] = typeValue.Elem()
		}
		return result, nil
	case reflect.Struct:
		result := make([]reflect.Type, typeValue.NumField())
		for index := range result {
			result[index] = typeValue.Field(index).Type
		}
		return result, nil
	default:
		return nil, fmt.Errorf("native aggregate type %s is not array or struct", typeValue)
	}
}

func NewAggregateDescriptor(typeValue reflect.Type, pack AggregatePack, unpack AggregateUnpack) (*AggregateDescriptor, error) {
	if typeValue == nil {
		return nil, fmt.Errorf("native aggregate type is nil")
	}
	if pack == nil || unpack == nil {
		return nil, fmt.Errorf("native aggregate codec is nil")
	}
	elements, err := aggregateElementTypes(typeValue)
	if err != nil {
		return nil, err
	}
	return &AggregateDescriptor{typeValue: typeValue, elements: elements, pack: pack, unpack: unpack}, nil
}

func NewReflectAggregateDescriptor(typeValue reflect.Type) (*AggregateDescriptor, error) {
	if typeValue == nil {
		return nil, fmt.Errorf("native aggregate type is nil")
	}
	elements, err := aggregateElementTypes(typeValue)
	if err != nil {
		return nil, err
	}
	if typeValue.Kind() == reflect.Struct {
		for index := 0; index < typeValue.NumField(); index++ {
			if typeValue.Field(index).PkgPath != "" {
				return nil, fmt.Errorf("native aggregate field %s is not exported", typeValue.Field(index).Name)
			}
		}
	}
	descriptor, err := NewAggregateDescriptor(
		typeValue,
		func(values []ValueRef) (ValueRef, error) {
			result := reflect.New(typeValue).Elem()
			for index, value := range values {
				if typeValue.Kind() == reflect.Array {
					result.Index(index).Set(value)
				} else {
					result.Field(index).Set(value)
				}
			}
			return result, nil
		},
		func(value ValueRef) ([]ValueRef, error) {
			result := make([]ValueRef, len(elements))
			for index := range result {
				if typeValue.Kind() == reflect.Array {
					result[index] = Clone(value.Index(index))
				} else {
					result[index] = Clone(value.Field(index))
				}
			}
			return result, nil
		},
	)
	if err != nil {
		return nil, err
	}
	return descriptor, nil
}

func (descriptor *AggregateDescriptor) Type() reflect.Type {
	if descriptor == nil {
		return nil
	}
	return descriptor.typeValue
}

func (descriptor *AggregateDescriptor) Pack(values []ValueRef) (ValueRef, error) {
	if descriptor == nil {
		return reflect.Value{}, fmt.Errorf("native aggregate descriptor is nil")
	}
	if len(values) != len(descriptor.elements) {
		return reflect.Value{}, fmt.Errorf("native aggregate has %d elements, want %d", len(values), len(descriptor.elements))
	}
	converted := make([]ValueRef, len(values))
	for index, value := range values {
		if !value.IsValid() {
			return reflect.Value{}, fmt.Errorf("native aggregate element %d is invalid", index)
		}
		item, err := convertedValue(value, descriptor.elements[index], index)
		if err != nil {
			return reflect.Value{}, err
		}
		converted[index] = Clone(item)
	}
	result, err := descriptor.pack(converted)
	if err != nil {
		return reflect.Value{}, err
	}
	if !result.IsValid() || result.Type() != descriptor.typeValue {
		return reflect.Value{}, fmt.Errorf("native aggregate codec returned the wrong type")
	}
	return Clone(result), nil
}

func indirectInterface(value ValueRef) ValueRef {
	for value.IsValid() && value.Kind() == reflect.Interface && !value.IsNil() {
		value = value.Elem()
	}
	return value
}

func (descriptor *AggregateDescriptor) Unpack(value ValueRef) ([]ValueRef, error) {
	if descriptor == nil {
		return nil, fmt.Errorf("native aggregate descriptor is nil")
	}
	value = indirectInterface(value)
	if !value.IsValid() || value.Type() != descriptor.typeValue {
		return nil, fmt.Errorf("native aggregate value has the wrong type")
	}
	values, err := descriptor.unpack(Clone(value))
	if err != nil {
		return nil, err
	}
	if len(values) != len(descriptor.elements) {
		return nil, fmt.Errorf("native aggregate codec returned %d elements, want %d", len(values), len(descriptor.elements))
	}
	result := make([]ValueRef, len(values))
	for index, item := range values {
		if !item.IsValid() {
			return nil, fmt.Errorf("native aggregate element %d is invalid", index)
		}
		converted, err := convertedValue(item, descriptor.elements[index], index)
		if err != nil {
			return nil, err
		}
		result[index] = Clone(converted)
	}
	return result, nil
}

func NewInterfaceProxyDescriptor(interfaceType reflect.Type, factory ProxyFactory) (*InterfaceProxyDescriptor, error) {
	if interfaceType == nil || interfaceType.Kind() != reflect.Interface {
		return nil, fmt.Errorf("native proxy target is not interface")
	}
	if factory == nil {
		return nil, fmt.Errorf("native proxy factory is nil")
	}
	for index := 0; index < interfaceType.NumMethod(); index++ {
		if interfaceType.Method(index).PkgPath != "" {
			return nil, fmt.Errorf("native interface method %s is not exported", interfaceType.Method(index).Name)
		}
	}
	return &InterfaceProxyDescriptor{interfaceType: interfaceType, factory: factory}, nil
}

func (descriptor *InterfaceProxyDescriptor) Type() reflect.Type {
	if descriptor == nil {
		return nil
	}
	return descriptor.interfaceType
}

func (descriptor *InterfaceProxyDescriptor) Build(token *CallbackToken) (ValueRef, error) {
	if descriptor == nil {
		return reflect.Value{}, fmt.Errorf("native proxy descriptor is nil")
	}
	if token == nil || token.dispatch == nil {
		return reflect.Value{}, fmt.Errorf("native callback token is invalid")
	}
	result, err := descriptor.factory(token)
	if err != nil {
		return reflect.Value{}, err
	}
	if !result.IsValid() || !result.Type().Implements(descriptor.interfaceType) {
		return reflect.Value{}, fmt.Errorf("native proxy factory returned %s, which does not implement %s", valueTypeText(result), descriptor.interfaceType)
	}
	if nilableValue(result) && result.IsNil() {
		return reflect.Value{}, fmt.Errorf("native proxy factory returned nil")
	}
	return Clone(result), nil
}

func RegisterInterfaceProxy(prototype any, factory ProxyFactory) error {
	typeValue := reflect.TypeOf(prototype)
	if typeValue == nil || typeValue.Kind() != reflect.Pointer || typeValue.Elem().Kind() != reflect.Interface {
		return fmt.Errorf("native proxy prototype is not a pointer to interface")
	}
	descriptor, err := NewInterfaceProxyDescriptor(typeValue.Elem(), factory)
	if err != nil {
		return err
	}
	interfaceProxies.Lock()
	defer interfaceProxies.Unlock()
	if interfaceProxies.values == nil {
		interfaceProxies.values = make(map[reflect.Type]*InterfaceProxyDescriptor)
	}
	if _, exists := interfaceProxies.values[typeValue.Elem()]; exists {
		return fmt.Errorf("native proxy for %s is registered twice", typeValue.Elem())
	}
	interfaceProxies.values[typeValue.Elem()] = descriptor
	return nil
}

func MakeInterfaceProxy(interfaceType reflect.Type, token *CallbackToken) (ValueRef, error) {
	interfaceProxies.RLock()
	descriptor := interfaceProxies.values[interfaceType]
	interfaceProxies.RUnlock()
	if descriptor == nil {
		return reflect.Value{}, fmt.Errorf("native proxy for %s is not registered", interfaceType)
	}
	return descriptor.Build(token)
}

func CallbackValue[T any](values []ValueRef, index int) T {
	var zero T
	if index < 0 || index >= len(values) || !values[index].IsValid() {
		panic(fmt.Errorf("native callback result %d is invalid", index))
	}
	value := values[index]
	if nilableValue(value) && value.IsNil() {
		return zero
	}
	converted, ok := value.Interface().(T)
	if !ok {
		panic(fmt.Errorf("native callback result %d has type %s", index, value.Type()))
	}
	return converted
}

func RegisterAggregateType(key string, prototype any) error {
	if key == "" {
		return fmt.Errorf("native aggregate key is empty")
	}
	typeValue := reflect.TypeOf(prototype)
	if typeValue == nil || (typeValue.Kind() != reflect.Struct && typeValue.Kind() != reflect.Array) {
		return fmt.Errorf("native aggregate prototype is not struct or array")
	}
	aggregateTypes.Lock()
	defer aggregateTypes.Unlock()
	if aggregateTypes.values == nil {
		aggregateTypes.values = make(map[string]reflect.Type)
	}
	if _, exists := aggregateTypes.values[key]; exists {
		return fmt.Errorf("native aggregate %s is registered twice", key)
	}
	aggregateTypes.values[key] = typeValue
	return nil
}

func NewAggregateValue(key string) any {
	aggregateTypes.RLock()
	typeValue := aggregateTypes.values[key]
	aggregateTypes.RUnlock()
	if typeValue == nil {
		return reflect.Value{}
	}
	return reflect.New(typeValue).Elem()
}

func RegisterNativeType(key string, prototype any) error {
	if key == "" {
		return fmt.Errorf("native type key is empty")
	}
	typeValue := reflect.TypeOf(prototype)
	if typeValue == nil {
		return fmt.Errorf("native type prototype is nil")
	}
	nativeTypes.Lock()
	defer nativeTypes.Unlock()
	if nativeTypes.values == nil {
		nativeTypes.values = make(map[string]reflect.Type)
	}
	if existing, exists := nativeTypes.values[key]; exists {
		if existing == typeValue {
			return nil
		}
		return fmt.Errorf("native type %s is registered with conflicting types", key)
	}
	nativeTypes.values[key] = typeValue
	return nil
}

func NewNativeValue(key string) any {
	nativeTypes.RLock()
	typeValue := nativeTypes.values[key]
	nativeTypes.RUnlock()
	if typeValue == nil {
		return reflect.Value{}
	}
	return reflect.New(typeValue).Elem()
}

func AddressValue(value any) any {
	ref, ok := reflectedValue(value)
	if !ok || !ref.CanAddr() {
		return reflect.Value{}
	}
	return ref.Addr()
}

func IndirectValue(value any) any {
	ref, ok := reflectedValue(value)
	if !ok {
		return reflect.Value{}
	}
	ref = indirectInterface(ref)
	if ref.Kind() != reflect.Pointer || ref.IsNil() {
		return reflect.Value{}
	}
	return ref.Elem()
}

func FieldReference(value any, index int) any {
	ref, ok := reflectedValue(value)
	if !ok || index < 0 {
		return reflect.Value{}
	}
	ref = indirectInterface(ref)
	for ref.Kind() == reflect.Pointer {
		if ref.IsNil() {
			return reflect.Value{}
		}
		ref = indirectInterface(ref.Elem())
	}
	if ref.Kind() == reflect.Struct && index < ref.NumField() {
		return ref.Field(index)
	}
	if ref.Kind() == reflect.Array && index < ref.Len() {
		return ref.Index(index)
	}
	return reflect.Value{}
}

func SetValue(target any, source any) bool {
	destination, ok := reflectedValue(target)
	if !ok || !destination.CanSet() {
		return false
	}
	value, ok := reflectedValue(source)
	if !ok {
		return false
	}
	converted, err := convertedValue(value, destination.Type(), 0)
	if err != nil {
		return false
	}
	destination.Set(converted)
	return true
}

func SetAggregateValue(aggregate any, index int, element any) bool {
	target, ok := reflectedValue(aggregate)
	if !ok || index < 0 {
		return false
	}
	value, ok := reflectedValue(element)
	if !ok {
		return false
	}
	var field reflect.Value
	if target.Kind() == reflect.Struct && index < target.NumField() {
		field = target.Field(index)
	} else if target.Kind() == reflect.Array && index < target.Len() {
		field = target.Index(index)
	} else {
		return false
	}
	converted, err := convertedValue(value, field.Type(), index)
	if err != nil || !field.CanSet() {
		return false
	}
	field.Set(converted)
	return true
}

func ValueAggregateField(aggregate any, index int) any {
	value, ok := reflectedValue(aggregate)
	if !ok || index < 0 {
		return reflect.Value{}
	}
	value = indirectInterface(value)
	if value.Kind() == reflect.Struct && index < value.NumField() {
		return Clone(value.Field(index))
	}
	if value.Kind() == reflect.Array && index < value.Len() {
		return Clone(value.Index(index))
	}
	return reflect.Value{}
}

func valueTypeText(value reflect.Value) string {
	if !value.IsValid() {
		return "an invalid value"
	}
	return value.Type().String()
}

func nilableValue(value reflect.Value) bool {
	switch value.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Pointer, reflect.Slice:
		return true
	default:
		return false
	}
}

func MakeChannel(channelType reflect.Type, capacity int) (ValueRef, error) {
	if channelType == nil || channelType.Kind() != reflect.Chan {
		return reflect.Value{}, fmt.Errorf("native channel type is not chan")
	}
	if channelType.ChanDir() != reflect.BothDir {
		return reflect.Value{}, fmt.Errorf("native channel type is directional")
	}
	if capacity < 0 {
		return reflect.Value{}, fmt.Errorf("native channel capacity is negative")
	}
	return reflect.MakeChan(channelType, capacity), nil
}

func TryChannelSend(channel ValueRef, value ValueRef) (ready bool, result CallResult) {
	if !channel.IsValid() || channel.Kind() != reflect.Chan {
		return false, CallResult{Err: fmt.Errorf("native send target is not channel")}
	}
	if channel.Type().ChanDir() == reflect.RecvDir {
		return false, CallResult{Err: fmt.Errorf("native send target is receive-only")}
	}
	if !value.IsValid() {
		return false, CallResult{Err: fmt.Errorf("native send value is invalid")}
	}
	converted, err := convertedValue(value, channel.Type().Elem(), 0)
	if err != nil {
		return false, CallResult{Err: err}
	}
	completed := false
	defer func() {
		if completed {
			return
		}
		recovered := recover()
		result.Panicked = true
		result.PanicValue = recovered
	}()
	ready = channel.TrySend(converted)
	completed = true
	return ready, result
}

func TryChannelReceive(channel ValueRef) (result ChannelReceiveResult) {
	if !channel.IsValid() || channel.Kind() != reflect.Chan {
		result.Err = fmt.Errorf("native receive target is not channel")
		return result
	}
	if channel.Type().ChanDir() == reflect.SendDir {
		result.Err = fmt.Errorf("native receive target is send-only")
		return result
	}
	completed := false
	defer func() {
		if completed {
			return
		}
		result.Panicked = true
		result.PanicValue = recover()
	}()
	value, open := channel.TryRecv()
	result.Value = value
	result.Ready = value.IsValid()
	result.Open = open
	completed = true
	return result
}

func CloseChannel(channel ValueRef) (result CallResult) {
	if !channel.IsValid() || channel.Kind() != reflect.Chan {
		return CallResult{Err: fmt.Errorf("native close target is not channel")}
	}
	if channel.Type().ChanDir() == reflect.RecvDir {
		return CallResult{Err: fmt.Errorf("native close target is receive-only")}
	}
	completed := false
	defer func() {
		if completed {
			return
		}
		result.Panicked = true
		result.PanicValue = recover()
	}()
	channel.Close()
	completed = true
	return result
}

func TryChannelSelect(cases []ChannelSelectCase) (result ChannelSelectResult) {
	result.Index = -1
	for index, item := range cases {
		if !item.Channel.IsValid() || item.Channel.Kind() != reflect.Chan {
			result.Err = fmt.Errorf("native select case %d target is not channel", index)
			return result
		}
		switch item.Direction {
		case ChannelCaseReceive:
			if item.Channel.Type().ChanDir() == reflect.SendDir {
				result.Err = fmt.Errorf("native select case %d target is send-only", index)
				return result
			}
			received := TryChannelReceive(item.Channel)
			if received.Err != nil {
				result.Err = received.Err
				return result
			}
			if received.Panicked {
				result.Panicked = true
				result.PanicValue = received.PanicValue
				return result
			}
			if received.Ready {
				result.Index = index
				result.Value = received.Value
				result.Ready = true
				result.Open = received.Open
				return result
			}
		case ChannelCaseSend:
			if item.Channel.Type().ChanDir() == reflect.RecvDir {
				result.Err = fmt.Errorf("native select case %d target is receive-only", index)
				return result
			}
			if !item.Send.IsValid() {
				result.Err = fmt.Errorf("native select case %d send value is invalid", index)
				return result
			}
			ready, sent := TryChannelSend(item.Channel, item.Send)
			if sent.Err != nil {
				result.Err = sent.Err
				return result
			}
			if sent.Panicked {
				result.Panicked = true
				result.PanicValue = sent.PanicValue
				return result
			}
			if ready {
				result.Index = index
				result.Ready = true
				result.Open = true
				return result
			}
		default:
			result.Err = fmt.Errorf("native select case %d has invalid direction", index)
			return result
		}
	}
	return result
}
