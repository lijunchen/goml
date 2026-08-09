package main

import (
    _goml_context "context"
    _goml_fmt "fmt"
    _goml_sync "sync"
)

type _goml_task_scope_state struct {
    mu _goml_sync.Mutex
    wg _goml_sync.WaitGroup
    state int
    ctx _goml_context.Context
    cancel _goml_context.CancelFunc
    panicked bool
    panic_value any
}

var _goml_task_registry_mutex _goml_sync.Mutex = _goml_sync.Mutex{}

var _goml_task_registry map[int64]*_goml_task_scope_state = make(map[int64]*_goml_task_scope_state)

var _goml_task_next_id int64 = 1

func _goml_task_lookup(id int64) *_goml_task_scope_state {
    _goml_task_registry_mutex.Lock()
    var scope *_goml_task_scope_state = _goml_task_registry[id]
    _goml_task_registry_mutex.Unlock()
    return scope
}

func _goml_task_record_panic(scope *_goml_task_scope_state, value any) {
    scope.mu.Lock()
    if !scope.panicked {
        scope.panicked = true
        scope.panic_value = value
    }
    scope.mu.Unlock()
    scope.cancel()
}

func _goml_task_run(scope *_goml_task_scope_state, body func() struct{}) {
    defer func() {
        var recovered any = recover()
        if recovered != nil {
            _goml_task_record_panic(scope, recovered)
        }
        scope.wg.Done()
    }()
    body()
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_runtime_std_task_scope_new(parent int64) int64 {
    var parent_context _goml_context.Context = _goml_context.Background()
    var parent_missing bool = false
    if parent != 0 {
        var parent_scope *_goml_task_scope_state = _goml_task_lookup(parent)
        if parent_scope != nil {
            parent_context = parent_scope.ctx
        } else {
            parent_missing = true
        }
    }
    var ctx _goml_context.Context
    var cancel _goml_context.CancelFunc
    ctx, cancel = _goml_context.WithCancel(parent_context)
    if parent_missing {
        cancel()
    }
    _goml_task_registry_mutex.Lock()
    var id int64 = _goml_task_next_id
    _goml_task_next_id = _goml_task_next_id + 1
    var scope *_goml_task_scope_state = &_goml_task_scope_state{
        state: 0,
        ctx: ctx,
        cancel: cancel,
    }
    _goml_task_registry[id] = scope
    _goml_task_registry_mutex.Unlock()
    return id
}

func _goml_runtime_std_task_scope_run(scope_id int64, body func() struct{}) struct{} {
    var scope *_goml_task_scope_state = _goml_task_lookup(scope_id)
    func() {
        defer func() {
            var recovered any = recover()
            if recovered != nil {
                _goml_task_record_panic(scope, recovered)
            }
        }()
        body()
    }()
    return struct{}{}
}

func _goml_runtime_std_task_scope_spawn(scope_id int64, body func() struct{}) bool {
    var scope *_goml_task_scope_state = _goml_task_lookup(scope_id)
    if scope == nil {
        panic("task scope is closed")
    }
    scope.mu.Lock()
    if scope.state != 0 {
        scope.mu.Unlock()
        panic("task scope is closed")
    }
    scope.wg.Add(1)
    scope.mu.Unlock()
    go _goml_task_run(scope, body)
    return true
}

func _goml_runtime_std_task_scope_finish(scope_id int64) struct{} {
    var scope *_goml_task_scope_state = _goml_task_lookup(scope_id)
    if scope == nil {
        return struct{}{}
    }
    scope.mu.Lock()
    if scope.state == 0 {
        scope.state = 1
    }
    scope.mu.Unlock()
    scope.wg.Wait()
    scope.cancel()
    scope.mu.Lock()
    var panicked bool = scope.panicked
    var panic_value any = scope.panic_value
    scope.state = 2
    scope.mu.Unlock()
    _goml_task_registry_mutex.Lock()
    delete(_goml_task_registry, scope_id)
    _goml_task_registry_mutex.Unlock()
    if panicked {
        panic(panic_value)
    }
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Option__int_x struct {
    value Option__int
}

func ref__Ref_11Option__int(value Option__int) *ref_Option__int_x {
    return &ref_Option__int_x{
        value: value,
    }
}

func ref_get__Ref_11Option__int(reference *ref_Option__int_x) Option__int {
    return reference.value
}

func ref_set__Ref_11Option__int(reference *ref_Option__int_x, value Option__int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Option__unit_x struct {
    value Option__unit
}

func ref__Ref_12Option__unit(value Option__unit) *ref_Option__unit_x {
    return &ref_Option__unit_x{
        value: value,
    }
}

func ref_set__Ref_12Option__unit(reference *ref_Option__unit_x, value Option__unit) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_4unit_4bool struct {
    _0 struct{}
    _1 bool
}

type _goml_m_std_p_internal_p_task_p_ScopeHandle struct {
    id int64
}

type _goml_m_std_p_internal_p_task_p_CancelToken struct {
    id int64
}

type _goml_m_std_p_task_p_CancelToken struct {
    value _goml_m_std_p_internal_p_task_p_CancelToken
}

type _goml_m_std_p_task_p_Scope struct {
    handle _goml_m_std_p_internal_p_task_p_ScopeHandle
}

type _goml_m_std_p_task_p_ConcurrencyLimit struct {
    slots chan struct{}
}

type _goml_m_std_p_task_p_Task____int struct {
    result *ref_Option__int_x
    ready chan struct{}
}

type _goml_m_std_p_task_p_Task____unit struct {
    result *ref_Option__unit_x
    ready chan struct{}
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {
    completed_0 *ref_bool_x
}

type closure_env_main_3 struct {
    completed_0 *ref_bool_x
}

type closure_env_main_4 struct {}

type closure_env_main_5 struct {}

type closure_env_main_6 struct {}

type closure_env_spawn_7 struct {}

type closure_env_std_task_scope_T_int_8 struct {
    result_0 *ref_Option__int_x
    body_1 func(_goml_m_std_p_task_p_Scope) int
    handle_2 _goml_m_std_p_internal_p_task_p_ScopeHandle
}

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 struct {
    result_0 *ref_Option__int_x
    body_1 func(_goml_m_std_p_task_p_CancelToken) int
    token_2 _goml_m_std_p_task_p_CancelToken
    ready_3 chan struct{}
}

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 struct {
    result_0 *ref_Option__unit_x
    body_1 func(_goml_m_std_p_task_p_CancelToken) struct{}
    token_2 _goml_m_std_p_task_p_CancelToken
    ready_3 chan struct{}
}

type closure_env_std_task_scope_with_T_int_11 struct {
    result_0 *ref_Option__int_x
    body_1 func(_goml_m_std_p_task_p_Scope) int
    handle_2 _goml_m_std_p_internal_p_task_p_ScopeHandle
}

type _goml_m_std_p_task_p_WaitResult____unit interface {
    is_goml_m_std_p_task_p_WaitResult____unit()
}

type Completed struct {
    _0 struct{}
}

func (_ Completed) is_goml_m_std_p_task_p_WaitResult____unit() {}

type Cancelled struct {}

func (_ Cancelled) is_goml_m_std_p_task_p_WaitResult____unit() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Option__unit interface {
    isOption__unit()
}

type Option__unit_None struct {}

func (_ Option__unit_None) isOption__unit() {}

type Option__unit_Some struct {
    _0 struct{}
}

func (_ Option__unit_Some) isOption__unit() {}

func _goml_m_std_p_internal_p_task_p_root__scope() _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t190 int64 = _goml_runtime_std_task_scope_new(0)
    var t191 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t190,
    }
    return t191
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__0 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t194 int64 = parent__0.id
    var t195 int64 = _goml_runtime_std_task_scope_new(t194)
    var t196 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t195,
    }
    return t196
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__1 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__2 func() struct{}) struct{} {
    var t198 int64 = scope__1.id
    _goml_runtime_std_task_scope_spawn(t198, body__2)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__5 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__6 func() struct{}) struct{} {
    var t204 int64 = scope__5.id
    _goml_runtime_std_task_scope_run(t204, body__6)
    var t205 int64 = scope__5.id
    _goml_runtime_std_task_scope_finish(t205)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__9 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t215 int64 = scope__9.id
    var t216 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t215,
    }
    return t216
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline514 bool = false
    var inline515 *ref_bool_x = ref__Ref_4bool(inline514)
    completed__0 = inline515
    var t269 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t270 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t269, p0)
    }
    var total__7 int
    var inline507 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline508 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline509 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline508,
        body_1: t270,
        handle_2: inline507,
    }
    var inline510 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline509)
    }
    _goml_m_std_p_internal_p_task_p_run(inline507, inline510)
    var inline512 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline508)
    total__7 = inline512
    var inline504 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline504)
    var t271 bool
    var inline502 bool = ref_get__Ref_4bool(completed__0)
    t271 = inline502
    var inline499 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t271)
    _goml_runtime_core_string_println(inline499)
    var t272 closure_env_main_6 = closure_env_main_6{}
    var t273 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t272, p0)
    }
    var nested__12 int
    var inline492 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline493 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline494 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline493,
        body_1: t273,
        handle_2: inline492,
    }
    var inline495 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline494)
    }
    _goml_m_std_p_internal_p_task_p_run(inline492, inline495)
    var inline497 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline493)
    nested__12 = inline497
    var inline489 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline489)
    var scope__13 int = 3
    var t274 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t274, p0)
    }
    var t275 int = spawn__15(scope__13)
    var inline486 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t275)
    _goml_runtime_core_string_println(inline486)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__261 int) chan struct{} {
    var t279 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__261)
    return t279
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__53 _goml_m_std_p_task_p_Task____int) int {
    var jp313 int
    Loop_loop_expr314:
    for {
        var t315 chan struct{} = self__53.ready
        var inline551 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t315)
        var inline553 bool = inline551._1
        if inline553 {} else {}
        var t316 *ref_Option__int_x = self__53.result
        var mtmp26 Option__int
        var inline549 Option__int = ref_get__Ref_11Option__int(t316)
        mtmp26 = inline549
        switch mtmp26.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x27 int = mtmp26.(Option__int_Some)._0
            jp313 = x27
            break Loop_loop_expr314
        default:
            panic("non-exhaustive match")
        }
    }
    return jp313
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__257 Option__int) *ref_Option__int_x {
    var t335 *ref_Option__int_x = ref__Ref_11Option__int(value__257)
    return t335
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp341 int
    Loop_loop_expr342:
    for {
        var mtmp0 Option__int
        var inline573 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline573
        switch mtmp0.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x1 int = mtmp0.(Option__int_Some)._0
            jp341 = x1
            break Loop_loop_expr342
        default:
            panic("non-exhaustive match")
        }
    }
    return jp341
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__257 Option__unit) *ref_Option__unit_x {
    var t348 *ref_Option__unit_x = ref__Ref_12Option__unit(value__257)
    return t348
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t361 string = _goml_runtime_core_int_to_string(self__69)
    return t361
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t364 string = _goml_runtime_core_bool_to_string(self__66)
    return t364
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env176 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env177 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env178 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env178.completed_0
    var inline581 bool = true
    ref_set__Ref_4bool(completed__0, inline581)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env179 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env179.completed_0
    var t420 closure_env_main_0 = closure_env_main_0{}
    var t421 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t420, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____int
    var inline606 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline607 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline608 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline609 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline608)
    var inline610 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline609,
    }
    var inline611 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline612 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline606,
        body_1: t421,
        token_2: inline610,
        ready_3: inline607,
    }
    var inline613 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline612)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline611, inline613)
    var inline615 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline606,
        ready: inline607,
    }
    left__3 = inline615
    var t422 closure_env_main_1 = closure_env_main_1{}
    var t423 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t422, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____int
    var inline595 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline596 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline597 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline598 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline597)
    var inline599 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline598,
    }
    var inline600 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline601 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline595,
        body_1: t423,
        token_2: inline599,
        ready_3: inline596,
    }
    var inline602 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline601)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline600, inline602)
    var inline604 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline595,
        ready: inline596,
    }
    right__5 = inline604
    var t424 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t425 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t424, p0)
    }
    var inline584 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit_None{})
    var inline585 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline586 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline587 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline586)
    var inline588 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline587,
    }
    var inline589 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline590 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline584,
        body_1: t425,
        token_2: inline588,
        ready_3: inline585,
    }
    var inline591 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline590)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline589, inline591)
    var t426 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t427 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t428 int = t426 + t427
    return t428
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env180 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env181 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t433 closure_env_main_4 = closure_env_main_4{}
    var t434 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t433, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____int
    var inline617 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline618 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline619 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline620 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline619)
    var inline621 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline620,
    }
    var inline622 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline623 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline617,
        body_1: t434,
        token_2: inline621,
        ready_3: inline618,
    }
    var inline624 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline623)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline622, inline624)
    var inline626 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline617,
        ready: inline618,
    }
    value__11 = inline626
    var t435 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t435
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env182 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t439 _goml_m_std_p_task_p_CancelToken
    var inline636 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline637 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline636)
    var inline638 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline637,
    }
    t439 = inline638
    var t440 closure_env_main_5 = closure_env_main_5{}
    var t441 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t440, p0)
    }
    var inline628 _goml_m_std_p_internal_p_task_p_CancelToken = t439.value
    var inline629 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline628)
    var inline630 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline631 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: inline630,
        body_1: t441,
        handle_2: inline629,
    }
    var inline632 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(inline631)
    }
    _goml_m_std_p_internal_p_task_p_run(inline629, inline632)
    var inline634 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline630)
    return inline634
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env183 closure_env_spawn_7, value__14 int) int {
    var t445 int = value__14 + 1
    return t445
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env184 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env184.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env184.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env184.handle_2
    var t447 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t448 int = body__9(t447)
    var t449 Option__int = Option__int_Some{
        _0: t448,
    }
    ref_set__Ref_11Option__int(result__11, t449)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env185 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env185.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env185.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env185.token_2
    var ready__28 chan struct{} = env185.ready_3
    var t452 int = body__26(token__29)
    var t453 Option__int = Option__int_Some{
        _0: t452,
    }
    ref_set__Ref_11Option__int(result__27, t453)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env186 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env186.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env186.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env186.token_2
    var ready__28 chan struct{} = env186.ready_3
    var t456 struct{} = body__26(token__29)
    var t457 Option__unit = Option__unit_Some{
        _0: t456,
    }
    ref_set__Ref_12Option__unit(result__27, t457)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(env187 closure_env_std_task_scope_with_T_int_11) struct{} {
    var result__15 *ref_Option__int_x = env187.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env187.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env187.handle_2
    var t460 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t461 int = body__13(t460)
    var t462 Option__int = Option__int_Some{
        _0: t461,
    }
    ref_set__Ref_11Option__int(result__15, t462)
    return struct{}{}
}

func main() {
    main0()
}
