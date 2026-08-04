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

func _goml_m_std_p_internal_p_task_p_spawn(scope__1 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__2 func() struct{}) struct{} {
    var t162 int64 = scope__1.id
    _goml_runtime_std_task_scope_spawn(t162, body__2)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__5 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__6 func() struct{}) struct{} {
    var t168 int64 = scope__5.id
    _goml_runtime_std_task_scope_run(t168, body__6)
    var t169 int64 = scope__5.id
    _goml_runtime_std_task_scope_finish(t169)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__9 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t179 int64 = scope__9.id
    var t180 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t179,
    }
    return t180
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline411 bool = false
    var inline412 *ref_bool_x = ref__Ref_4bool(inline411)
    completed__0 = inline412
    var t233 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var total__7 int = _goml_m_std_p_task_p_scope____T__int(func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t233, p0)
    })
    var inline408 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline408)
    var t234 bool
    var inline406 bool = ref_get__Ref_4bool(completed__0)
    t234 = inline406
    var inline403 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t234)
    _goml_runtime_core_string_println(inline403)
    var t235 closure_env_main_6 = closure_env_main_6{}
    var nested__12 int = _goml_m_std_p_task_p_scope____T__int(func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t235, p0)
    })
    var inline400 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline400)
    var scope__13 int = 3
    var t236 int
    var inline398 int = scope__13 + 1
    t236 = inline398
    var inline395 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t236)
    _goml_runtime_core_string_println(inline395)
    return struct{}{}
}

func _goml_m_std_p_task_p_scope____T__int(body__9 func(_goml_m_std_p_task_p_Scope) int) int {
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle
    var inline416 int64 = _goml_runtime_std_task_scope_new(0)
    var inline417 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: inline416,
    }
    handle__10 = inline417
    var result__11 *ref_Option__int_x
    var inline414 *ref_Option__int_x = ref__Ref_11Option__int(Option__int_None{})
    result__11 = inline414
    var t248 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: result__11,
        body_1: body__9,
        handle_2: handle__10,
    }
    _goml_m_std_p_internal_p_task_p_run(handle__10, func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(t248)
    })
    var t249 int = _goml_m_std_p_task_p_completed__scope__value____T__int(result__11)
    return t249
}

func _goml_m_inherent_i_std_p_task_p_Scope_i_std_p_task_p_Scope_i_spawn____T__int(self__25 _goml_m_std_p_task_p_Scope, body__26 func(_goml_m_std_p_task_p_CancelToken) int) _goml_m_std_p_task_p_Task____int {
    var result__27 *ref_Option__int_x
    var inline425 *ref_Option__int_x = ref__Ref_11Option__int(Option__int_None{})
    result__27 = inline425
    var ready__28 chan struct{}
    var inline422 int = 0
    var inline423 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(inline422)
    ready__28 = inline423
    var t252 _goml_m_std_p_internal_p_task_p_ScopeHandle = self__25.handle
    var t253 _goml_m_std_p_internal_p_task_p_CancelToken
    var inline419 int64 = t252.id
    var inline420 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: inline419,
    }
    t253 = inline420
    var token__29 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: t253,
    }
    var t254 _goml_m_std_p_internal_p_task_p_ScopeHandle = self__25.handle
    var t255 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: result__27,
        body_1: body__26,
        token_2: token__29,
        ready_3: ready__28,
    }
    _goml_m_std_p_internal_p_task_p_spawn(t254, func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(t255)
    })
    var t256 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: result__27,
        ready: ready__28,
    }
    return t256
}

func _goml_m_inherent_i_std_p_task_p_Scope_i_std_p_task_p_Scope_i_spawn____T__unit(self__25 _goml_m_std_p_task_p_Scope, body__26 func(_goml_m_std_p_task_p_CancelToken) struct{}) _goml_m_std_p_task_p_Task____unit {
    var result__27 *ref_Option__unit_x
    var inline433 *ref_Option__unit_x = ref__Ref_12Option__unit(Option__unit_None{})
    result__27 = inline433
    var ready__28 chan struct{}
    var inline430 int = 0
    var inline431 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(inline430)
    ready__28 = inline431
    var t261 _goml_m_std_p_internal_p_task_p_ScopeHandle = self__25.handle
    var t262 _goml_m_std_p_internal_p_task_p_CancelToken
    var inline427 int64 = t261.id
    var inline428 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: inline427,
    }
    t262 = inline428
    var token__29 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: t262,
    }
    var t263 _goml_m_std_p_internal_p_task_p_ScopeHandle = self__25.handle
    var t264 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: result__27,
        body_1: body__26,
        token_2: token__29,
        ready_3: ready__28,
    }
    _goml_m_std_p_internal_p_task_p_spawn(t263, func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(t264)
    })
    var t265 _goml_m_std_p_task_p_Task____unit = _goml_m_std_p_task_p_Task____unit{
        result: result__27,
        ready: ready__28,
    }
    return t265
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__53 _goml_m_std_p_task_p_Task____int) int {
    var jp269 int
    Loop_loop_expr270:
    for {
        var t271 chan struct{} = self__53.ready
        var inline437 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t271)
        var inline439 bool = inline437._1
        if inline439 {} else {}
        var t272 *ref_Option__int_x = self__53.result
        var mtmp26 Option__int
        var inline435 Option__int = ref_get__Ref_11Option__int(t272)
        mtmp26 = inline435
        switch mtmp26.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x27 int = mtmp26.(Option__int_Some)._0
            jp269 = x27
            break Loop_loop_expr270
        default:
            panic("non-exhaustive match")
        }
    }
    return jp269
}

func _goml_m_std_p_task_p_scope__with____T__int(parent__12 _goml_m_std_p_task_p_CancelToken, body__13 func(_goml_m_std_p_task_p_Scope) int) int {
    var t285 _goml_m_std_p_internal_p_task_p_CancelToken = parent__12.value
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle
    var inline450 int64 = t285.id
    var inline451 int64 = _goml_runtime_std_task_scope_new(inline450)
    var inline452 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: inline451,
    }
    handle__14 = inline452
    var result__15 *ref_Option__int_x
    var inline448 *ref_Option__int_x = ref__Ref_11Option__int(Option__int_None{})
    result__15 = inline448
    var t286 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: result__15,
        body_1: body__13,
        handle_2: handle__14,
    }
    _goml_m_std_p_internal_p_task_p_run(handle__14, func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(t286)
    })
    var t287 int = _goml_m_std_p_task_p_completed__scope__value____T__int(result__15)
    return t287
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp296 int
    Loop_loop_expr297:
    for {
        var mtmp0 Option__int
        var inline454 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline454
        switch mtmp0.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x1 int = mtmp0.(Option__int_Some)._0
            jp296 = x1
            break Loop_loop_expr297
        default:
            panic("non-exhaustive match")
        }
    }
    return jp296
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t316 string = _goml_runtime_core_int_to_string(self__69)
    return t316
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t319 string = _goml_runtime_core_bool_to_string(self__66)
    return t319
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env140 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env141 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env142 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env142.completed_0
    var inline456 bool = true
    ref_set__Ref_4bool(completed__0, inline456)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env143 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env143.completed_0
    var t335 closure_env_main_0 = closure_env_main_0{}
    var left__3 _goml_m_std_p_task_p_Task____int = _goml_m_inherent_i_std_p_task_p_Scope_i_std_p_task_p_Scope_i_spawn____T__int(scope__1, func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t335, p0)
    })
    var t336 closure_env_main_1 = closure_env_main_1{}
    var right__5 _goml_m_std_p_task_p_Task____int = _goml_m_inherent_i_std_p_task_p_Scope_i_std_p_task_p_Scope_i_spawn____T__int(scope__1, func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t336, p0)
    })
    var t337 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    _goml_m_inherent_i_std_p_task_p_Scope_i_std_p_task_p_Scope_i_spawn____T__unit(scope__1, func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t337, p0)
    })
    var t338 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t339 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t340 int = t338 + t339
    return t340
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env144 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env145 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t345 closure_env_main_4 = closure_env_main_4{}
    var value__11 _goml_m_std_p_task_p_Task____int = _goml_m_inherent_i_std_p_task_p_Scope_i_std_p_task_p_Scope_i_spawn____T__int(scope__9, func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t345, p0)
    })
    var t346 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t346
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env146 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t349 _goml_m_std_p_task_p_CancelToken
    var inline459 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline460 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline459)
    var inline461 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline460,
    }
    t349 = inline461
    var t350 closure_env_main_5 = closure_env_main_5{}
    var t351 int = _goml_m_std_p_task_p_scope__with____T__int(t349, func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t350, p0)
    })
    return t351
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env148 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env148.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env148.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env148.handle_2
    var t356 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t357 int = body__9(t356)
    var t358 Option__int = Option__int_Some{
        _0: t357,
    }
    ref_set__Ref_11Option__int(result__11, t358)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env149 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env149.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env149.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env149.token_2
    var ready__28 chan struct{} = env149.ready_3
    var t361 int = body__26(token__29)
    var t362 Option__int = Option__int_Some{
        _0: t361,
    }
    ref_set__Ref_11Option__int(result__27, t362)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env150 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env150.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env150.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env150.token_2
    var ready__28 chan struct{} = env150.ready_3
    var t365 struct{} = body__26(token__29)
    var t366 Option__unit = Option__unit_Some{
        _0: t365,
    }
    ref_set__Ref_12Option__unit(result__27, t366)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(env151 closure_env_std_task_scope_with_T_int_11) struct{} {
    var result__15 *ref_Option__int_x = env151.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env151.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env151.handle_2
    var t369 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t370 int = body__13(t369)
    var t371 Option__int = Option__int_Some{
        _0: t370,
    }
    ref_set__Ref_11Option__int(result__15, t371)
    return struct{}{}
}

func main() {
    main0()
}
