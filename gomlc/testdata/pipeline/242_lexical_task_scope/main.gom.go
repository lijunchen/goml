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
    var t229 int64
    var inline504 int64 = 0
    var inline505 int64 = _goml_runtime_std_task_scope_new(inline504)
    t229 = inline505
    var t230 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t229,
    }
    return t230
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__12 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t233 int64 = parent__12.id
    var t234 int64
    var inline507 int64 = _goml_runtime_std_task_scope_new(t233)
    t234 = inline507
    var t235 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t234,
    }
    return t235
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__13 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__14 func() struct{}) struct{} {
    var t237 int64 = scope__13.id
    _goml_runtime_std_task_scope_spawn(t237, body__14)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__17 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__18 func() struct{}) struct{} {
    var t243 int64 = scope__17.id
    _goml_runtime_std_task_scope_run(t243, body__18)
    var t244 int64 = scope__17.id
    _goml_runtime_std_task_scope_finish(t244)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__21 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t254 int64 = scope__21.id
    var t255 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t254,
    }
    return t255
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline576 bool = false
    var inline577 *ref_bool_x = ref__Ref_4bool(inline576)
    completed__0 = inline577
    var t308 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t309 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t308, p0)
    }
    var total__7 int
    var inline569 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline570 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline571 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline570,
        body_1: t309,
        handle_2: inline569,
    }
    var inline572 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline571)
    }
    _goml_m_std_p_internal_p_task_p_run(inline569, inline572)
    var inline574 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline570)
    total__7 = inline574
    var inline566 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline566)
    var t310 bool
    var inline564 bool = ref_get__Ref_4bool(completed__0)
    t310 = inline564
    var inline561 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t310)
    _goml_runtime_core_string_println(inline561)
    var t311 closure_env_main_6 = closure_env_main_6{}
    var t312 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t311, p0)
    }
    var nested__12 int
    var inline554 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline555 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline556 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline555,
        body_1: t312,
        handle_2: inline554,
    }
    var inline557 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline556)
    }
    _goml_m_std_p_internal_p_task_p_run(inline554, inline557)
    var inline559 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline555)
    nested__12 = inline559
    var inline551 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline551)
    var scope__13 int = 3
    var t313 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t313, p0)
    }
    var t314 int = spawn__15(scope__13)
    var inline548 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t314)
    _goml_runtime_core_string_println(inline548)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__277 int) chan struct{} {
    var t318 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__277)
    return t318
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__53 _goml_m_std_p_task_p_Task____int) int {
    var jp352 int
    Loop_loop_expr353:
    for {
        var t354 chan struct{} = self__53.ready
        var inline613 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t354)
        var inline615 bool = inline613._1
        if inline615 {} else {}
        var t355 *ref_Option__int_x = self__53.result
        var mtmp26 Option__int
        var inline611 Option__int = ref_get__Ref_11Option__int(t355)
        mtmp26 = inline611
        switch mtmp26.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x27 int = mtmp26.(Option__int_Some)._0
            jp352 = x27
            break Loop_loop_expr353
        default:
            panic("non-exhaustive match")
        }
    }
    return jp352
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__273 Option__int) *ref_Option__int_x {
    var t374 *ref_Option__int_x = ref__Ref_11Option__int(value__273)
    return t374
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp380 int
    Loop_loop_expr381:
    for {
        var mtmp0 Option__int
        var inline635 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline635
        switch mtmp0.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x1 int = mtmp0.(Option__int_Some)._0
            jp380 = x1
            break Loop_loop_expr381
        default:
            panic("non-exhaustive match")
        }
    }
    return jp380
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__273 Option__unit) *ref_Option__unit_x {
    var t387 *ref_Option__unit_x = ref__Ref_12Option__unit(value__273)
    return t387
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t400 string = _goml_runtime_core_int_to_string(self__67)
    return t400
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t403 string = _goml_runtime_core_bool_to_string(self__64)
    return t403
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env191 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env192 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env193 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env193.completed_0
    var inline643 bool = true
    ref_set__Ref_4bool(completed__0, inline643)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env194 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env194.completed_0
    var t459 closure_env_main_0 = closure_env_main_0{}
    var t460 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t459, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____int
    var inline668 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline669 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline670 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline671 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline670)
    var inline672 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline671,
    }
    var inline673 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline674 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline668,
        body_1: t460,
        token_2: inline672,
        ready_3: inline669,
    }
    var inline675 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline674)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline673, inline675)
    var inline677 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline668,
        ready: inline669,
    }
    left__3 = inline677
    var t461 closure_env_main_1 = closure_env_main_1{}
    var t462 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t461, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____int
    var inline657 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline658 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline659 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline660 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline659)
    var inline661 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline660,
    }
    var inline662 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline663 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline657,
        body_1: t462,
        token_2: inline661,
        ready_3: inline658,
    }
    var inline664 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline663)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline662, inline664)
    var inline666 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline657,
        ready: inline658,
    }
    right__5 = inline666
    var t463 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t464 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t463, p0)
    }
    var inline646 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit_None{})
    var inline647 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline648 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline649 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline648)
    var inline650 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline649,
    }
    var inline651 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline652 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline646,
        body_1: t464,
        token_2: inline650,
        ready_3: inline647,
    }
    var inline653 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline652)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline651, inline653)
    var t465 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t466 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t467 int = t465 + t466
    return t467
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env195 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env196 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t472 closure_env_main_4 = closure_env_main_4{}
    var t473 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t472, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____int
    var inline679 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline680 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline681 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline682 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline681)
    var inline683 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline682,
    }
    var inline684 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline685 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline679,
        body_1: t473,
        token_2: inline683,
        ready_3: inline680,
    }
    var inline686 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline685)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline684, inline686)
    var inline688 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline679,
        ready: inline680,
    }
    value__11 = inline688
    var t474 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t474
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env197 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t478 _goml_m_std_p_task_p_CancelToken
    var inline698 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline699 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline698)
    var inline700 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline699,
    }
    t478 = inline700
    var t479 closure_env_main_5 = closure_env_main_5{}
    var t480 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t479, p0)
    }
    var inline690 _goml_m_std_p_internal_p_task_p_CancelToken = t478.value
    var inline691 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline690)
    var inline692 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline693 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: inline692,
        body_1: t480,
        handle_2: inline691,
    }
    var inline694 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(inline693)
    }
    _goml_m_std_p_internal_p_task_p_run(inline691, inline694)
    var inline696 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline692)
    return inline696
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env198 closure_env_spawn_7, value__14 int) int {
    var t484 int = value__14 + 1
    return t484
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env199 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env199.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env199.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env199.handle_2
    var t486 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t487 int = body__9(t486)
    var t488 Option__int = Option__int_Some{
        _0: t487,
    }
    ref_set__Ref_11Option__int(result__11, t488)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env200 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env200.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env200.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env200.token_2
    var ready__28 chan struct{} = env200.ready_3
    var t491 int = body__26(token__29)
    var t492 Option__int = Option__int_Some{
        _0: t491,
    }
    ref_set__Ref_11Option__int(result__27, t492)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env201 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env201.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env201.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env201.token_2
    var ready__28 chan struct{} = env201.ready_3
    var t495 struct{} = body__26(token__29)
    var t496 Option__unit = Option__unit_Some{
        _0: t495,
    }
    ref_set__Ref_12Option__unit(result__27, t496)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(env202 closure_env_std_task_scope_with_T_int_11) struct{} {
    var result__15 *ref_Option__int_x = env202.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env202.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env202.handle_2
    var t499 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t500 int = body__13(t499)
    var t501 Option__int = Option__int_Some{
        _0: t500,
    }
    ref_set__Ref_11Option__int(result__15, t501)
    return struct{}{}
}

func main() {
    main0()
}
