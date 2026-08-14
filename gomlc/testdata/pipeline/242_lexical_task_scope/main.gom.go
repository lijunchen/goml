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
    var t224 int64
    var inline499 int64 = 0
    var inline500 int64 = _goml_runtime_std_task_scope_new(inline499)
    t224 = inline500
    var t225 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t224,
    }
    return t225
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__12 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t228 int64 = parent__12.id
    var t229 int64
    var inline502 int64 = _goml_runtime_std_task_scope_new(t228)
    t229 = inline502
    var t230 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t229,
    }
    return t230
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__13 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__14 func() struct{}) struct{} {
    var t232 int64 = scope__13.id
    _goml_runtime_std_task_scope_spawn(t232, body__14)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__17 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__18 func() struct{}) struct{} {
    var t238 int64 = scope__17.id
    _goml_runtime_std_task_scope_run(t238, body__18)
    var t239 int64 = scope__17.id
    _goml_runtime_std_task_scope_finish(t239)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__21 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t249 int64 = scope__21.id
    var t250 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t249,
    }
    return t250
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline571 bool = false
    var inline572 *ref_bool_x = ref__Ref_4bool(inline571)
    completed__0 = inline572
    var t303 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t304 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t303, p0)
    }
    var total__7 int
    var inline564 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline565 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline566 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline565,
        body_1: t304,
        handle_2: inline564,
    }
    var inline567 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline566)
    }
    _goml_m_std_p_internal_p_task_p_run(inline564, inline567)
    var inline569 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline565)
    total__7 = inline569
    var inline561 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline561)
    var t305 bool
    var inline559 bool = ref_get__Ref_4bool(completed__0)
    t305 = inline559
    var inline556 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t305)
    _goml_runtime_core_string_println(inline556)
    var t306 closure_env_main_6 = closure_env_main_6{}
    var t307 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t306, p0)
    }
    var nested__12 int
    var inline549 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline550 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline551 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline550,
        body_1: t307,
        handle_2: inline549,
    }
    var inline552 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline551)
    }
    _goml_m_std_p_internal_p_task_p_run(inline549, inline552)
    var inline554 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline550)
    nested__12 = inline554
    var inline546 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline546)
    var scope__13 int = 3
    var t308 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t308, p0)
    }
    var t309 int = spawn__15(scope__13)
    var inline543 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t309)
    _goml_runtime_core_string_println(inline543)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__274 int) chan struct{} {
    var t313 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__274)
    return t313
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__53 _goml_m_std_p_task_p_Task____int) int {
    var jp347 int
    Loop_loop_expr348:
    for {
        var t349 chan struct{} = self__53.ready
        var inline608 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t349)
        var inline610 bool = inline608._1
        if inline610 {} else {}
        var t350 *ref_Option__int_x = self__53.result
        var mtmp26 Option__int
        var inline606 Option__int = ref_get__Ref_11Option__int(t350)
        mtmp26 = inline606
        switch mtmp26.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x27 int = mtmp26.(Option__int_Some)._0
            jp347 = x27
            break Loop_loop_expr348
        default:
            panic("non-exhaustive match")
        }
    }
    return jp347
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__270 Option__int) *ref_Option__int_x {
    var t369 *ref_Option__int_x = ref__Ref_11Option__int(value__270)
    return t369
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp375 int
    Loop_loop_expr376:
    for {
        var mtmp0 Option__int
        var inline630 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline630
        switch mtmp0.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x1 int = mtmp0.(Option__int_Some)._0
            jp375 = x1
            break Loop_loop_expr376
        default:
            panic("non-exhaustive match")
        }
    }
    return jp375
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__270 Option__unit) *ref_Option__unit_x {
    var t382 *ref_Option__unit_x = ref__Ref_12Option__unit(value__270)
    return t382
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t395 string = _goml_runtime_core_int_to_string(self__67)
    return t395
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t398 string = _goml_runtime_core_bool_to_string(self__64)
    return t398
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env186 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env187 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env188 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env188.completed_0
    var inline638 bool = true
    ref_set__Ref_4bool(completed__0, inline638)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env189 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env189.completed_0
    var t454 closure_env_main_0 = closure_env_main_0{}
    var t455 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t454, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____int
    var inline663 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline664 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline665 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline666 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline665)
    var inline667 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline666,
    }
    var inline668 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline669 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline663,
        body_1: t455,
        token_2: inline667,
        ready_3: inline664,
    }
    var inline670 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline669)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline668, inline670)
    var inline672 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline663,
        ready: inline664,
    }
    left__3 = inline672
    var t456 closure_env_main_1 = closure_env_main_1{}
    var t457 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t456, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____int
    var inline652 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline653 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline654 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline655 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline654)
    var inline656 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline655,
    }
    var inline657 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline658 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline652,
        body_1: t457,
        token_2: inline656,
        ready_3: inline653,
    }
    var inline659 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline658)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline657, inline659)
    var inline661 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline652,
        ready: inline653,
    }
    right__5 = inline661
    var t458 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t459 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t458, p0)
    }
    var inline641 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit_None{})
    var inline642 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline643 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline644 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline643)
    var inline645 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline644,
    }
    var inline646 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline647 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline641,
        body_1: t459,
        token_2: inline645,
        ready_3: inline642,
    }
    var inline648 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline647)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline646, inline648)
    var t460 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t461 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t462 int = t460 + t461
    return t462
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env190 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env191 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t467 closure_env_main_4 = closure_env_main_4{}
    var t468 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t467, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____int
    var inline674 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline675 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline676 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline677 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline676)
    var inline678 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline677,
    }
    var inline679 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline680 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline674,
        body_1: t468,
        token_2: inline678,
        ready_3: inline675,
    }
    var inline681 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline680)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline679, inline681)
    var inline683 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline674,
        ready: inline675,
    }
    value__11 = inline683
    var t469 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t469
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env192 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t473 _goml_m_std_p_task_p_CancelToken
    var inline693 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline694 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline693)
    var inline695 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline694,
    }
    t473 = inline695
    var t474 closure_env_main_5 = closure_env_main_5{}
    var t475 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t474, p0)
    }
    var inline685 _goml_m_std_p_internal_p_task_p_CancelToken = t473.value
    var inline686 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline685)
    var inline687 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline688 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: inline687,
        body_1: t475,
        handle_2: inline686,
    }
    var inline689 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(inline688)
    }
    _goml_m_std_p_internal_p_task_p_run(inline686, inline689)
    var inline691 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline687)
    return inline691
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env193 closure_env_spawn_7, value__14 int) int {
    var t479 int = value__14 + 1
    return t479
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env194 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env194.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env194.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env194.handle_2
    var t481 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t482 int = body__9(t481)
    var t483 Option__int = Option__int_Some{
        _0: t482,
    }
    ref_set__Ref_11Option__int(result__11, t483)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env195 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env195.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env195.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env195.token_2
    var ready__28 chan struct{} = env195.ready_3
    var t486 int = body__26(token__29)
    var t487 Option__int = Option__int_Some{
        _0: t486,
    }
    ref_set__Ref_11Option__int(result__27, t487)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env196 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env196.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env196.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env196.token_2
    var ready__28 chan struct{} = env196.ready_3
    var t490 struct{} = body__26(token__29)
    var t491 Option__unit = Option__unit_Some{
        _0: t490,
    }
    ref_set__Ref_12Option__unit(result__27, t491)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(env197 closure_env_std_task_scope_with_T_int_11) struct{} {
    var result__15 *ref_Option__int_x = env197.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env197.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env197.handle_2
    var t494 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t495 int = body__13(t494)
    var t496 Option__int = Option__int_Some{
        _0: t495,
    }
    ref_set__Ref_11Option__int(result__15, t496)
    return struct{}{}
}

func main() {
    main0()
}
