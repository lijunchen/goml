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
    var t214 int64
    var inline489 int64 = 0
    var inline490 int64 = _goml_runtime_std_task_scope_new(inline489)
    t214 = inline490
    var t215 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t214,
    }
    return t215
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__12 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t218 int64 = parent__12.id
    var t219 int64
    var inline492 int64 = _goml_runtime_std_task_scope_new(t218)
    t219 = inline492
    var t220 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t219,
    }
    return t220
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__13 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__14 func() struct{}) struct{} {
    var t222 int64 = scope__13.id
    _goml_runtime_std_task_scope_spawn(t222, body__14)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__17 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__18 func() struct{}) struct{} {
    var t228 int64 = scope__17.id
    _goml_runtime_std_task_scope_run(t228, body__18)
    var t229 int64 = scope__17.id
    _goml_runtime_std_task_scope_finish(t229)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__21 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t239 int64 = scope__21.id
    var t240 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t239,
    }
    return t240
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline561 bool = false
    var inline562 *ref_bool_x = ref__Ref_4bool(inline561)
    completed__0 = inline562
    var t293 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t294 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t293, p0)
    }
    var total__7 int
    var inline554 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline555 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline556 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline555,
        body_1: t294,
        handle_2: inline554,
    }
    var inline557 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline556)
    }
    _goml_m_std_p_internal_p_task_p_run(inline554, inline557)
    var inline559 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline555)
    total__7 = inline559
    var inline551 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline551)
    var t295 bool
    var inline549 bool = ref_get__Ref_4bool(completed__0)
    t295 = inline549
    var inline546 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t295)
    _goml_runtime_core_string_println(inline546)
    var t296 closure_env_main_6 = closure_env_main_6{}
    var t297 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t296, p0)
    }
    var nested__12 int
    var inline539 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline540 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline541 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline540,
        body_1: t297,
        handle_2: inline539,
    }
    var inline542 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline541)
    }
    _goml_m_std_p_internal_p_task_p_run(inline539, inline542)
    var inline544 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline540)
    nested__12 = inline544
    var inline536 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline536)
    var scope__13 int = 3
    var t298 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t298, p0)
    }
    var t299 int = spawn__15(scope__13)
    var inline533 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t299)
    _goml_runtime_core_string_println(inline533)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__259 int) chan struct{} {
    var t303 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__259)
    return t303
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__53 _goml_m_std_p_task_p_Task____int) int {
    var jp337 int
    Loop_loop_expr338:
    for {
        var t339 chan struct{} = self__53.ready
        var inline598 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t339)
        var inline600 bool = inline598._1
        if inline600 {} else {}
        var t340 *ref_Option__int_x = self__53.result
        var mtmp26 Option__int
        var inline596 Option__int = ref_get__Ref_11Option__int(t340)
        mtmp26 = inline596
        switch mtmp26.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x27 int = mtmp26.(Option__int_Some)._0
            jp337 = x27
            break Loop_loop_expr338
        default:
            panic("non-exhaustive match")
        }
    }
    return jp337
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__255 Option__int) *ref_Option__int_x {
    var t359 *ref_Option__int_x = ref__Ref_11Option__int(value__255)
    return t359
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp365 int
    Loop_loop_expr366:
    for {
        var mtmp0 Option__int
        var inline620 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline620
        switch mtmp0.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x1 int = mtmp0.(Option__int_Some)._0
            jp365 = x1
            break Loop_loop_expr366
        default:
            panic("non-exhaustive match")
        }
    }
    return jp365
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__255 Option__unit) *ref_Option__unit_x {
    var t372 *ref_Option__unit_x = ref__Ref_12Option__unit(value__255)
    return t372
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t385 string = _goml_runtime_core_int_to_string(self__67)
    return t385
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t388 string = _goml_runtime_core_bool_to_string(self__64)
    return t388
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env176 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env177 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env178 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env178.completed_0
    var inline628 bool = true
    ref_set__Ref_4bool(completed__0, inline628)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env179 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env179.completed_0
    var t444 closure_env_main_0 = closure_env_main_0{}
    var t445 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t444, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____int
    var inline653 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline654 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline655 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline656 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline655)
    var inline657 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline656,
    }
    var inline658 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline659 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline653,
        body_1: t445,
        token_2: inline657,
        ready_3: inline654,
    }
    var inline660 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline659)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline658, inline660)
    var inline662 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline653,
        ready: inline654,
    }
    left__3 = inline662
    var t446 closure_env_main_1 = closure_env_main_1{}
    var t447 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t446, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____int
    var inline642 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline643 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline644 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline645 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline644)
    var inline646 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline645,
    }
    var inline647 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline648 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline642,
        body_1: t447,
        token_2: inline646,
        ready_3: inline643,
    }
    var inline649 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline648)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline647, inline649)
    var inline651 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline642,
        ready: inline643,
    }
    right__5 = inline651
    var t448 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t449 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t448, p0)
    }
    var inline631 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit_None{})
    var inline632 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline633 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline634 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline633)
    var inline635 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline634,
    }
    var inline636 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline637 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline631,
        body_1: t449,
        token_2: inline635,
        ready_3: inline632,
    }
    var inline638 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline637)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline636, inline638)
    var t450 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t451 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t452 int = t450 + t451
    return t452
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env180 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env181 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t457 closure_env_main_4 = closure_env_main_4{}
    var t458 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t457, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____int
    var inline664 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline665 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline666 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline667 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline666)
    var inline668 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline667,
    }
    var inline669 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline670 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline664,
        body_1: t458,
        token_2: inline668,
        ready_3: inline665,
    }
    var inline671 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline670)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline669, inline671)
    var inline673 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline664,
        ready: inline665,
    }
    value__11 = inline673
    var t459 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t459
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env182 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t463 _goml_m_std_p_task_p_CancelToken
    var inline683 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline684 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline683)
    var inline685 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline684,
    }
    t463 = inline685
    var t464 closure_env_main_5 = closure_env_main_5{}
    var t465 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t464, p0)
    }
    var inline675 _goml_m_std_p_internal_p_task_p_CancelToken = t463.value
    var inline676 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline675)
    var inline677 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline678 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: inline677,
        body_1: t465,
        handle_2: inline676,
    }
    var inline679 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(inline678)
    }
    _goml_m_std_p_internal_p_task_p_run(inline676, inline679)
    var inline681 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline677)
    return inline681
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env183 closure_env_spawn_7, value__14 int) int {
    var t469 int = value__14 + 1
    return t469
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env184 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env184.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env184.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env184.handle_2
    var t471 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t472 int = body__9(t471)
    var t473 Option__int = Option__int_Some{
        _0: t472,
    }
    ref_set__Ref_11Option__int(result__11, t473)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env185 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env185.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env185.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env185.token_2
    var ready__28 chan struct{} = env185.ready_3
    var t476 int = body__26(token__29)
    var t477 Option__int = Option__int_Some{
        _0: t476,
    }
    ref_set__Ref_11Option__int(result__27, t477)
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
    var t480 struct{} = body__26(token__29)
    var t481 Option__unit = Option__unit_Some{
        _0: t480,
    }
    ref_set__Ref_12Option__unit(result__27, t481)
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
    var t484 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t485 int = body__13(t484)
    var t486 Option__int = Option__int_Some{
        _0: t485,
    }
    ref_set__Ref_11Option__int(result__15, t486)
    return struct{}{}
}

func main() {
    main0()
}
