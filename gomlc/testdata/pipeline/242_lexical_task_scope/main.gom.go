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

type Ordering int32

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
    var t450 int64
    var inline725 int64 = 0
    var inline726 int64 = _goml_runtime_std_task_scope_new(inline725)
    t450 = inline726
    var t451 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t450,
    }
    return t451
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__12 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t454 int64 = parent__12.id
    var t455 int64
    var inline728 int64 = _goml_runtime_std_task_scope_new(t454)
    t455 = inline728
    var t456 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t455,
    }
    return t456
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__13 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__14 func() struct{}) struct{} {
    var t458 int64 = scope__13.id
    _goml_runtime_std_task_scope_spawn(t458, body__14)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__17 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__18 func() struct{}) struct{} {
    var t464 int64 = scope__17.id
    _goml_runtime_std_task_scope_run(t464, body__18)
    var t465 int64 = scope__17.id
    _goml_runtime_std_task_scope_finish(t465)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__21 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t475 int64 = scope__21.id
    var t476 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t475,
    }
    return t476
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline797 bool = false
    var inline798 *ref_bool_x = ref__Ref_4bool(inline797)
    completed__0 = inline798
    var t529 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t530 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t529, p0)
    }
    var total__7 int
    var inline790 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline791 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline792 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline791,
        body_1: t530,
        handle_2: inline790,
    }
    var inline793 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline792)
    }
    _goml_m_std_p_internal_p_task_p_run(inline790, inline793)
    var inline795 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline791)
    total__7 = inline795
    var inline787 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline787)
    var t531 bool
    var inline785 bool = ref_get__Ref_4bool(completed__0)
    t531 = inline785
    var inline782 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t531)
    _goml_runtime_core_string_println(inline782)
    var t532 closure_env_main_6 = closure_env_main_6{}
    var t533 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t532, p0)
    }
    var nested__12 int
    var inline775 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline776 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline777 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline776,
        body_1: t533,
        handle_2: inline775,
    }
    var inline778 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline777)
    }
    _goml_m_std_p_internal_p_task_p_run(inline775, inline778)
    var inline780 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline776)
    nested__12 = inline780
    var inline772 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline772)
    var scope__13 int = 3
    var t534 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t534, p0)
    }
    var t535 int = spawn__15(scope__13)
    var inline769 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t535)
    _goml_runtime_core_string_println(inline769)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__435 int) chan struct{} {
    var t539 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__435)
    return t539
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__53 _goml_m_std_p_task_p_Task____int) int {
    var jp573 int
    Loop_loop_expr574:
    for {
        var t575 chan struct{} = self__53.ready
        var inline834 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t575)
        var inline836 bool = inline834._1
        if inline836 {} else {}
        var t576 *ref_Option__int_x = self__53.result
        var mtmp26 Option__int
        var inline832 Option__int = ref_get__Ref_11Option__int(t576)
        mtmp26 = inline832
        switch mtmp26.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x27 int = mtmp26.(Option__int_Some)._0
            jp573 = x27
            break Loop_loop_expr574
        default:
            panic("non-exhaustive match")
        }
    }
    return jp573
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__431 Option__int) *ref_Option__int_x {
    var t595 *ref_Option__int_x = ref__Ref_11Option__int(value__431)
    return t595
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp601 int
    Loop_loop_expr602:
    for {
        var mtmp0 Option__int
        var inline856 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline856
        switch mtmp0.(type) {
        case Option__int_None:
            continue
        case Option__int_Some:
            var x1 int = mtmp0.(Option__int_Some)._0
            jp601 = x1
            break Loop_loop_expr602
        default:
            panic("non-exhaustive match")
        }
    }
    return jp601
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__431 Option__unit) *ref_Option__unit_x {
    var t608 *ref_Option__unit_x = ref__Ref_12Option__unit(value__431)
    return t608
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t621 string = _goml_runtime_core_int_to_string(self__151)
    return t621
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t624 string = _goml_runtime_core_bool_to_string(self__148)
    return t624
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env412 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env413 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env414 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env414.completed_0
    var inline864 bool = true
    ref_set__Ref_4bool(completed__0, inline864)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env415 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env415.completed_0
    var t680 closure_env_main_0 = closure_env_main_0{}
    var t681 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t680, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____int
    var inline889 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline890 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline891 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline892 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline891)
    var inline893 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline892,
    }
    var inline894 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline895 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline889,
        body_1: t681,
        token_2: inline893,
        ready_3: inline890,
    }
    var inline896 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline895)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline894, inline896)
    var inline898 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline889,
        ready: inline890,
    }
    left__3 = inline898
    var t682 closure_env_main_1 = closure_env_main_1{}
    var t683 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t682, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____int
    var inline878 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline879 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline880 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline881 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline880)
    var inline882 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline881,
    }
    var inline883 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline884 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline878,
        body_1: t683,
        token_2: inline882,
        ready_3: inline879,
    }
    var inline885 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline884)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline883, inline885)
    var inline887 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline878,
        ready: inline879,
    }
    right__5 = inline887
    var t684 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t685 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t684, p0)
    }
    var inline867 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit_None{})
    var inline868 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline869 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline870 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline869)
    var inline871 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline870,
    }
    var inline872 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline873 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline867,
        body_1: t685,
        token_2: inline871,
        ready_3: inline868,
    }
    var inline874 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline873)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline872, inline874)
    var t686 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t687 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t688 int = t686 + t687
    return t688
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env416 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env417 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t693 closure_env_main_4 = closure_env_main_4{}
    var t694 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t693, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____int
    var inline900 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline901 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline902 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline903 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline902)
    var inline904 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline903,
    }
    var inline905 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline906 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline900,
        body_1: t694,
        token_2: inline904,
        ready_3: inline901,
    }
    var inline907 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline906)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline905, inline907)
    var inline909 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline900,
        ready: inline901,
    }
    value__11 = inline909
    var t695 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t695
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env418 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t699 _goml_m_std_p_task_p_CancelToken
    var inline919 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline920 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline919)
    var inline921 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline920,
    }
    t699 = inline921
    var t700 closure_env_main_5 = closure_env_main_5{}
    var t701 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t700, p0)
    }
    var inline911 _goml_m_std_p_internal_p_task_p_CancelToken = t699.value
    var inline912 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline911)
    var inline913 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int_None{})
    var inline914 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: inline913,
        body_1: t701,
        handle_2: inline912,
    }
    var inline915 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(inline914)
    }
    _goml_m_std_p_internal_p_task_p_run(inline912, inline915)
    var inline917 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline913)
    return inline917
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env419 closure_env_spawn_7, value__14 int) int {
    var t705 int = value__14 + 1
    return t705
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env420 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env420.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env420.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env420.handle_2
    var t707 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t708 int = body__9(t707)
    var t709 Option__int = Option__int_Some{
        _0: t708,
    }
    ref_set__Ref_11Option__int(result__11, t709)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env421 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env421.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env421.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env421.token_2
    var ready__28 chan struct{} = env421.ready_3
    var t712 int = body__26(token__29)
    var t713 Option__int = Option__int_Some{
        _0: t712,
    }
    ref_set__Ref_11Option__int(result__27, t713)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env422 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env422.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env422.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env422.token_2
    var ready__28 chan struct{} = env422.ready_3
    var t716 struct{} = body__26(token__29)
    var t717 Option__unit = Option__unit_Some{
        _0: t716,
    }
    ref_set__Ref_12Option__unit(result__27, t717)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(env423 closure_env_std_task_scope_with_T_int_11) struct{} {
    var result__15 *ref_Option__int_x = env423.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env423.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env423.handle_2
    var t720 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t721 int = body__13(t720)
    var t722 Option__int = Option__int_Some{
        _0: t721,
    }
    ref_set__Ref_11Option__int(result__15, t722)
    return struct{}{}
}

func main() {
    main0()
}
