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

type _goml_m_std_p_task_p_WaitResult____unit struct {
    _tag int32
    _v0_0 struct{}
}

type Option__int struct {
    _tag int32
    _v1_0 int
}

type Option__unit struct {
    _tag int32
    _v1_0 struct{}
}

func _goml_m_std_p_internal_p_task_p_root__scope() _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t456 int64
    var inline739 int64 = 0
    var inline740 int64 = _goml_runtime_std_task_scope_new(inline739)
    t456 = inline740
    var t457 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t456,
    }
    return t457
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__13 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t460 int64 = parent__13.id
    var t461 int64
    var inline742 int64 = _goml_runtime_std_task_scope_new(t460)
    t461 = inline742
    var t462 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t461,
    }
    return t462
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__14 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__15 func() struct{}) struct{} {
    var t464 int64 = scope__14.id
    _goml_runtime_std_task_scope_spawn(t464, body__15)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__18 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__19 func() struct{}) struct{} {
    var t470 int64 = scope__18.id
    _goml_runtime_std_task_scope_run(t470, body__19)
    var t471 int64 = scope__18.id
    _goml_runtime_std_task_scope_finish(t471)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__22 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t481 int64 = scope__22.id
    var t482 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t481,
    }
    return t482
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline816 bool = false
    var inline817 *ref_bool_x = ref__Ref_4bool(inline816)
    completed__0 = inline817
    var t543 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t544 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t543, p0)
    }
    var total__7 int
    var inline809 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline810 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline811 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline810,
        body_1: t544,
        handle_2: inline809,
    }
    var inline812 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline811)
    }
    _goml_m_std_p_internal_p_task_p_run(inline809, inline812)
    var inline814 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline810)
    total__7 = inline814
    var inline806 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(total__7)
    _goml_runtime_core_string_println(inline806)
    var t545 bool
    var inline804 bool = ref_get__Ref_4bool(completed__0)
    t545 = inline804
    var inline801 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t545)
    _goml_runtime_core_string_println(inline801)
    var t546 closure_env_main_6 = closure_env_main_6{}
    var t547 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t546, p0)
    }
    var nested__12 int
    var inline794 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline795 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline796 closure_env_std_task_scope_T_int_8 = closure_env_std_task_scope_T_int_8{
        result_0: inline795,
        body_1: t547,
        handle_2: inline794,
    }
    var inline797 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(inline796)
    }
    _goml_m_std_p_internal_p_task_p_run(inline794, inline797)
    var inline799 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline795)
    nested__12 = inline799
    var inline791 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline791)
    var scope__13 int = 3
    var t548 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t548, p0)
    }
    var t549 int = spawn__15(scope__13)
    var inline788 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t549)
    _goml_runtime_core_string_println(inline788)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__435 int) chan struct{} {
    var t553 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__435)
    return t553
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__55 _goml_m_std_p_task_p_Task____int) int {
    var jp587 int
    Loop_loop_expr588:
    for {
        var t589 chan struct{} = self__55.ready
        var inline853 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t589)
        var inline855 bool = inline853._1
        if inline855 {} else {}
        var t590 *ref_Option__int_x = self__55.result
        var mtmp26 Option__int
        var inline851 Option__int = ref_get__Ref_11Option__int(t590)
        mtmp26 = inline851
        switch mtmp26._tag {
        case 0:
            continue
        case 1:
            var x27 int = mtmp26._v1_0
            jp587 = x27
            break Loop_loop_expr588
        default:
            panic("non-exhaustive match")
        }
    }
    return jp587
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__431 Option__int) *ref_Option__int_x {
    var t609 *ref_Option__int_x = ref__Ref_11Option__int(value__431)
    return t609
}

func _goml_m_std_p_task_p_completed__scope__value____T__int(result__0 *ref_Option__int_x) int {
    var jp615 int
    Loop_loop_expr616:
    for {
        var mtmp0 Option__int
        var inline875 Option__int = ref_get__Ref_11Option__int(result__0)
        mtmp0 = inline875
        switch mtmp0._tag {
        case 0:
            continue
        case 1:
            var x1 int = mtmp0._v1_0
            jp615 = x1
            break Loop_loop_expr616
        default:
            panic("non-exhaustive match")
        }
    }
    return jp615
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__431 Option__unit) *ref_Option__unit_x {
    var t622 *ref_Option__unit_x = ref__Ref_12Option__unit(value__431)
    return t622
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t635 string = _goml_runtime_core_int_to_string(self__151)
    return t635
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t638 string = _goml_runtime_core_bool_to_string(self__148)
    return t638
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env415 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env416 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env417 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env417.completed_0
    var inline883 bool = true
    ref_set__Ref_4bool(completed__0, inline883)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env418 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env418.completed_0
    var t694 closure_env_main_0 = closure_env_main_0{}
    var t695 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t694, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____int
    var inline908 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline909 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline910 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline911 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline910)
    var inline912 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline911,
    }
    var inline913 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline914 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline908,
        body_1: t695,
        token_2: inline912,
        ready_3: inline909,
    }
    var inline915 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline914)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline913, inline915)
    var inline917 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline908,
        ready: inline909,
    }
    left__3 = inline917
    var t696 closure_env_main_1 = closure_env_main_1{}
    var t697 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t696, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____int
    var inline897 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline898 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline899 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline900 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline899)
    var inline901 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline900,
    }
    var inline902 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline903 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline897,
        body_1: t697,
        token_2: inline901,
        ready_3: inline898,
    }
    var inline904 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline903)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline902, inline904)
    var inline906 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline897,
        ready: inline898,
    }
    right__5 = inline906
    var t698 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t699 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t698, p0)
    }
    var inline886 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline887 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline888 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline889 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline888)
    var inline890 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline889,
    }
    var inline891 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline892 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline886,
        body_1: t699,
        token_2: inline890,
        ready_3: inline887,
    }
    var inline893 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline892)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline891, inline893)
    var t700 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(left__3)
    var t701 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(right__5)
    var t702 int = t700 + t701
    return t702
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env419 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env420 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t707 closure_env_main_4 = closure_env_main_4{}
    var t708 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t707, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____int
    var inline919 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline920 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline921 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline922 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline921)
    var inline923 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline922,
    }
    var inline924 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline925 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9{
        result_0: inline919,
        body_1: t708,
        token_2: inline923,
        ready_3: inline920,
    }
    var inline926 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(inline925)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline924, inline926)
    var inline928 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline919,
        ready: inline920,
    }
    value__11 = inline928
    var t709 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(value__11)
    return t709
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env421 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t713 _goml_m_std_p_task_p_CancelToken
    var inline938 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline939 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline938)
    var inline940 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline939,
    }
    t713 = inline940
    var t714 closure_env_main_5 = closure_env_main_5{}
    var t715 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t714, p0)
    }
    var inline930 _goml_m_std_p_internal_p_task_p_CancelToken = t713.value
    var inline931 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline930)
    var inline932 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline933 closure_env_std_task_scope_with_T_int_11 = closure_env_std_task_scope_with_T_int_11{
        result_0: inline932,
        body_1: t715,
        handle_2: inline931,
    }
    var inline934 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(inline933)
    }
    _goml_m_std_p_internal_p_task_p_run(inline931, inline934)
    var inline936 int = _goml_m_std_p_task_p_completed__scope__value____T__int(inline932)
    return inline936
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env422 closure_env_spawn_7, value__14 int) int {
    var t719 int = value__14 + 1
    return t719
}

func _goml_m_inherent_i_closure__en_hda463fefb4961eb85c12a6840bf1cc91__int__8_i_apply(env423 closure_env_std_task_scope_T_int_8) struct{} {
    var result__11 *ref_Option__int_x = env423.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env423.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env423.handle_2
    var t721 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t722 int = body__9(t721)
    var t723 Option__int = Option__int{
        _tag: 1,
        _v1_0: t722,
    }
    ref_set__Ref_11Option__int(result__11, t723)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h75e335c3b82dc059de4adb2d19eea017__int__9_i_apply(env424 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_9) struct{} {
    var result__27 *ref_Option__int_x = env424.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env424.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env424.token_2
    var ready__28 chan struct{} = env424.ready_3
    var t726 int = body__26(token__29)
    var t727 Option__int = Option__int{
        _tag: 1,
        _v1_0: t726,
    }
    ref_set__Ref_11Option__int(result__27, t727)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env425 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env425.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env425.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env425.token_2
    var ready__28 chan struct{} = env425.ready_3
    var t730 struct{} = body__26(token__29)
    var t731 Option__unit = Option__unit{
        _tag: 1,
        _v1_0: t730,
    }
    ref_set__Ref_12Option__unit(result__27, t731)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_hcea15107708be056347b1cb247dbf928_int__11_i_apply(env426 closure_env_std_task_scope_with_T_int_11) struct{} {
    var result__15 *ref_Option__int_x = env426.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env426.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env426.handle_2
    var t734 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t735 int = body__13(t734)
    var t736 Option__int = Option__int{
        _tag: 1,
        _v1_0: t735,
    }
    ref_set__Ref_11Option__int(result__15, t736)
    return struct{}{}
}

func main() {
    main0()
}
