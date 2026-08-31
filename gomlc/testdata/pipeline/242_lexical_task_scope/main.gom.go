package main

import (
    _goml_context "context"
    _goml_os "os"
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

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
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

type ref_Option__isize_x struct {
    value Option__isize
}

func ref__Ref_13Option__isize(value Option__isize) *ref_Option__isize_x {
    return &ref_Option__isize_x{
        value: value,
    }
}

func ref_get__Ref_13Option__isize(reference *ref_Option__isize_x) Option__isize {
    return reference.value
}

func ref_set__Ref_13Option__isize(reference *ref_Option__isize_x, value Option__isize) struct{} {
    reference.value = value
    return struct{}{}
}

type ref__goml_m_Option_____o__q__x struct {
    value _goml_m_Option_____o__q_
}

func ref___goml_m_Ref__10Option_____o__q_(value _goml_m_Option_____o__q_) *ref__goml_m_Option_____o__q__x {
    return &ref__goml_m_Option_____o__q__x{
        value: value,
    }
}

func ref_set___goml_m_Ref__10Option_____o__q_(reference *ref__goml_m_Option_____o__q__x, value _goml_m_Option_____o__q_) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_4unit_4bool struct {
    _0 struct{}
    _1 bool
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
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

type _goml_m_std_p_task_p_Task____isize struct {
    result *ref_Option__isize_x
    ready chan struct{}
}

type _goml_m_std_p_task_p_Task_____o__q_ struct {
    result *ref__goml_m_Option_____o__q__x
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

type closure_env_std_task_scope_T_isize_8 struct {
    result_0 *ref_Option__isize_x
    body_1 func(_goml_m_std_p_task_p_Scope) int
    handle_2 _goml_m_std_p_internal_p_task_p_ScopeHandle
}

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 struct {
    result_0 *ref_Option__isize_x
    body_1 func(_goml_m_std_p_task_p_CancelToken) int
    token_2 _goml_m_std_p_task_p_CancelToken
    ready_3 chan struct{}
}

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_10 struct {
    result_0 *ref__goml_m_Option_____o__q__x
    body_1 func(_goml_m_std_p_task_p_CancelToken) struct{}
    token_2 _goml_m_std_p_task_p_CancelToken
    ready_3 chan struct{}
}

type closure_env_std_task_scope_with_T_isize_11 struct {
    result_0 *ref_Option__isize_x
    body_1 func(_goml_m_std_p_task_p_Scope) int
    handle_2 _goml_m_std_p_internal_p_task_p_ScopeHandle
}

type Ordering int32

type _goml_m_std_p_task_p_WaitResult_____o__q_ struct {
    _tag int32
    _v0_0 struct{}
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type _goml_m_Option_____o__q_ struct {
    _tag int32
    _v1_0 struct{}
}

func _goml_m_std_p_internal_p_task_p_root__scope() _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t0 int64
    var inline0 int64 = 0
    var inline1 int64 = _goml_runtime_std_task_scope_new(inline0)
    t0 = inline1
    var t1 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t0,
    }
    return t1
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__0 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t0 int64 = parent__0.id
    var t1 int64
    var inline0 int64 = _goml_runtime_std_task_scope_new(t0)
    t1 = inline0
    var t2 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t1,
    }
    return t2
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__0 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__0 func() struct{}) struct{} {
    var t0 int64 = scope__0.id
    _goml_runtime_std_task_scope_spawn(t0, body__0)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__0 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__0 func() struct{}) struct{} {
    var t0 int64 = scope__0.id
    _goml_runtime_std_task_scope_run(t0, body__0)
    var t1 int64 = scope__0.id
    _goml_runtime_std_task_scope_finish(t1)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__0 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t0 int64 = scope__0.id
    var t1 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t0,
    }
    return t1
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline21 bool = false
    var inline22 *ref_bool_x = ref__Ref_4bool(inline21)
    completed__0 = inline22
    var t0 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t1 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t0, p0)
    }
    var total__0 int
    var inline15 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline16 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline17 closure_env_std_task_scope_T_isize_8 = closure_env_std_task_scope_T_isize_8{
        result_0: inline16,
        body_1: t1,
        handle_2: inline15,
    }
    var inline18 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h79e1b97c71c1c66e8a54d8d6a22e1edf_size__8_i_apply(inline17)
    }
    _goml_m_std_p_internal_p_task_p_run(inline15, inline18)
    var inline20 int = _goml_m_std_p_task_p_completed__scope__value____T__isize(inline16)
    total__0 = inline20
    var inline13 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(total__0)
    _goml_runtime_core_string_println(inline13)
    var t2 bool
    var inline12 bool = ref_get__Ref_4bool(completed__0)
    t2 = inline12
    var inline10 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t2)
    _goml_runtime_core_string_println(inline10)
    var t3 closure_env_main_6 = closure_env_main_6{}
    var t4 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t3, p0)
    }
    var nested__0 int
    var inline4 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline5 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline6 closure_env_std_task_scope_T_isize_8 = closure_env_std_task_scope_T_isize_8{
        result_0: inline5,
        body_1: t4,
        handle_2: inline4,
    }
    var inline7 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h79e1b97c71c1c66e8a54d8d6a22e1edf_size__8_i_apply(inline6)
    }
    _goml_m_std_p_internal_p_task_p_run(inline4, inline7)
    var inline9 int = _goml_m_std_p_task_p_completed__scope__value____T__isize(inline5)
    nested__0 = inline9
    var inline2 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(nested__0)
    _goml_runtime_core_string_println(inline2)
    var scope__0 int = 3
    var t5 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__0 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t5, p0)
    }
    var t6 int = spawn__0(scope__0)
    var inline0 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t6)
    _goml_runtime_core_string_println(inline0)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T___o__q_(capacity__0 int) chan struct{} {
    var t0 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__0)
    return t0
}

func _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(self__0 _goml_m_std_p_task_p_Task____isize) int {
    var jp0 int
    Loop_loop_expr0:
    for {
        var t0 chan struct{} = self__0.ready
        var inline1 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t0)
        var inline3 bool = inline1._1
        if inline3 {} else {}
        var t1 *ref_Option__isize_x = self__0.result
        var mtmp1 Option__isize
        var inline0 Option__isize = ref_get__Ref_13Option__isize(t1)
        mtmp1 = inline0
        switch mtmp1._tag {
        case 0:
            continue
        case 1:
            var x0 int = mtmp1._v1_0
            jp0 = x0
            break Loop_loop_expr0
        default:
            panic("non-exhaustive match")
        }
    }
    return jp0
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(value__0 Option__isize) *ref_Option__isize_x {
    var t0 *ref_Option__isize_x = ref__Ref_13Option__isize(value__0)
    return t0
}

func _goml_m_std_p_task_p_completed__scope__value____T__isize(result__0 *ref_Option__isize_x) int {
    var jp0 int
    Loop_loop_expr0:
    for {
        var mtmp0 Option__isize
        var inline0 Option__isize = ref_get__Ref_13Option__isize(result__0)
        mtmp0 = inline0
        switch mtmp0._tag {
        case 1:
            var x0 int = mtmp0._v1_0
            jp0 = x0
            break Loop_loop_expr0
        default:
            continue
        }
    }
    return jp0
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l__o__q__r_(value__0 _goml_m_Option_____o__q_) *ref__goml_m_Option_____o__q__x {
    var t0 *ref__goml_m_Option_____o__q__x = ref___goml_m_Ref__10Option_____o__q_(value__0)
    return t0
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__0 int) string {
    var inline0 int64 = int64(int(self__0))
    var inline1 string = signed_decimal_string(inline0)
    return inline1
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__0 bool) string {
    var t0 string = _goml_runtime_core_bool_to_string(self__0)
    return t0
}

func signed_decimal_string(value__0 int64) string {
    var t0 bool = value__0 < 0
    if t0 {
        var t1 uint64 = uint64(int64(value__0))
        var t2 uint64 = 0 - t1
        var t3 string = decimal_string(t2)
        var t4 string = "-" + t3
        return t4
    } else {
        var t5 uint64 = uint64(int64(value__0))
        var t6 string = decimal_string(t5)
        return t6
    }
}

func decimal_string(value__0 uint64) string {
    var t0 bool = value__0 == 0
    if t0 {
        return "0"
    } else {
        var reversed__0 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__0 uint64 = value__0
        Loop_loop0:
        for {
            var t10 bool = remaining__0 > 0
            if t10 {
                var t11 uint64 = remaining__0 % 10
                var t12 uint8 = uint8(uint64(t11))
                var t13 uint8 = t12 + 48
                vec_push__Vec_5uint8(reversed__0, t13)
                var compound_old1 uint64 = remaining__0
                var compound_value1 uint64 = 10
                var t14 uint64 = compound_old1 / compound_value1
                remaining__0 = t14
                continue
            } else {
                break Loop_loop0
            }
        }
        var t1 int
        var inline3 int = vec_len__Vec_5uint8(reversed__0)
        t1 = inline3
        var bytes__0 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1)
        var offset__0 int = 0
        Loop_loop1:
        for {
            var t2 int
            var inline2 int = vec_len__Vec_5uint8(reversed__0)
            t2 = inline2
            var t3 bool = offset__0 < t2
            if t3 {
                var t4 int
                var inline1 int = vec_len__Vec_5uint8(reversed__0)
                t4 = inline1
                var t5 int = t4 - offset__0
                var t6 int = t5 - 1
                var t7 uint8 = vec_get__Vec_5uint8(reversed__0, t6)
                vec_push__Vec_5uint8(bytes__0, t7)
                var compound_old0 int = offset__0
                var compound_value0 int = 1
                var t8 int = compound_old0 + compound_value0
                offset__0 = t8
                continue
            } else {
                break Loop_loop1
            }
        }
        var mtmp0 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__0)
        var x0 string = mtmp0._1
        return x0
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env0 closure_env_main_0, _cancel__0 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env0 closure_env_main_1, _cancel__0 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env0 closure_env_main_2, _cancel__0 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env0.completed_0
    var inline0 bool = true
    ref_set__Ref_4bool(completed__0, inline0)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env0 closure_env_main_3, scope__0 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env0.completed_0
    var t0 closure_env_main_0 = closure_env_main_0{}
    var t1 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t0, p0)
    }
    var left__0 _goml_m_std_p_task_p_Task____isize
    var inline20 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline21 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T___o__q_(0)
    var inline22 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline23 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline22)
    var inline24 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline23,
    }
    var inline25 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline26 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9{
        result_0: inline20,
        body_1: t1,
        token_2: inline24,
        ready_3: inline21,
    }
    var inline27 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(inline26)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline25, inline27)
    var inline29 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline20,
        ready: inline21,
    }
    left__0 = inline29
    var t2 closure_env_main_1 = closure_env_main_1{}
    var t3 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t2, p0)
    }
    var right__0 _goml_m_std_p_task_p_Task____isize
    var inline10 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline11 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T___o__q_(0)
    var inline12 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline13 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline12)
    var inline14 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline13,
    }
    var inline15 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline16 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9{
        result_0: inline10,
        body_1: t3,
        token_2: inline14,
        ready_3: inline11,
    }
    var inline17 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(inline16)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline15, inline17)
    var inline19 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline10,
        ready: inline11,
    }
    right__0 = inline19
    var t4 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t5 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t4, p0)
    }
    var inline0 *ref__goml_m_Option_____o__q__x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l__o__q__r_(_goml_m_Option_____o__q_{
        _tag: 0,
    })
    var inline1 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T___o__q_(0)
    var inline2 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline3 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2)
    var inline4 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline3,
    }
    var inline5 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline6 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_10{
        result_0: inline0,
        body_1: t5,
        token_2: inline4,
        ready_3: inline1,
    }
    var inline7 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_he1d9c8f9e38e0bccbae1cb701d05cd60___T__10_i_apply(inline6)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline5, inline7)
    var t6 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(left__0)
    var t7 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(right__0)
    var t8 int = t6 + t7
    return t8
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env0 closure_env_main_4, _cancel__0 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env0 closure_env_main_5, scope__0 _goml_m_std_p_task_p_Scope) int {
    var t0 closure_env_main_4 = closure_env_main_4{}
    var t1 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t0, p0)
    }
    var value__0 _goml_m_std_p_task_p_Task____isize
    var inline0 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T___o__q_(0)
    var inline2 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline3 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2)
    var inline4 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline3,
    }
    var inline5 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline6 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9{
        result_0: inline0,
        body_1: t1,
        token_2: inline4,
        ready_3: inline1,
    }
    var inline7 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(inline6)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline5, inline7)
    var inline9 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline0,
        ready: inline1,
    }
    value__0 = inline9
    var t2 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(value__0)
    return t2
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env0 closure_env_main_6, scope__0 _goml_m_std_p_task_p_Scope) int {
    var t0 _goml_m_std_p_task_p_CancelToken
    var inline7 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__0.handle
    var inline8 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline7)
    var inline9 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline8,
    }
    t0 = inline9
    var t1 closure_env_main_5 = closure_env_main_5{}
    var t2 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t1, p0)
    }
    var inline0 _goml_m_std_p_internal_p_task_p_CancelToken = t0.value
    var inline1 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline0)
    var inline2 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline3 closure_env_std_task_scope_with_T_isize_11 = closure_env_std_task_scope_with_T_isize_11{
        result_0: inline2,
        body_1: t2,
        handle_2: inline1,
    }
    var inline4 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h51a4106953b36d9d5cb4938997ce2588_ize__11_i_apply(inline3)
    }
    _goml_m_std_p_internal_p_task_p_run(inline1, inline4)
    var inline6 int = _goml_m_std_p_task_p_completed__scope__value____T__isize(inline2)
    return inline6
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env0 closure_env_spawn_7, value__0 int) int {
    var t0 int = value__0 + 1
    return t0
}

func _goml_m_inherent_i_closure__en_h79e1b97c71c1c66e8a54d8d6a22e1edf_size__8_i_apply(env0 closure_env_std_task_scope_T_isize_8) struct{} {
    var result__0 *ref_Option__isize_x = env0.result_0
    var body__0 func(_goml_m_std_p_task_p_Scope) int = env0.body_1
    var handle__0 _goml_m_std_p_internal_p_task_p_ScopeHandle = env0.handle_2
    var t0 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__0,
    }
    var t1 int = body__0(t0)
    var t2 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t1,
    }
    ref_set__Ref_13Option__isize(result__0, t2)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(env0 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9) struct{} {
    var result__0 *ref_Option__isize_x = env0.result_0
    var body__0 func(_goml_m_std_p_task_p_CancelToken) int = env0.body_1
    var token__0 _goml_m_std_p_task_p_CancelToken = env0.token_2
    var ready__0 chan struct{} = env0.ready_3
    var t0 int = body__0(token__0)
    var t1 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t0,
    }
    ref_set__Ref_13Option__isize(result__0, t1)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__0)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_he1d9c8f9e38e0bccbae1cb701d05cd60___T__10_i_apply(env0 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_10) struct{} {
    var result__0 *ref__goml_m_Option_____o__q__x = env0.result_0
    var body__0 func(_goml_m_std_p_task_p_CancelToken) struct{} = env0.body_1
    var token__0 _goml_m_std_p_task_p_CancelToken = env0.token_2
    var ready__0 chan struct{} = env0.ready_3
    var t0 struct{} = body__0(token__0)
    var t1 _goml_m_Option_____o__q_ = _goml_m_Option_____o__q_{
        _tag: 1,
        _v1_0: t0,
    }
    ref_set___goml_m_Ref__10Option_____o__q_(result__0, t1)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__0)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h51a4106953b36d9d5cb4938997ce2588_ize__11_i_apply(env0 closure_env_std_task_scope_with_T_isize_11) struct{} {
    var result__0 *ref_Option__isize_x = env0.result_0
    var body__0 func(_goml_m_std_p_task_p_Scope) int = env0.body_1
    var handle__0 _goml_m_std_p_internal_p_task_p_ScopeHandle = env0.handle_2
    var t0 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__0,
    }
    var t1 int = body__0(t0)
    var t2 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t1,
    }
    ref_set__Ref_13Option__isize(result__0, t2)
    return struct{}{}
}

func main() {
    main0()
}
