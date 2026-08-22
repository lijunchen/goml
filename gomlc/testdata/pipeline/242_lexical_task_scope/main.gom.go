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

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 struct {
    result_0 *ref_Option__unit_x
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

type _goml_m_std_p_task_p_WaitResult____unit struct {
    _tag int32
    _v0_0 struct{}
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__unit struct {
    _tag int32
    _v1_0 struct{}
}

func _goml_m_std_p_internal_p_task_p_root__scope() _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t841 int64
    var inline1167 int64 = 0
    var inline1168 int64 = _goml_runtime_std_task_scope_new(inline1167)
    t841 = inline1168
    var t842 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t841,
    }
    return t842
}

func _goml_m_std_p_internal_p_task_p_child__scope(parent__13 _goml_m_std_p_internal_p_task_p_CancelToken) _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t845 int64 = parent__13.id
    var t846 int64
    var inline1170 int64 = _goml_runtime_std_task_scope_new(t845)
    t846 = inline1170
    var t847 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t846,
    }
    return t847
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__14 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__15 func() struct{}) struct{} {
    var t849 int64 = scope__14.id
    _goml_runtime_std_task_scope_spawn(t849, body__15)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__18 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__19 func() struct{}) struct{} {
    var t855 int64 = scope__18.id
    _goml_runtime_std_task_scope_run(t855, body__19)
    var t856 int64 = scope__18.id
    _goml_runtime_std_task_scope_finish(t856)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__22 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t866 int64 = scope__22.id
    var t867 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t866,
    }
    return t867
}

func main0() struct{} {
    var completed__0 *ref_bool_x
    var inline1244 bool = false
    var inline1245 *ref_bool_x = ref__Ref_4bool(inline1244)
    completed__0 = inline1245
    var t928 closure_env_main_3 = closure_env_main_3{
        completed_0: completed__0,
    }
    var t929 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t928, p0)
    }
    var total__7 int
    var inline1237 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline1238 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1239 closure_env_std_task_scope_T_isize_8 = closure_env_std_task_scope_T_isize_8{
        result_0: inline1238,
        body_1: t929,
        handle_2: inline1237,
    }
    var inline1240 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h79e1b97c71c1c66e8a54d8d6a22e1edf_size__8_i_apply(inline1239)
    }
    _goml_m_std_p_internal_p_task_p_run(inline1237, inline1240)
    var inline1242 int = _goml_m_std_p_task_p_completed__scope__value____T__isize(inline1238)
    total__7 = inline1242
    var inline1234 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(total__7)
    _goml_runtime_core_string_println(inline1234)
    var t930 bool
    var inline1232 bool = ref_get__Ref_4bool(completed__0)
    t930 = inline1232
    var inline1229 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t930)
    _goml_runtime_core_string_println(inline1229)
    var t931 closure_env_main_6 = closure_env_main_6{}
    var t932 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t931, p0)
    }
    var nested__12 int
    var inline1222 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline1223 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1224 closure_env_std_task_scope_T_isize_8 = closure_env_std_task_scope_T_isize_8{
        result_0: inline1223,
        body_1: t932,
        handle_2: inline1222,
    }
    var inline1225 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h79e1b97c71c1c66e8a54d8d6a22e1edf_size__8_i_apply(inline1224)
    }
    _goml_m_std_p_internal_p_task_p_run(inline1222, inline1225)
    var inline1227 int = _goml_m_std_p_task_p_completed__scope__value____T__isize(inline1223)
    nested__12 = inline1227
    var inline1219 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(nested__12)
    _goml_runtime_core_string_println(inline1219)
    var scope__13 int = 3
    var t933 closure_env_spawn_7 = closure_env_spawn_7{}
    var spawn__15 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(t933, p0)
    }
    var t934 int = spawn__15(scope__13)
    var inline1216 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t934)
    _goml_runtime_core_string_println(inline1216)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__688 int) chan struct{} {
    var t938 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__688)
    return t938
}

func _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(self__55 _goml_m_std_p_task_p_Task____isize) int {
    var jp972 int
    Loop_loop_expr973:
    for {
        var t974 chan struct{} = self__55.ready
        var inline1281 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t974)
        var inline1283 bool = inline1281._1
        if inline1283 {} else {}
        var t975 *ref_Option__isize_x = self__55.result
        var mtmp26 Option__isize
        var inline1279 Option__isize = ref_get__Ref_13Option__isize(t975)
        mtmp26 = inline1279
        switch mtmp26._tag {
        case 0:
            continue
        case 1:
            var x27 int = mtmp26._v1_0
            jp972 = x27
            break Loop_loop_expr973
        default:
            panic("non-exhaustive match")
        }
    }
    return jp972
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(value__684 Option__isize) *ref_Option__isize_x {
    var t994 *ref_Option__isize_x = ref__Ref_13Option__isize(value__684)
    return t994
}

func _goml_m_std_p_task_p_completed__scope__value____T__isize(result__0 *ref_Option__isize_x) int {
    var jp1000 int
    Loop_loop_expr1001:
    for {
        var mtmp0 Option__isize
        var inline1303 Option__isize = ref_get__Ref_13Option__isize(result__0)
        mtmp0 = inline1303
        switch mtmp0._tag {
        case 0:
            continue
        case 1:
            var x1 int = mtmp0._v1_0
            jp1000 = x1
            break Loop_loop_expr1001
        default:
            panic("non-exhaustive match")
        }
    }
    return jp1000
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__684 Option__unit) *ref_Option__unit_x {
    var t1007 *ref_Option__unit_x = ref__Ref_12Option__unit(value__684)
    return t1007
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1305 int64 = int64(int(self__404))
    var inline1306 string = signed_decimal_string(inline1305)
    return inline1306
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1023 string = _goml_runtime_core_bool_to_string(self__401)
    return t1023
}

func signed_decimal_string(value__214 int64) string {
    var t1032 bool = value__214 < 0
    if t1032 {
        var t1033 uint64 = uint64(int64(value__214))
        var t1034 uint64 = 0 - t1033
        var t1035 string = decimal_string(t1034)
        var t1036 string = "-" + t1035
        return t1036
    } else {
        var t1037 uint64 = uint64(int64(value__214))
        var t1038 string = decimal_string(t1037)
        return t1038
    }
}

func decimal_string(value__208 uint64) string {
    var t1061 bool = value__208 == 0
    if t1061 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1054:
        for {
            var t1055 bool = remaining__210 > 0
            if t1055 {
                var t1056_rhs uint64 = 10
                var t1056 uint64 = remaining__210 % t1056_rhs
                var t1057 uint8 = uint8(uint64(t1056))
                var t1058 uint8 = t1057 + 48
                vec_push__Vec_5uint8(reversed__209, t1058)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1059 uint64 = compound_old353 / compound_value354
                remaining__210 = t1059
                continue
            } else {
                break Loop_loop1054
            }
        }
        var t1043 int
        var inline1324 int = vec_len__Vec_5uint8(reversed__209)
        t1043 = inline1324
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1043)
        var offset__212 int = 0
        Loop_loop1045:
        for {
            var t1046 int
            var inline1322 int = vec_len__Vec_5uint8(reversed__209)
            t1046 = inline1322
            var t1047 bool = offset__212 < t1046
            if t1047 {
                var t1048 int
                var inline1320 int = vec_len__Vec_5uint8(reversed__209)
                t1048 = inline1320
                var t1049 int = t1048 - offset__212
                var t1050 int = t1049 - 1
                var t1051 uint8 = vec_get__Vec_5uint8(reversed__209, t1050)
                vec_push__Vec_5uint8(bytes__211, t1051)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1052 int = compound_old358 + compound_value359
                offset__212 = t1052
                continue
            } else {
                break Loop_loop1045
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env800 closure_env_main_0, _cancel__2 _goml_m_std_p_task_p_CancelToken) int {
    return 20
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env801 closure_env_main_1, _cancel__4 _goml_m_std_p_task_p_CancelToken) int {
    return 22
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env802 closure_env_main_2, _cancel__6 _goml_m_std_p_task_p_CancelToken) struct{} {
    var completed__0 *ref_bool_x = env802.completed_0
    var inline1332 bool = true
    ref_set__Ref_4bool(completed__0, inline1332)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env803 closure_env_main_3, scope__1 _goml_m_std_p_task_p_Scope) int {
    var completed__0 *ref_bool_x = env803.completed_0
    var t1122 closure_env_main_0 = closure_env_main_0{}
    var t1123 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t1122, p0)
    }
    var left__3 _goml_m_std_p_task_p_Task____isize
    var inline1357 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1358 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline1359 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline1360 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline1359)
    var inline1361 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline1360,
    }
    var inline1362 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline1363 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9{
        result_0: inline1357,
        body_1: t1123,
        token_2: inline1361,
        ready_3: inline1358,
    }
    var inline1364 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(inline1363)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline1362, inline1364)
    var inline1366 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline1357,
        ready: inline1358,
    }
    left__3 = inline1366
    var t1124 closure_env_main_1 = closure_env_main_1{}
    var t1125 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t1124, p0)
    }
    var right__5 _goml_m_std_p_task_p_Task____isize
    var inline1346 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1347 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline1348 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline1349 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline1348)
    var inline1350 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline1349,
    }
    var inline1351 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline1352 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9{
        result_0: inline1346,
        body_1: t1125,
        token_2: inline1350,
        ready_3: inline1347,
    }
    var inline1353 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(inline1352)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline1351, inline1353)
    var inline1355 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline1346,
        ready: inline1347,
    }
    right__5 = inline1355
    var t1126 closure_env_main_2 = closure_env_main_2{
        completed_0: completed__0,
    }
    var t1127 func(_goml_m_std_p_task_p_CancelToken) struct{} = func(p0 _goml_m_std_p_task_p_CancelToken) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t1126, p0)
    }
    var inline1335 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline1336 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline1337 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline1338 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline1337)
    var inline1339 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline1338,
    }
    var inline1340 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__1.handle
    var inline1341 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10{
        result_0: inline1335,
        body_1: t1127,
        token_2: inline1339,
        ready_3: inline1336,
    }
    var inline1342 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(inline1341)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline1340, inline1342)
    var t1128 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(left__3)
    var t1129 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(right__5)
    var t1130 int = t1128 + t1129
    return t1130
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env804 closure_env_main_4, _cancel__10 _goml_m_std_p_task_p_CancelToken) int {
    return 7
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env805 closure_env_main_5, scope__9 _goml_m_std_p_task_p_Scope) int {
    var t1135 closure_env_main_4 = closure_env_main_4{}
    var t1136 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t1135, p0)
    }
    var value__11 _goml_m_std_p_task_p_Task____isize
    var inline1368 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1369 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline1370 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline1371 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline1370)
    var inline1372 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline1371,
    }
    var inline1373 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__9.handle
    var inline1374 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9{
        result_0: inline1368,
        body_1: t1136,
        token_2: inline1372,
        ready_3: inline1369,
    }
    var inline1375 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(inline1374)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline1373, inline1375)
    var inline1377 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline1368,
        ready: inline1369,
    }
    value__11 = inline1377
    var t1137 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(value__11)
    return t1137
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env806 closure_env_main_6, scope__8 _goml_m_std_p_task_p_Scope) int {
    var t1141 _goml_m_std_p_task_p_CancelToken
    var inline1387 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline1388 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline1387)
    var inline1389 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline1388,
    }
    t1141 = inline1389
    var t1142 closure_env_main_5 = closure_env_main_5{}
    var t1143 func(_goml_m_std_p_task_p_Scope) int = func(p0 _goml_m_std_p_task_p_Scope) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t1142, p0)
    }
    var inline1379 _goml_m_std_p_internal_p_task_p_CancelToken = t1141.value
    var inline1380 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_child__scope(inline1379)
    var inline1381 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline1382 closure_env_std_task_scope_with_T_isize_11 = closure_env_std_task_scope_with_T_isize_11{
        result_0: inline1381,
        body_1: t1143,
        handle_2: inline1380,
    }
    var inline1383 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h51a4106953b36d9d5cb4938997ce2588_ize__11_i_apply(inline1382)
    }
    _goml_m_std_p_internal_p_task_p_run(inline1380, inline1383)
    var inline1385 int = _goml_m_std_p_task_p_completed__scope__value____T__isize(inline1381)
    return inline1385
}

func _goml_m_inherent_i_closure__env__spawn__7_i_closure__env__spawn__7_i_apply(env807 closure_env_spawn_7, value__14 int) int {
    var t1147 int = value__14 + 1
    return t1147
}

func _goml_m_inherent_i_closure__en_h79e1b97c71c1c66e8a54d8d6a22e1edf_size__8_i_apply(env808 closure_env_std_task_scope_T_isize_8) struct{} {
    var result__11 *ref_Option__isize_x = env808.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) int = env808.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env808.handle_2
    var t1149 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t1150 int = body__9(t1149)
    var t1151 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t1150,
    }
    ref_set__Ref_13Option__isize(result__11, t1151)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h2edb97eafd7aca63bdc79db1f12910bf_size__9_i_apply(env809 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_9) struct{} {
    var result__27 *ref_Option__isize_x = env809.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env809.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env809.token_2
    var ready__28 chan struct{} = env809.ready_3
    var t1154 int = body__26(token__29)
    var t1155 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t1154,
    }
    ref_set__Ref_13Option__isize(result__27, t1155)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6c3ecee718369d6dae40e4a419b42584_nit__10_i_apply(env810 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_unit_10) struct{} {
    var result__27 *ref_Option__unit_x = env810.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) struct{} = env810.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env810.token_2
    var ready__28 chan struct{} = env810.ready_3
    var t1158 struct{} = body__26(token__29)
    var t1159 Option__unit = Option__unit{
        _tag: 1,
        _v1_0: t1158,
    }
    ref_set__Ref_12Option__unit(result__27, t1159)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h51a4106953b36d9d5cb4938997ce2588_ize__11_i_apply(env811 closure_env_std_task_scope_with_T_isize_11) struct{} {
    var result__15 *ref_Option__isize_x = env811.result_0
    var body__13 func(_goml_m_std_p_task_p_Scope) int = env811.body_1
    var handle__14 _goml_m_std_p_internal_p_task_p_ScopeHandle = env811.handle_2
    var t1162 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__14,
    }
    var t1163 int = body__13(t1162)
    var t1164 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t1163,
    }
    ref_set__Ref_13Option__isize(result__15, t1164)
    return struct{}{}
}

func main() {
    main0()
}
