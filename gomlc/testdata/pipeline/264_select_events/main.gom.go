package main

import (
    _goml_context "context"
    _goml_fmt "fmt"
    _goml_sync "sync"
    _goml_time "time"
)

var _goml_time_timer_mutex _goml_sync.Mutex = _goml_sync.Mutex{}

var _goml_time_timer_registry map[int64]*_goml_time.Timer = make(map[int64]*_goml_time.Timer)

var _goml_time_timer_next_id int64 = 1

func _goml_time_timer_fire(id int64, ready chan struct{}) struct{} {
    _goml_time_timer_mutex.Lock()
    var timer *_goml_time.Timer = _goml_time_timer_registry[id]
    if timer != nil {
        delete(_goml_time_timer_registry, id)
        close(ready)
    }
    _goml_time_timer_mutex.Unlock()
    return struct{}{}
}

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

func _goml_task_context(id int64) _goml_context.Context {
    var scope *_goml_task_scope_state = _goml_task_lookup(id)
    if scope != nil {
        return scope.ctx
    }
    var ctx _goml_context.Context
    var cancel _goml_context.CancelFunc
    ctx, cancel = _goml_context.WithCancel(_goml_context.Background())
    cancel()
    return ctx
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

func _goml_runtime_std_time_timer_new(nanoseconds int64) Tuple2_5int64_14Receiver_4unit {
    var ready chan struct{} = make(chan struct{})
    _goml_time_timer_mutex.Lock()
    var id int64 = _goml_time_timer_next_id
    _goml_time_timer_next_id = _goml_time_timer_next_id + 1
    var timer *_goml_time.Timer = _goml_time.AfterFunc(_goml_time.Duration(nanoseconds), func() {
        _goml_time_timer_fire(id, ready)
    })
    _goml_time_timer_registry[id] = timer
    _goml_time_timer_mutex.Unlock()
    return Tuple2_5int64_14Receiver_4unit{
        _0: id,
        _1: ready,
    }
}

func _goml_runtime_std_time_timer_stop(id int64) bool {
    _goml_time_timer_mutex.Lock()
    var timer *_goml_time.Timer = _goml_time_timer_registry[id]
    if timer == nil {
        _goml_time_timer_mutex.Unlock()
        return false
    }
    var stopped bool = timer.Stop()
    if stopped {
        delete(_goml_time_timer_registry, id)
    }
    _goml_time_timer_mutex.Unlock()
    return stopped
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

func _goml_runtime_std_task_scope_cancel(scope_id int64) struct{} {
    var scope *_goml_task_scope_state = _goml_task_lookup(scope_id)
    if scope != nil {
        scope.cancel()
    }
    return struct{}{}
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

func _goml_runtime_std_task_scope_done(scope_id int64) <-chan struct{} {
    return _goml_task_context(scope_id).Done()
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
}

type ref_Option__unit_x struct {
    value Option__unit
}

func ref__Ref_12Option__unit(value Option__unit) *ref_Option__unit_x {
    return &ref_Option__unit_x{
        value: value,
    }
}

func ref_get__Ref_12Option__unit(reference *ref_Option__unit_x) Option__unit {
    return reference.value
}

func ref_set__Ref_12Option__unit(reference *ref_Option__unit_x, value Option__unit) struct{} {
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

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
}

type Tuple6_4bool_10Vec_5uint8_3int_4bool_3int_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 int
    _3 bool
    _4 int
    _5 string
}

type Tuple5_4bool_3int_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 bool
    _3 int
    _4 string
}

type Tuple9_4bool_3int_5int64_6uint32_5int64_3int_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 int64
    _3 uint32
    _4 int64
    _5 int
    _6 bool
    _7 int
    _8 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 string
}

type Tuple6_4bool_11Vec_6string_3int_4bool_3int_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 int
    _3 bool
    _4 int
    _5 string
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple5_4bool_3int_10Vec_5uint8_10Vec_5uint8_6string struct {
    _0 bool
    _1 int
    _2 *_goml_vec_uint8
    _3 *_goml_vec_uint8
    _4 string
}

type Tuple6_4bool_3int_10Vec_5uint8_10Vec_5uint8_6string_4bool struct {
    _0 bool
    _1 int
    _2 *_goml_vec_uint8
    _3 *_goml_vec_uint8
    _4 string
    _5 bool
}

type Tuple3_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 string
}

type Tuple4_4bool_3int_6string_4bool struct {
    _0 bool
    _1 int
    _2 string
    _3 bool
}

type Tuple3_4bool_4uint_6string struct {
    _0 bool
    _1 uint
    _2 string
}

type Tuple3_4bool_7float32_6string struct {
    _0 bool
    _1 float32
    _2 string
}

type Tuple3_4bool_7float64_6string struct {
    _0 bool
    _1 float64
    _2 string
}

type Tuple2_5int64_14Receiver_4unit struct {
    _0 int64
    _1 <-chan struct{}
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4unit_4bool struct {
    _0 struct{}
    _1 bool
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_io_p_ErrorDetails struct {
    kind_value _goml_m_std_p_io_p_ErrorKind
    operation_value string
    context_value Option__string
    raw_os_code_value Option__int
    message_value string
}

type _goml_m_std_p_io_p_Error struct {
    details _goml_m_std_p_io_p_ErrorDetails
}

type _goml_m_std_p_time_p_Error struct {
    details _goml_m_std_p_io_p_ErrorDetails
}

type _goml_m_std_p_time_p_Duration struct {
    nanoseconds int64
}

type _goml_m_std_p_time_p_Timer struct {
    id int64
    ready <-chan struct{}
}

type _goml_m_std_p_time_p_Instant struct {
    nanoseconds int64
}

type _goml_m_std_p_time_p_SystemTime struct {
    unix_nanoseconds int64
}

type _goml_m_std_p_task_p_Task____int struct {
    result *ref_Option__int_x
    ready chan struct{}
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_std_task_scope_T_unit_3 struct {
    result_0 *ref_Option__unit_x
    body_1 func(_goml_m_std_p_task_p_Scope) struct{}
    handle_2 _goml_m_std_p_internal_p_task_p_ScopeHandle
}

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_4 struct {
    result_0 *ref_Option__int_x
    body_1 func(_goml_m_std_p_task_p_CancelToken) int
    token_2 _goml_m_std_p_task_p_CancelToken
    ready_3 chan struct{}
}

type Ordering int32

type _goml_m_std_p_io_p_ErrorKind int32

type _goml_m_std_p_task_p_WaitResult____unit struct {
    _tag int32
    _v0_0 struct{}
}

type Option__uint8 struct {
    _tag int32
    _v1_0 uint8
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__int struct {
    _tag int32
    _v1_0 int
}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error()
}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error() {}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Err struct {
    _0 _goml_m_std_p_io_p_Error
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Err) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error() {}

type _goml_m_Result____string____std_p_io_p_Error interface {
    is_goml_m_Result____string____std_p_io_p_Error()
}

type _goml_m_Result____string____std_p_io_p_Error_Ok struct {
    _0 string
}

func (_ _goml_m_Result____string____std_p_io_p_Error_Ok) is_goml_m_Result____string____std_p_io_p_Error() {}

type _goml_m_Result____string____std_p_io_p_Error_Err struct {
    _0 _goml_m_std_p_io_p_Error
}

func (_ _goml_m_Result____string____std_p_io_p_Error_Err) is_goml_m_Result____string____std_p_io_p_Error() {}

type _goml_m_Result____unit____std_p_io_p_Error interface {
    is_goml_m_Result____unit____std_p_io_p_Error()
}

type _goml_m_Result____unit____std_p_io_p_Error_Ok struct {
    _0 struct{}
}

func (_ _goml_m_Result____unit____std_p_io_p_Error_Ok) is_goml_m_Result____unit____std_p_io_p_Error() {}

type _goml_m_Result____unit____std_p_io_p_Error_Err struct {
    _0 _goml_m_std_p_io_p_Error
}

func (_ _goml_m_Result____unit____std_p_io_p_Error_Err) is_goml_m_Result____unit____std_p_io_p_Error() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_bytes_p_Bytes
    _v1_0 string
}

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

type _goml_m_Option____std_p_time_p_Duration struct {
    _tag int32
    _v1_0 _goml_m_std_p_time_p_Duration
}

type _goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error interface {
    is_goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error()
}

type _goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error_Ok struct {
    _0 _goml_m_std_p_time_p_SystemTime
}

func (_ _goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error_Ok) is_goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error() {}

type _goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error_Err struct {
    _0 _goml_m_std_p_time_p_Error
}

func (_ _goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error_Err) is_goml_m_Result____std_p_time_p_SystemTime____std_p_time_p_Error() {}

type _goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error interface {
    is_goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error()
}

type _goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error_Ok struct {
    _0 _goml_m_std_p_time_p_Duration
}

func (_ _goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error_Ok) is_goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error() {}

type _goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error_Err struct {
    _0 _goml_m_std_p_time_p_Error
}

func (_ _goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error_Err) is_goml_m_Result____std_p_time_p_Duration____std_p_time_p_Error() {}

type Option__unit struct {
    _tag int32
    _v1_0 struct{}
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func _goml_m_std_p_internal_p_task_p_root__scope() _goml_m_std_p_internal_p_task_p_ScopeHandle {
    var t450 int64
    var inline1718 int64 = 0
    var inline1719 int64 = _goml_runtime_std_task_scope_new(inline1718)
    t450 = inline1719
    var t451 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t450,
    }
    return t451
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__14 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__15 func() struct{}) struct{} {
    var t458 int64 = scope__14.id
    _goml_runtime_std_task_scope_spawn(t458, body__15)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__18 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__19 func() struct{}) struct{} {
    var t464 int64 = scope__18.id
    _goml_runtime_std_task_scope_run(t464, body__19)
    var t465 int64 = scope__18.id
    _goml_runtime_std_task_scope_finish(t465)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_cancel(scope__20 _goml_m_std_p_internal_p_task_p_ScopeHandle) struct{} {
    var t468 int64 = scope__20.id
    _goml_runtime_std_task_scope_cancel(t468)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__22 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t475 int64 = scope__22.id
    var t476 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t475,
    }
    return t476
}

func _goml_m_inherent_i_std_p_inter_h25a0a7c69f0f67d3e38fa37e5f2fc20e_celToken_i_done(self__32 _goml_m_std_p_internal_p_task_p_CancelToken) <-chan struct{} {
    var t493 int64 = self__32.id
    var inline1741 <-chan struct{} = _goml_runtime_std_task_scope_done(t493)
    return inline1741
}

func _goml_m_std_p_internal_p_host_p_timer__new(value__99 int64) Tuple2_5int64_14Receiver_4unit {
    var t718 Tuple2_5int64_14Receiver_4unit = _goml_runtime_std_time_timer_new(value__99)
    return t718
}

func _goml_m_std_p_internal_p_host_p_timer__stop(id__100 int64) bool {
    var t721 bool = _goml_runtime_std_time_timer_stop(id__100)
    return t721
}

func _goml_m_inherent_i_std_p_time__h80a59d7ad7a13ffcd2fc533934df5aaa_om__nanoseconds(value__11 int64) _goml_m_std_p_time_p_Duration {
    var t1047 bool = value__11 < 0
    var jp1045 int64
    if t1047 {
        jp1045 = 0
    } else {
        jp1045 = value__11
    }
    var t1046 _goml_m_std_p_time_p_Duration = _goml_m_std_p_time_p_Duration{
        nanoseconds: jp1045,
    }
    return t1046
}

func _goml_m_inherent_i_std_p_time__h3ebf4d33c01c8036463b8efe535bf1db_as__nanoseconds(self__26 _goml_m_std_p_time_p_Duration) int64 {
    var t1101 int64 = self__26.nanoseconds
    return t1101
}

func _goml_m_inherent_i_std_p_time_p_Timer_i_std_p_time_p_Timer_i_new(duration__74 _goml_m_std_p_time_p_Duration) _goml_m_std_p_time_p_Timer {
    var t1331 int64
    var inline2176 int64 = duration__74.nanoseconds
    t1331 = inline2176
    var mtmp4 Tuple2_5int64_14Receiver_4unit
    var inline2174 Tuple2_5int64_14Receiver_4unit = _goml_runtime_std_time_timer_new(t1331)
    mtmp4 = inline2174
    var x5 int64 = mtmp4._0
    var x6 <-chan struct{} = mtmp4._1
    var t1332 _goml_m_std_p_time_p_Timer = _goml_m_std_p_time_p_Timer{
        id: x5,
        ready: x6,
    }
    return t1332
}

func main0() struct{} {
    var t1349 _goml_m_std_p_time_p_Duration
    var inline2244 _goml_m_std_p_time_p_Duration = _goml_m_std_p_time_p_Duration{
        nanoseconds: 0,
    }
    t1349 = inline2244
    var timer__0 _goml_m_std_p_time_p_Timer = _goml_m_inherent_i_std_p_time_p_Timer_i_std_p_time_p_Timer_i_new(t1349)
    var t1362 <-chan struct{}
    var inline2242 <-chan struct{} = timer__0.ready
    t1362 = inline2242
    var _goml_m_______1_i_select__open bool
    select {
    case _, _goml_m_______1_i_select__open = <-t1362:
        if _goml_m_______1_i_select__open {}
        var inline2196 int = 1
        var inline2197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline2196)
        _goml_runtime_core_string_println(inline2197)
    }
    var t1351 _goml_m_std_p_time_p_Duration
    var inline2238 int64 = 60
    var inline2239 int64 = inline2238 * 1000000000
    var inline2240 _goml_m_std_p_time_p_Duration = _goml_m_inherent_i_std_p_time__h80a59d7ad7a13ffcd2fc533934df5aaa_om__nanoseconds(inline2239)
    t1351 = inline2240
    var stopped__2 _goml_m_std_p_time_p_Timer
    var inline2230 int64 = _goml_m_inherent_i_std_p_time__h3ebf4d33c01c8036463b8efe535bf1db_as__nanoseconds(t1351)
    var inline2231 Tuple2_5int64_14Receiver_4unit = _goml_m_std_p_internal_p_host_p_timer__new(inline2230)
    var inline2232 int64 = inline2231._0
    var inline2233 <-chan struct{} = inline2231._1
    var inline2236 _goml_m_std_p_time_p_Timer = _goml_m_std_p_time_p_Timer{
        id: inline2232,
        ready: inline2233,
    }
    stopped__2 = inline2236
    var t1352 bool
    var inline2227 int64 = stopped__2.id
    var inline2228 bool = _goml_m_std_p_internal_p_host_p_timer__stop(inline2227)
    t1352 = inline2228
    var inline2224 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1352)
    _goml_runtime_core_string_println(inline2224)
    var t1359 <-chan struct{}
    var inline2222 <-chan struct{} = stopped__2.ready
    t1359 = inline2222
    var _goml_m_______3_i_select__open bool
    select {
    case _, _goml_m_______3_i_select__open = <-t1359:
        if _goml_m_______3_i_select__open {}
        var inline2200 int = 2
        var inline2201 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline2200)
        _goml_runtime_core_string_println(inline2201)
    default:
        var inline2204 int = 3
        var inline2205 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline2204)
        _goml_runtime_core_string_println(inline2205)
    }
    var t1354 closure_env_main_1 = closure_env_main_1{}
    var t1355 func(_goml_m_std_p_task_p_Scope) struct{} = func(p0 _goml_m_std_p_task_p_Scope) struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t1354, p0)
    }
    var inline2215 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline2216 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline2217 closure_env_std_task_scope_T_unit_3 = closure_env_std_task_scope_T_unit_3{
        result_0: inline2216,
        body_1: t1355,
        handle_2: inline2215,
    }
    var inline2218 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(inline2217)
    }
    _goml_m_std_p_internal_p_task_p_run(inline2215, inline2218)
    _goml_m_std_p_task_p_completed__scope__value____T__unit(inline2216)
    var t1356 closure_env_main_2 = closure_env_main_2{}
    var t1357 func(_goml_m_std_p_task_p_Scope) struct{} = func(p0 _goml_m_std_p_task_p_Scope) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t1356, p0)
    }
    var inline2208 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline2209 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline2210 closure_env_std_task_scope_T_unit_3 = closure_env_std_task_scope_T_unit_3{
        result_0: inline2209,
        body_1: t1357,
        handle_2: inline2208,
    }
    var inline2211 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(inline2210)
    }
    _goml_m_std_p_internal_p_task_p_run(inline2208, inline2211)
    _goml_m_std_p_task_p_completed__scope__value____T__unit(inline2209)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__435 int) chan struct{} {
    var t1366 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__435)
    return t1366
}

func _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(self__55 _goml_m_std_p_task_p_Task____int) int {
    var jp1463 int
    Loop_loop_expr1464:
    for {
        var t1465 chan struct{} = self__55.ready
        var inline2288 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t1465)
        var inline2290 bool = inline2288._1
        if inline2290 {} else {}
        var t1466 *ref_Option__int_x = self__55.result
        var mtmp26 Option__int
        var inline2286 Option__int = ref_get__Ref_11Option__int(t1466)
        mtmp26 = inline2286
        switch mtmp26._tag {
        case 0:
            continue
        case 1:
            var x27 int = mtmp26._v1_0
            jp1463 = x27
            break Loop_loop_expr1464
        default:
            panic("non-exhaustive match")
        }
    }
    return jp1463
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t1599 string = _goml_runtime_core_int_to_string(self__151)
    return t1599
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1602 string = _goml_runtime_core_bool_to_string(self__148)
    return t1602
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__431 Option__unit) *ref_Option__unit_x {
    var t1605 *ref_Option__unit_x = ref__Ref_12Option__unit(value__431)
    return t1605
}

func _goml_m_std_p_task_p_completed__scope__value____T__unit(result__0 *ref_Option__unit_x) struct{} {
    Loop_loop_expr1610:
    for {
        var mtmp0 Option__unit
        var inline2336 Option__unit = ref_get__Ref_12Option__unit(result__0)
        mtmp0 = inline2336
        switch mtmp0._tag {
        case 0:
            continue
        case 1:
            break Loop_loop_expr1610
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__431 Option__int) *ref_Option__int_x {
    var t1614 *ref_Option__int_x = ref__Ref_11Option__int(value__431)
    return t1614
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__unit(self__443 chan struct{}) <-chan struct{} {
    var t1621 <-chan struct{} = func(p0 chan struct{}) <-chan struct{} {
        return p0
    }(self__443)
    return t1621
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env416 closure_env_main_0, ___5 _goml_m_std_p_task_p_CancelToken) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env417 closure_env_main_1, scope__4 _goml_m_std_p_task_p_Scope) struct{} {
    var t1698 closure_env_main_0 = closure_env_main_0{}
    var t1699 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t1698, p0)
    }
    var work__6 _goml_m_std_p_task_p_Task____int
    var inline2363 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(Option__int{
        _tag: 0,
    })
    var inline2364 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline2365 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__4.handle
    var inline2366 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2365)
    var inline2367 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline2366,
    }
    var inline2368 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__4.handle
    var inline2369 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_4 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_4{
        result_0: inline2363,
        body_1: t1699,
        token_2: inline2367,
        ready_3: inline2364,
    }
    var inline2370 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6814e44501d9a8feef1758607d671522__int__4_i_apply(inline2369)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline2368, inline2370)
    var inline2372 _goml_m_std_p_task_p_Task____int = _goml_m_std_p_task_p_Task____int{
        result: inline2363,
        ready: inline2364,
    }
    work__6 = inline2372
    var t1701 <-chan struct{}
    var inline2360 chan struct{} = work__6.ready
    var inline2361 <-chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__unit(inline2360)
    t1701 = inline2361
    var _goml_m_______7_i_select__open bool
    select {
    case _, _goml_m_______7_i_select__open = <-t1701:
        if _goml_m_______7_i_select__open {}
        var t1702 int = _goml_m_inherent_i_std_p_task_p_Task_i_std_p_task_p_Task_l_T_r__i_join____T__int(work__6)
        var inline2357 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t1702)
        _goml_runtime_core_string_println(inline2357)
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env418 closure_env_main_2, scope__8 _goml_m_std_p_task_p_Scope) struct{} {
    var cancel__9 _goml_m_std_p_task_p_CancelToken
    var inline2384 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline2385 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2384)
    var inline2386 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline2385,
    }
    cancel__9 = inline2386
    var inline2381 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    _goml_m_std_p_internal_p_task_p_cancel(inline2381)
    var t1706 <-chan struct{}
    var inline2378 _goml_m_std_p_internal_p_task_p_CancelToken = cancel__9.value
    var inline2379 <-chan struct{} = _goml_m_inherent_i_std_p_inter_h25a0a7c69f0f67d3e38fa37e5f2fc20e_celToken_i_done(inline2378)
    t1706 = inline2379
    var _goml_m_______10_i_select__open bool
    select {
    case _, _goml_m_______10_i_select__open = <-t1706:
        if _goml_m_______10_i_select__open {}
        var inline2374 int = 4
        var inline2375 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline2374)
        _goml_runtime_core_string_println(inline2375)
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(env419 closure_env_std_task_scope_T_unit_3) struct{} {
    var result__11 *ref_Option__unit_x = env419.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) struct{} = env419.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env419.handle_2
    var t1709 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t1710 struct{} = body__9(t1709)
    var t1711 Option__unit = Option__unit{
        _tag: 1,
        _v1_0: t1710,
    }
    ref_set__Ref_12Option__unit(result__11, t1711)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h6814e44501d9a8feef1758607d671522__int__4_i_apply(env420 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_int_4) struct{} {
    var result__27 *ref_Option__int_x = env420.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env420.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env420.token_2
    var ready__28 chan struct{} = env420.ready_3
    var t1714 int = body__26(token__29)
    var t1715 Option__int = Option__int{
        _tag: 1,
        _v1_0: t1714,
    }
    ref_set__Ref_11Option__int(result__27, t1715)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func main() {
    main0()
}
