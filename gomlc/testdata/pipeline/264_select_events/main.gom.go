package main

import (
    _goml_context "context"
    _goml_os "os"
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

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
}

type _goml_vec_uint32 struct {
    items []uint32
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

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_io_p_ErrorDetails struct {
    kind_value _goml_m_std_p_io_p_ErrorKind
    operation_value string
    context_value Option__string
    raw_os_code_value Option__isize
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

type _goml_m_std_p_task_p_Task____isize struct {
    result *ref_Option__isize_x
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

type closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4 struct {
    result_0 *ref_Option__isize_x
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

type Option__u8 struct {
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

type Option__isize struct {
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
    var t835 int64
    var inline2129 int64 = 0
    var inline2130 int64 = _goml_runtime_std_task_scope_new(inline2129)
    t835 = inline2130
    var t836 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_ScopeHandle{
        id: t835,
    }
    return t836
}

func _goml_m_std_p_internal_p_task_p_spawn(scope__14 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__15 func() struct{}) struct{} {
    var t843 int64 = scope__14.id
    _goml_runtime_std_task_scope_spawn(t843, body__15)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_run(scope__18 _goml_m_std_p_internal_p_task_p_ScopeHandle, body__19 func() struct{}) struct{} {
    var t849 int64 = scope__18.id
    _goml_runtime_std_task_scope_run(t849, body__19)
    var t850 int64 = scope__18.id
    _goml_runtime_std_task_scope_finish(t850)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_cancel(scope__20 _goml_m_std_p_internal_p_task_p_ScopeHandle) struct{} {
    var t853 int64 = scope__20.id
    _goml_runtime_std_task_scope_cancel(t853)
    return struct{}{}
}

func _goml_m_std_p_internal_p_task_p_token(scope__22 _goml_m_std_p_internal_p_task_p_ScopeHandle) _goml_m_std_p_internal_p_task_p_CancelToken {
    var t860 int64 = scope__22.id
    var t861 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_CancelToken{
        id: t860,
    }
    return t861
}

func _goml_m_inherent_i_std_p_inter_h25a0a7c69f0f67d3e38fa37e5f2fc20e_celToken_i_done(self__32 _goml_m_std_p_internal_p_task_p_CancelToken) <-chan struct{} {
    var t878 int64 = self__32.id
    var inline2152 <-chan struct{} = _goml_runtime_std_task_scope_done(t878)
    return inline2152
}

func _goml_m_std_p_internal_p_host_p_timer__new(value__93 int64) Tuple2_5int64_14Receiver_4unit {
    var t1091 Tuple2_5int64_14Receiver_4unit = _goml_runtime_std_time_timer_new(value__93)
    return t1091
}

func _goml_m_std_p_internal_p_host_p_timer__stop(id__94 int64) bool {
    var t1094 bool = _goml_runtime_std_time_timer_stop(id__94)
    return t1094
}

func _goml_m_inherent_i_std_p_time__h80a59d7ad7a13ffcd2fc533934df5aaa_om__nanoseconds(value__11 int64) _goml_m_std_p_time_p_Duration {
    var t1420 bool = value__11 < 0
    var jp1418 int64
    if t1420 {
        jp1418 = 0
    } else {
        jp1418 = value__11
    }
    var t1419 _goml_m_std_p_time_p_Duration = _goml_m_std_p_time_p_Duration{
        nanoseconds: jp1418,
    }
    return t1419
}

func _goml_m_inherent_i_std_p_time__h3ebf4d33c01c8036463b8efe535bf1db_as__nanoseconds(self__26 _goml_m_std_p_time_p_Duration) int64 {
    var t1474 int64 = self__26.nanoseconds
    return t1474
}

func _goml_m_inherent_i_std_p_time_p_Timer_i_std_p_time_p_Timer_i_new(duration__74 _goml_m_std_p_time_p_Duration) _goml_m_std_p_time_p_Timer {
    var t1704 int64
    var inline2587 int64 = duration__74.nanoseconds
    t1704 = inline2587
    var mtmp4 Tuple2_5int64_14Receiver_4unit
    var inline2585 Tuple2_5int64_14Receiver_4unit = _goml_runtime_std_time_timer_new(t1704)
    mtmp4 = inline2585
    var x5 int64 = mtmp4._0
    var x6 <-chan struct{} = mtmp4._1
    var t1705 _goml_m_std_p_time_p_Timer = _goml_m_std_p_time_p_Timer{
        id: x5,
        ready: x6,
    }
    return t1705
}

func main0() struct{} {
    var t1722 _goml_m_std_p_time_p_Duration
    var inline2655 _goml_m_std_p_time_p_Duration = _goml_m_std_p_time_p_Duration{
        nanoseconds: 0,
    }
    t1722 = inline2655
    var timer__0 _goml_m_std_p_time_p_Timer = _goml_m_inherent_i_std_p_time_p_Timer_i_std_p_time_p_Timer_i_new(t1722)
    var t1735 <-chan struct{}
    var inline2653 <-chan struct{} = timer__0.ready
    t1735 = inline2653
    var _goml_m_______1_i_select__open bool
    select {
    case _, _goml_m_______1_i_select__open = <-t1735:
        if _goml_m_______1_i_select__open {}
        var inline2607 int = 1
        var inline2608 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2607)
        _goml_runtime_core_string_println(inline2608)
    }
    var t1724 _goml_m_std_p_time_p_Duration
    var inline2649 int64 = 60
    var inline2650 int64 = inline2649 * 1000000000
    var inline2651 _goml_m_std_p_time_p_Duration = _goml_m_inherent_i_std_p_time__h80a59d7ad7a13ffcd2fc533934df5aaa_om__nanoseconds(inline2650)
    t1724 = inline2651
    var stopped__2 _goml_m_std_p_time_p_Timer
    var inline2641 int64 = _goml_m_inherent_i_std_p_time__h3ebf4d33c01c8036463b8efe535bf1db_as__nanoseconds(t1724)
    var inline2642 Tuple2_5int64_14Receiver_4unit = _goml_m_std_p_internal_p_host_p_timer__new(inline2641)
    var inline2643 int64 = inline2642._0
    var inline2644 <-chan struct{} = inline2642._1
    var inline2647 _goml_m_std_p_time_p_Timer = _goml_m_std_p_time_p_Timer{
        id: inline2643,
        ready: inline2644,
    }
    stopped__2 = inline2647
    var t1725 bool
    var inline2638 int64 = stopped__2.id
    var inline2639 bool = _goml_m_std_p_internal_p_host_p_timer__stop(inline2638)
    t1725 = inline2639
    var inline2635 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1725)
    _goml_runtime_core_string_println(inline2635)
    var t1732 <-chan struct{}
    var inline2633 <-chan struct{} = stopped__2.ready
    t1732 = inline2633
    var _goml_m_______3_i_select__open bool
    select {
    case _, _goml_m_______3_i_select__open = <-t1732:
        if _goml_m_______3_i_select__open {}
        var inline2611 int = 2
        var inline2612 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2611)
        _goml_runtime_core_string_println(inline2612)
    default:
        var inline2615 int = 3
        var inline2616 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2615)
        _goml_runtime_core_string_println(inline2616)
    }
    var t1727 closure_env_main_1 = closure_env_main_1{}
    var t1728 func(_goml_m_std_p_task_p_Scope) struct{} = func(p0 _goml_m_std_p_task_p_Scope) struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t1727, p0)
    }
    var inline2626 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline2627 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline2628 closure_env_std_task_scope_T_unit_3 = closure_env_std_task_scope_T_unit_3{
        result_0: inline2627,
        body_1: t1728,
        handle_2: inline2626,
    }
    var inline2629 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(inline2628)
    }
    _goml_m_std_p_internal_p_task_p_run(inline2626, inline2629)
    _goml_m_std_p_task_p_completed__scope__value____T__unit(inline2627)
    var t1729 closure_env_main_2 = closure_env_main_2{}
    var t1730 func(_goml_m_std_p_task_p_Scope) struct{} = func(p0 _goml_m_std_p_task_p_Scope) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t1729, p0)
    }
    var inline2619 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline2620 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline2621 closure_env_std_task_scope_T_unit_3 = closure_env_std_task_scope_T_unit_3{
        result_0: inline2620,
        body_1: t1730,
        handle_2: inline2619,
    }
    var inline2622 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(inline2621)
    }
    _goml_m_std_p_internal_p_task_p_run(inline2619, inline2622)
    _goml_m_std_p_task_p_completed__scope__value____T__unit(inline2620)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__688 int) chan struct{} {
    var t1739 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__688)
    return t1739
}

func _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(self__55 _goml_m_std_p_task_p_Task____isize) int {
    var jp1836 int
    Loop_loop_expr1837:
    for {
        var t1838 chan struct{} = self__55.ready
        var inline2699 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t1838)
        var inline2701 bool = inline2699._1
        if inline2701 {} else {}
        var t1839 *ref_Option__isize_x = self__55.result
        var mtmp26 Option__isize
        var inline2697 Option__isize = ref_get__Ref_13Option__isize(t1839)
        mtmp26 = inline2697
        switch mtmp26._tag {
        case 0:
            continue
        case 1:
            var x27 int = mtmp26._v1_0
            jp1836 = x27
            break Loop_loop_expr1837
        default:
            panic("non-exhaustive match")
        }
    }
    return jp1836
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline2747 int64 = int64(int(self__404))
    var inline2748 string = signed_decimal_string(inline2747)
    return inline2748
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1975 string = _goml_runtime_core_bool_to_string(self__401)
    return t1975
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__684 Option__unit) *ref_Option__unit_x {
    var t1978 *ref_Option__unit_x = ref__Ref_12Option__unit(value__684)
    return t1978
}

func _goml_m_std_p_task_p_completed__scope__value____T__unit(result__0 *ref_Option__unit_x) struct{} {
    Loop_loop_expr1983:
    for {
        var mtmp0 Option__unit
        var inline2750 Option__unit = ref_get__Ref_12Option__unit(result__0)
        mtmp0 = inline2750
        switch mtmp0._tag {
        case 0:
            continue
        case 1:
            break Loop_loop_expr1983
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(value__684 Option__isize) *ref_Option__isize_x {
    var t1987 *ref_Option__isize_x = ref__Ref_13Option__isize(value__684)
    return t1987
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__unit(self__696 chan struct{}) <-chan struct{} {
    var t1994 <-chan struct{} = func(p0 chan struct{}) <-chan struct{} {
        return p0
    }(self__696)
    return t1994
}

func signed_decimal_string(value__214 int64) string {
    var t2038 bool = value__214 < 0
    if t2038 {
        var t2039 uint64 = uint64(int64(value__214))
        var t2040 uint64 = 0 - t2039
        var t2041 string = decimal_string(t2040)
        var t2042 string = "-" + t2041
        return t2042
    } else {
        var t2043 uint64 = uint64(int64(value__214))
        var t2044 string = decimal_string(t2043)
        return t2044
    }
}

func decimal_string(value__208 uint64) string {
    var t2077 bool = value__208 == 0
    if t2077 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop2070:
        for {
            var t2071 bool = remaining__210 > 0
            if t2071 {
                var t2072_rhs uint64 = 10
                var t2072 uint64 = remaining__210 % t2072_rhs
                var t2073 uint8 = uint8(uint64(t2072))
                var t2074 uint8 = t2073 + 48
                vec_push__Vec_5uint8(reversed__209, t2074)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t2075 uint64 = compound_old353 / compound_value354
                remaining__210 = t2075
                continue
            } else {
                break Loop_loop2070
            }
        }
        var t2059 int
        var inline2783 int = vec_len__Vec_5uint8(reversed__209)
        t2059 = inline2783
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t2059)
        var offset__212 int = 0
        Loop_loop2061:
        for {
            var t2062 int
            var inline2781 int = vec_len__Vec_5uint8(reversed__209)
            t2062 = inline2781
            var t2063 bool = offset__212 < t2062
            if t2063 {
                var t2064 int
                var inline2779 int = vec_len__Vec_5uint8(reversed__209)
                t2064 = inline2779
                var t2065 int = t2064 - offset__212
                var t2066 int = t2065 - 1
                var t2067 uint8 = vec_get__Vec_5uint8(reversed__209, t2066)
                vec_push__Vec_5uint8(bytes__211, t2067)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t2068 int = compound_old358 + compound_value359
                offset__212 = t2068
                continue
            } else {
                break Loop_loop2061
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env801 closure_env_main_0, ___5 _goml_m_std_p_task_p_CancelToken) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env802 closure_env_main_1, scope__4 _goml_m_std_p_task_p_Scope) struct{} {
    var t2109 closure_env_main_0 = closure_env_main_0{}
    var t2110 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t2109, p0)
    }
    var work__6 _goml_m_std_p_task_p_Task____isize
    var inline2795 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline2796 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline2797 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__4.handle
    var inline2798 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2797)
    var inline2799 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline2798,
    }
    var inline2800 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__4.handle
    var inline2801 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4{
        result_0: inline2795,
        body_1: t2110,
        token_2: inline2799,
        ready_3: inline2796,
    }
    var inline2802 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h06a158548ef8dc096a001bc90e690a25_size__4_i_apply(inline2801)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline2800, inline2802)
    var inline2804 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline2795,
        ready: inline2796,
    }
    work__6 = inline2804
    var t2112 <-chan struct{}
    var inline2792 chan struct{} = work__6.ready
    var inline2793 <-chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__unit(inline2792)
    t2112 = inline2793
    var _goml_m_______7_i_select__open bool
    select {
    case _, _goml_m_______7_i_select__open = <-t2112:
        if _goml_m_______7_i_select__open {}
        var t2113 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(work__6)
        var inline2789 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t2113)
        _goml_runtime_core_string_println(inline2789)
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env803 closure_env_main_2, scope__8 _goml_m_std_p_task_p_Scope) struct{} {
    var cancel__9 _goml_m_std_p_task_p_CancelToken
    var inline2816 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline2817 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2816)
    var inline2818 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline2817,
    }
    cancel__9 = inline2818
    var inline2813 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    _goml_m_std_p_internal_p_task_p_cancel(inline2813)
    var t2117 <-chan struct{}
    var inline2810 _goml_m_std_p_internal_p_task_p_CancelToken = cancel__9.value
    var inline2811 <-chan struct{} = _goml_m_inherent_i_std_p_inter_h25a0a7c69f0f67d3e38fa37e5f2fc20e_celToken_i_done(inline2810)
    t2117 = inline2811
    var _goml_m_______10_i_select__open bool
    select {
    case _, _goml_m_______10_i_select__open = <-t2117:
        if _goml_m_______10_i_select__open {}
        var inline2806 int = 4
        var inline2807 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2806)
        _goml_runtime_core_string_println(inline2807)
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(env804 closure_env_std_task_scope_T_unit_3) struct{} {
    var result__11 *ref_Option__unit_x = env804.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) struct{} = env804.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env804.handle_2
    var t2120 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t2121 struct{} = body__9(t2120)
    var t2122 Option__unit = Option__unit{
        _tag: 1,
        _v1_0: t2121,
    }
    ref_set__Ref_12Option__unit(result__11, t2122)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h06a158548ef8dc096a001bc90e690a25_size__4_i_apply(env805 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4) struct{} {
    var result__27 *ref_Option__isize_x = env805.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env805.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env805.token_2
    var ready__28 chan struct{} = env805.ready_3
    var t2125 int = body__26(token__29)
    var t2126 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t2125,
    }
    ref_set__Ref_13Option__isize(result__27, t2126)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func main() {
    main0()
}
