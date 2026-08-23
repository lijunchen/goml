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
    var inline2131 int64 = 0
    var inline2132 int64 = _goml_runtime_std_task_scope_new(inline2131)
    t835 = inline2132
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
    var inline2154 <-chan struct{} = _goml_runtime_std_task_scope_done(t878)
    return inline2154
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
    var t1419 bool = value__11 < 0
    var jp1417 int64
    if t1419 {
        jp1417 = 0
    } else {
        jp1417 = value__11
    }
    var t1418 _goml_m_std_p_time_p_Duration = _goml_m_std_p_time_p_Duration{
        nanoseconds: jp1417,
    }
    return t1418
}

func _goml_m_inherent_i_std_p_time__h3ebf4d33c01c8036463b8efe535bf1db_as__nanoseconds(self__26 _goml_m_std_p_time_p_Duration) int64 {
    var t1473 int64 = self__26.nanoseconds
    return t1473
}

func _goml_m_inherent_i_std_p_time_p_Timer_i_std_p_time_p_Timer_i_new(duration__74 _goml_m_std_p_time_p_Duration) _goml_m_std_p_time_p_Timer {
    var t1703 int64
    var inline2591 int64 = duration__74.nanoseconds
    t1703 = inline2591
    var mtmp4 Tuple2_5int64_14Receiver_4unit
    var inline2589 Tuple2_5int64_14Receiver_4unit = _goml_runtime_std_time_timer_new(t1703)
    mtmp4 = inline2589
    var x5 int64 = mtmp4._0
    var x6 <-chan struct{} = mtmp4._1
    var t1704 _goml_m_std_p_time_p_Timer = _goml_m_std_p_time_p_Timer{
        id: x5,
        ready: x6,
    }
    return t1704
}

func main0() struct{} {
    var t1721 _goml_m_std_p_time_p_Duration
    var inline2659 _goml_m_std_p_time_p_Duration = _goml_m_std_p_time_p_Duration{
        nanoseconds: 0,
    }
    t1721 = inline2659
    var timer__0 _goml_m_std_p_time_p_Timer = _goml_m_inherent_i_std_p_time_p_Timer_i_std_p_time_p_Timer_i_new(t1721)
    var t1734 <-chan struct{}
    var inline2657 <-chan struct{} = timer__0.ready
    t1734 = inline2657
    var _goml_m_______1_i_select__open bool
    select {
    case _, _goml_m_______1_i_select__open = <-t1734:
        if _goml_m_______1_i_select__open {}
        var inline2611 int = 1
        var inline2612 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2611)
        _goml_runtime_core_string_println(inline2612)
    }
    var t1723 _goml_m_std_p_time_p_Duration
    var inline2653 int64 = 60
    var inline2654 int64 = inline2653 * 1000000000
    var inline2655 _goml_m_std_p_time_p_Duration = _goml_m_inherent_i_std_p_time__h80a59d7ad7a13ffcd2fc533934df5aaa_om__nanoseconds(inline2654)
    t1723 = inline2655
    var stopped__2 _goml_m_std_p_time_p_Timer
    var inline2645 int64 = _goml_m_inherent_i_std_p_time__h3ebf4d33c01c8036463b8efe535bf1db_as__nanoseconds(t1723)
    var inline2646 Tuple2_5int64_14Receiver_4unit = _goml_m_std_p_internal_p_host_p_timer__new(inline2645)
    var inline2647 int64 = inline2646._0
    var inline2648 <-chan struct{} = inline2646._1
    var inline2651 _goml_m_std_p_time_p_Timer = _goml_m_std_p_time_p_Timer{
        id: inline2647,
        ready: inline2648,
    }
    stopped__2 = inline2651
    var t1724 bool
    var inline2642 int64 = stopped__2.id
    var inline2643 bool = _goml_m_std_p_internal_p_host_p_timer__stop(inline2642)
    t1724 = inline2643
    var inline2639 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1724)
    _goml_runtime_core_string_println(inline2639)
    var t1731 <-chan struct{}
    var inline2637 <-chan struct{} = stopped__2.ready
    t1731 = inline2637
    var _goml_m_______3_i_select__open bool
    select {
    case _, _goml_m_______3_i_select__open = <-t1731:
        if _goml_m_______3_i_select__open {}
        var inline2615 int = 2
        var inline2616 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2615)
        _goml_runtime_core_string_println(inline2616)
    default:
        var inline2619 int = 3
        var inline2620 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2619)
        _goml_runtime_core_string_println(inline2620)
    }
    var t1726 closure_env_main_1 = closure_env_main_1{}
    var t1727 func(_goml_m_std_p_task_p_Scope) struct{} = func(p0 _goml_m_std_p_task_p_Scope) struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t1726, p0)
    }
    var inline2630 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline2631 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline2632 closure_env_std_task_scope_T_unit_3 = closure_env_std_task_scope_T_unit_3{
        result_0: inline2631,
        body_1: t1727,
        handle_2: inline2630,
    }
    var inline2633 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(inline2632)
    }
    _goml_m_std_p_internal_p_task_p_run(inline2630, inline2633)
    _goml_m_std_p_task_p_completed__scope__value____T__unit(inline2631)
    var t1728 closure_env_main_2 = closure_env_main_2{}
    var t1729 func(_goml_m_std_p_task_p_Scope) struct{} = func(p0 _goml_m_std_p_task_p_Scope) struct{} {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t1728, p0)
    }
    var inline2623 _goml_m_std_p_internal_p_task_p_ScopeHandle = _goml_m_std_p_internal_p_task_p_root__scope()
    var inline2624 *ref_Option__unit_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(Option__unit{
        _tag: 0,
    })
    var inline2625 closure_env_std_task_scope_T_unit_3 = closure_env_std_task_scope_T_unit_3{
        result_0: inline2624,
        body_1: t1729,
        handle_2: inline2623,
    }
    var inline2626 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(inline2625)
    }
    _goml_m_std_p_internal_p_task_p_run(inline2623, inline2626)
    _goml_m_std_p_task_p_completed__scope__value____T__unit(inline2624)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(capacity__688 int) chan struct{} {
    var t1738 chan struct{} = func(p0 int) chan struct{} {
        return make(chan struct{}, p0)
    }(capacity__688)
    return t1738
}

func _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(self__55 _goml_m_std_p_task_p_Task____isize) int {
    var jp1838 int
    Loop_loop_expr1839:
    for {
        var t1840 chan struct{} = self__55.ready
        var inline2703 Tuple2_4unit_4bool = func(p0 chan struct{}) Tuple2_4unit_4bool {
            var value struct{}
            var ok bool
            value, ok = <-p0
            return Tuple2_4unit_4bool{
                _0: value,
                _1: ok,
            }
        }(t1840)
        var inline2705 bool = inline2703._1
        if inline2705 {} else {}
        var t1841 *ref_Option__isize_x = self__55.result
        var mtmp26 Option__isize
        var inline2701 Option__isize = ref_get__Ref_13Option__isize(t1841)
        mtmp26 = inline2701
        switch mtmp26._tag {
        case 0:
            continue
        case 1:
            var x27 int = mtmp26._v1_0
            jp1838 = x27
            break Loop_loop_expr1839
        default:
            panic("non-exhaustive match")
        }
    }
    return jp1838
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline2751 int64 = int64(int(self__404))
    var inline2752 string = signed_decimal_string(inline2751)
    return inline2752
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1977 string = _goml_runtime_core_bool_to_string(self__401)
    return t1977
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_unit_r_(value__684 Option__unit) *ref_Option__unit_x {
    var t1980 *ref_Option__unit_x = ref__Ref_12Option__unit(value__684)
    return t1980
}

func _goml_m_std_p_task_p_completed__scope__value____T__unit(result__0 *ref_Option__unit_x) struct{} {
    Loop_loop_expr1985:
    for {
        var mtmp0 Option__unit
        var inline2754 Option__unit = ref_get__Ref_12Option__unit(result__0)
        mtmp0 = inline2754
        switch mtmp0._tag {
        case 0:
            continue
        case 1:
            break Loop_loop_expr1985
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(value__684 Option__isize) *ref_Option__isize_x {
    var t1989 *ref_Option__isize_x = ref__Ref_13Option__isize(value__684)
    return t1989
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__unit(self__696 chan struct{}) <-chan struct{} {
    var t1996 <-chan struct{} = func(p0 chan struct{}) <-chan struct{} {
        return p0
    }(self__696)
    return t1996
}

func signed_decimal_string(value__214 int64) string {
    var t2040 bool = value__214 < 0
    if t2040 {
        var t2041 uint64 = uint64(int64(value__214))
        var t2042 uint64 = 0 - t2041
        var t2043 string = decimal_string(t2042)
        var t2044 string = "-" + t2043
        return t2044
    } else {
        var t2045 uint64 = uint64(int64(value__214))
        var t2046 string = decimal_string(t2045)
        return t2046
    }
}

func decimal_string(value__208 uint64) string {
    var t2079 bool = value__208 == 0
    if t2079 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop2072:
        for {
            var t2073 bool = remaining__210 > 0
            if t2073 {
                var t2074_rhs uint64 = 10
                var t2074 uint64 = remaining__210 % t2074_rhs
                var t2075 uint8 = uint8(uint64(t2074))
                var t2076 uint8 = t2075 + 48
                vec_push__Vec_5uint8(reversed__209, t2076)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t2077 uint64 = compound_old353 / compound_value354
                remaining__210 = t2077
                continue
            } else {
                break Loop_loop2072
            }
        }
        var t2061 int
        var inline2787 int = vec_len__Vec_5uint8(reversed__209)
        t2061 = inline2787
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t2061)
        var offset__212 int = 0
        Loop_loop2063:
        for {
            var t2064 int
            var inline2785 int = vec_len__Vec_5uint8(reversed__209)
            t2064 = inline2785
            var t2065 bool = offset__212 < t2064
            if t2065 {
                var t2066 int
                var inline2783 int = vec_len__Vec_5uint8(reversed__209)
                t2066 = inline2783
                var t2067 int = t2066 - offset__212
                var t2068 int = t2067 - 1
                var t2069 uint8 = vec_get__Vec_5uint8(reversed__209, t2068)
                vec_push__Vec_5uint8(bytes__211, t2069)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t2070 int = compound_old358 + compound_value359
                offset__212 = t2070
                continue
            } else {
                break Loop_loop2063
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
    var t2111 closure_env_main_0 = closure_env_main_0{}
    var t2112 func(_goml_m_std_p_task_p_CancelToken) int = func(p0 _goml_m_std_p_task_p_CancelToken) int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t2111, p0)
    }
    var work__6 _goml_m_std_p_task_p_Task____isize
    var inline2799 *ref_Option__isize_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_isize_r_(Option__isize{
        _tag: 0,
    })
    var inline2800 chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__unit(0)
    var inline2801 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__4.handle
    var inline2802 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2801)
    var inline2803 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline2802,
    }
    var inline2804 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__4.handle
    var inline2805 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4 = closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4{
        result_0: inline2799,
        body_1: t2112,
        token_2: inline2803,
        ready_3: inline2800,
    }
    var inline2806 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h06a158548ef8dc096a001bc90e690a25_size__4_i_apply(inline2805)
    }
    _goml_m_std_p_internal_p_task_p_spawn(inline2804, inline2806)
    var inline2808 _goml_m_std_p_task_p_Task____isize = _goml_m_std_p_task_p_Task____isize{
        result: inline2799,
        ready: inline2800,
    }
    work__6 = inline2808
    var t2114 <-chan struct{}
    var inline2796 chan struct{} = work__6.ready
    var inline2797 <-chan struct{} = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__unit(inline2796)
    t2114 = inline2797
    var _goml_m_______7_i_select__open bool
    select {
    case _, _goml_m_______7_i_select__open = <-t2114:
        if _goml_m_______7_i_select__open {}
        var t2115 int = _goml_m_inherent_i_std_p_task__h53c24a9f0a217bb05f370161a3b86100_oin____T__isize(work__6)
        var inline2793 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t2115)
        _goml_runtime_core_string_println(inline2793)
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env803 closure_env_main_2, scope__8 _goml_m_std_p_task_p_Scope) struct{} {
    var cancel__9 _goml_m_std_p_task_p_CancelToken
    var inline2820 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    var inline2821 _goml_m_std_p_internal_p_task_p_CancelToken = _goml_m_std_p_internal_p_task_p_token(inline2820)
    var inline2822 _goml_m_std_p_task_p_CancelToken = _goml_m_std_p_task_p_CancelToken{
        value: inline2821,
    }
    cancel__9 = inline2822
    var inline2817 _goml_m_std_p_internal_p_task_p_ScopeHandle = scope__8.handle
    _goml_m_std_p_internal_p_task_p_cancel(inline2817)
    var t2119 <-chan struct{}
    var inline2814 _goml_m_std_p_internal_p_task_p_CancelToken = cancel__9.value
    var inline2815 <-chan struct{} = _goml_m_inherent_i_std_p_inter_h25a0a7c69f0f67d3e38fa37e5f2fc20e_celToken_i_done(inline2814)
    t2119 = inline2815
    var _goml_m_______10_i_select__open bool
    select {
    case _, _goml_m_______10_i_select__open = <-t2119:
        if _goml_m_______10_i_select__open {}
        var inline2810 int = 4
        var inline2811 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline2810)
        _goml_runtime_core_string_println(inline2811)
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__en_h1a4b8fcc393b2b50e0101934341de27f_unit__3_i_apply(env804 closure_env_std_task_scope_T_unit_3) struct{} {
    var result__11 *ref_Option__unit_x = env804.result_0
    var body__9 func(_goml_m_std_p_task_p_Scope) struct{} = env804.body_1
    var handle__10 _goml_m_std_p_internal_p_task_p_ScopeHandle = env804.handle_2
    var t2122 _goml_m_std_p_task_p_Scope = _goml_m_std_p_task_p_Scope{
        handle: handle__10,
    }
    var t2123 struct{} = body__9(t2122)
    var t2124 Option__unit = Option__unit{
        _tag: 1,
        _v1_0: t2123,
    }
    ref_set__Ref_12Option__unit(result__11, t2124)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_h06a158548ef8dc096a001bc90e690a25_size__4_i_apply(env805 closure_env_inherent_std_task_Scope_std_task_Scope_spawn_T_isize_4) struct{} {
    var result__27 *ref_Option__isize_x = env805.result_0
    var body__26 func(_goml_m_std_p_task_p_CancelToken) int = env805.body_1
    var token__29 _goml_m_std_p_task_p_CancelToken = env805.token_2
    var ready__28 chan struct{} = env805.ready_3
    var t2127 int = body__26(token__29)
    var t2128 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: t2127,
    }
    ref_set__Ref_13Option__isize(result__27, t2128)
    func(p0 chan struct{}) struct{} {
        close(p0)
        return struct{}{}
    }(ready__28)
    return struct{}{}
}

func main() {
    main0()
}
