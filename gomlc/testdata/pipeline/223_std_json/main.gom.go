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

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_reserve__Vec_5uint8(vec *_goml_vec_uint8, additional int) struct{} {
    if additional < 0 {
        panic("negative vector capacity")
    }
    var length int = len(vec.items)
    var required int = length + additional
    if required < length {
        panic("vector capacity overflow")
    }
    if required > cap(vec.items) {
        var next_capacity int = cap(vec.items) * 2
        if next_capacity < required {
            next_capacity = required
        }
        var next_items []uint8 = make([]uint8, length, next_capacity)
        copy(next_items, vec.items)
        vec.items = next_items
    }
    return struct{}{}
}

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
}

type _goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema struct {
    items []Tuple2_6string_28_goml_m_std_p_serde_p_Schema
}

type _goml_vec__goml_m_std_p_serde_p_VariantSchema struct {
    items []_goml_m_std_p_serde_p_VariantSchema
}

type _goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value struct {
    items []Tuple2_6string_27_goml_m_std_p_serde_p_Value
}

type _goml_vec__goml_m_std_p_serde_p_Value struct {
    items []_goml_m_std_p_serde_p_Value
}

type _goml_vec__goml_m_std_p_serde_p_ValueDeserializeFrame struct {
    items []_goml_m_std_p_serde_p_ValueDeserializeFrame
}

type _goml_vec__goml_m_std_p_serde_p_ValueSerializeFrame struct {
    items []_goml_m_std_p_serde_p_ValueSerializeFrame
}

type _goml_vec__goml_m_std_p_json_p_JsonDeserializeFrame struct {
    items []_goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_vec__goml_m_std_p_json_p_Value struct {
    items []_goml_m_std_p_json_p_Value
}

func vec_push___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value, elem _goml_m_std_p_json_p_Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value, index int) _goml_m_std_p_json_p_Value {
    return vec.items[index]
}

func vec_len___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value) int {
    return int(len(vec.items))
}

type _goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value struct {
    items []Tuple2_6string_26_goml_m_std_p_json_p_Value
}

func vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, index int) Tuple2_6string_26_goml_m_std_p_json_p_Value {
    return vec.items[index]
}

func vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value) int {
    return int(len(vec.items))
}

type _goml_vec__goml_m_std_p_json_p_JsonSerializeFrame struct {
    items []_goml_m_std_p_json_p_JsonSerializeFrame
}

type _goml_vec__goml_m_std_p_serde_p_Schema struct {
    items []_goml_m_std_p_serde_p_Schema
}

type ref__goml_m_Option____std_p_serde_p_Value_x struct {
    value _goml_m_Option____std_p_serde_p_Value
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

type ref_Option__string_x struct {
    value Option__string
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

type Tuple2_6string_28_goml_m_std_p_serde_p_Schema struct {
    _0 string
    _1 _goml_m_std_p_serde_p_Schema
}

type Tuple2_6string_27_goml_m_std_p_serde_p_Value struct {
    _0 string
    _1 _goml_m_std_p_serde_p_Value
}

type Tuple3_6string_3int_50Vec_44Tuple2_6string_27_goml_m_std_p_serde_p_Value struct {
    _0 string
    _1 int
    _2 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

type Tuple3_4bool_4bool_4bool struct {
    _0 bool
    _1 bool
    _2 bool
}

type Tuple3_4bool_4bool_14Option__string struct {
    _0 bool
    _1 bool
    _2 Option__string
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple2_13Option__isize_13Option__isize struct {
    _0 Option__isize
    _1 Option__isize
}

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type _goml_m_std_p_num_p_ParseIntError struct {
    details _goml_m_std_p_io_p_ErrorDetails
    input_value string
    radix_value int
}

type _goml_m_std_p_num_p_ParseFloatError struct {
    details _goml_m_std_p_io_p_ErrorDetails
    input_value string
}

type _goml_m_std_p_serde_p_VariantSchema struct {
    index int
    name string
    kind int
    fields *_goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_ValueDeserializer struct {
    current *ref__goml_m_Option____std_p_serde_p_Value_x
    frames *_goml_vec__goml_m_std_p_serde_p_ValueDeserializeFrame
}

type _goml_m_std_p_serde_p_ValueSerializer struct {
    frames *_goml_vec__goml_m_std_p_serde_p_ValueSerializeFrame
    result *ref__goml_m_Option____std_p_serde_p_Value_x
}

type _goml_m_std_p_text_p_StringBuilder struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_json_p_JsonDeserializer struct {
    parser _goml_m_std_p_json_p_JsonParser
    frames *_goml_vec__goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_m_std_p_json_p_JsonParser struct {
    input string
    index *ref_int_x
}

type _goml_m_std_p_json_p_JsonSerializer struct {
    builder _goml_m_std_p_text_p_StringBuilder
    frames *_goml_vec__goml_m_std_p_json_p_JsonSerializeFrame
    root_written *ref_bool_x
}

type _goml_m_FnIterator_____o_isize_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_isize_c_char_q_
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type FnIterator__isize struct {
    next_fn func() Option__isize
}

type closure_env_inherent_string_string_char_indices_0 struct {
    index_0 *ref_int_x
    self_1 string
}

type closure_env_inherent_string_string_chars_1 struct {
    self_0 string
    index_1 *ref_int_x
}

type closure_env_goml_builtin_range_2 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type _goml_m_std_p_io_p_ErrorKind int32

type _goml_m_std_p_serde_p_FieldKey struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type _goml_m_std_p_serde_p_VariantKey struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type _goml_m_std_p_serde_p_Value interface {
    is_goml_m_std_p_serde_p_Value()
}

type _goml_m_std_p_serde_p_Value_Unit struct {}

func (_ _goml_m_std_p_serde_p_Value_Unit) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Bool struct {
    _0 bool
}

func (_ _goml_m_std_p_serde_p_Value_Bool) is_goml_m_std_p_serde_p_Value() {}

type Int struct {
    _0 int
}

func (_ Int) is_goml_m_std_p_serde_p_Value() {}

type Int8 struct {
    _0 int8
}

func (_ Int8) is_goml_m_std_p_serde_p_Value() {}

type Int16 struct {
    _0 int16
}

func (_ Int16) is_goml_m_std_p_serde_p_Value() {}

type Int32 struct {
    _0 int32
}

func (_ Int32) is_goml_m_std_p_serde_p_Value() {}

type Int64 struct {
    _0 int64
}

func (_ Int64) is_goml_m_std_p_serde_p_Value() {}

type Uint struct {
    _0 uint
}

func (_ Uint) is_goml_m_std_p_serde_p_Value() {}

type Uint8 struct {
    _0 uint8
}

func (_ Uint8) is_goml_m_std_p_serde_p_Value() {}

type Uint16 struct {
    _0 uint16
}

func (_ Uint16) is_goml_m_std_p_serde_p_Value() {}

type Uint32 struct {
    _0 uint32
}

func (_ Uint32) is_goml_m_std_p_serde_p_Value() {}

type Uint64 struct {
    _0 uint64
}

func (_ Uint64) is_goml_m_std_p_serde_p_Value() {}

type Float32 struct {
    _0 float32
}

func (_ Float32) is_goml_m_std_p_serde_p_Value() {}

type Float64 struct {
    _0 float64
}

func (_ Float64) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Number struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_Value_Number) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_String struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_Value_String) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Char struct {
    _0 rune
}

func (_ _goml_m_std_p_serde_p_Value_Char) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Sequence) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Tuple) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Optional struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Optional) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Struct) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Variant struct {
    _0 int
    _1 string
    _2 int
    _3 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Variant) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Schema interface {
    is_goml_m_std_p_serde_p_Schema()
}

type Any struct {}

func (_ Any) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Unit struct {}

func (_ _goml_m_std_p_serde_p_Schema_Unit) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Bool struct {}

func (_ _goml_m_std_p_serde_p_Schema_Bool) is_goml_m_std_p_serde_p_Schema() {}

type Integer struct {
    _0 bool
    _1 int
}

func (_ Integer) is_goml_m_std_p_serde_p_Schema() {}

type Float struct {
    _0 int
}

func (_ Float) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_String struct {}

func (_ _goml_m_std_p_serde_p_Schema_String) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Char struct {}

func (_ _goml_m_std_p_serde_p_Schema_Char) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Sequence) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Tuple) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Optional struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Optional) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Struct) is_goml_m_std_p_serde_p_Schema() {}

type Enum struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_VariantSchema
}

func (_ Enum) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame interface {
    is_goml_m_std_p_serde_p_ValueDeserializeFrame()
}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Optional struct {}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Optional) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Sequence) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Tuple) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Map struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 *ref_int_x
    _2 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Map) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _1 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Struct) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Variant struct {
    _0 int
    _1 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _2 *ref_int_x
    _3 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Variant) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame interface {
    is_goml_m_std_p_serde_p_ValueSerializeFrame()
}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Optional struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 bool
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Optional) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 int
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Sequence) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 int
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Tuple) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Map struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 int
    _2 *ref_int_x
    _3 *ref__goml_m_Option____std_p_serde_p_Value_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Map) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _1 int
    _2 *ref_Option__string_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Struct) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Variant struct {
    _0 int
    _1 string
    _2 int
    _3 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _4 int
    _5 *ref_Option__string_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Variant) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame interface {
    is_goml_m_std_p_json_p_JsonDeserializeFrame()
}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Optional struct {}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Optional) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Sequence struct {
    _0 *ref_bool_x
    _1 *ref_bool_x
    _2 *ref_int_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Sequence) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Tuple struct {
    _0 int
    _1 *ref_int_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Tuple) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Map struct {
    _0 *ref_bool_x
    _1 *ref_bool_x
    _2 *ref_int_x
    _3 *ref_Option__string_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Map) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Struct struct {
    _0 string
    _1 *ref_bool_x
    _2 *ref_bool_x
    _3 *ref_Option__string_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Struct) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Variant struct {
    _0 string
    _1 string
    _2 int
    _3 *ref_int_x
    _4 *ref_int_x
    _5 *ref_int_x
    _6 *ref_bool_x
    _7 *ref_bool_x
    _8 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Variant) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_Value interface {
    is_goml_m_std_p_json_p_Value()
}

type Object struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
}

func (_ Object) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_Array struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ _goml_m_std_p_json_p_Value_Array) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_String struct {
    _0 string
}

func (_ _goml_m_std_p_json_p_Value_String) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_Number struct {
    _0 string
}

func (_ _goml_m_std_p_json_p_Value_Number) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_Bool struct {
    _0 bool
}

func (_ _goml_m_std_p_json_p_Value_Bool) is_goml_m_std_p_json_p_Value() {}

type Null struct {}

func (_ Null) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_JsonSerializeFrame interface {
    is_goml_m_std_p_json_p_JsonSerializeFrame()
}

type _goml_m_std_p_json_p_JsonSerializeFrame_Optional struct {
    _0 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Optional) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Array struct {
    _0 int
    _1 *ref_int_x
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Array) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Map struct {
    _0 int
    _1 *ref_int_x
    _2 *ref_int_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Map) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Struct struct {
    _0 int
    _1 *ref_int_x
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Struct) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Variant struct {
    _0 int
    _1 int
    _2 *ref_int_x
    _3 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Variant) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

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

type Result__isize__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Result__usize__string struct {
    _tag int32
    _v0_0 uint
    _v1_0 string
}

type Result__f32__string struct {
    _tag int32
    _v0_0 float32
    _v1_0 string
}

type Result__f64__string struct {
    _tag int32
    _v0_0 float64
    _v1_0 string
}

type _goml_m_Result____isize____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____isize____std_p_num_p_ParseIntError()
}

type _goml_m_Result____isize____std_p_num_p_ParseIntError_Ok struct {
    _0 int
}

func (_ _goml_m_Result____isize____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____isize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____isize____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____isize____std_p_num_p_ParseIntError_Err) is_goml_m_Result____isize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____usize____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____usize____std_p_num_p_ParseIntError()
}

type _goml_m_Result____usize____std_p_num_p_ParseIntError_Ok struct {
    _0 uint
}

func (_ _goml_m_Result____usize____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____usize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____usize____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____usize____std_p_num_p_ParseIntError_Err) is_goml_m_Result____usize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____f32____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____f32____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____f32____std_p_num_p_ParseFloatError_Ok struct {
    _0 float32
}

func (_ _goml_m_Result____f32____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____f32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____f32____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____f32____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____f32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____f64____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____f64____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____f64____std_p_num_p_ParseFloatError_Ok struct {
    _0 float64
}

func (_ _goml_m_Result____f64____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____f64____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____f64____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____f64____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____f64____std_p_num_p_ParseFloatError() {}

type Option__i64 struct {
    _tag int32
    _v1_0 int64
}

type _goml_m_Option____std_p_serde_p_FieldKey struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_FieldKey
}

type _goml_m_Result____std_p_serde_p_Value____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_serde_p_Value
    _v1_0 string
}

type _goml_m_Result_____o_string_c__h0fd03a31a9a9f0946c70cfe7367b8f54_r__q_____string struct {
    _tag int32
    _v0_0 Tuple3_6string_3int_50Vec_44Tuple2_6string_27_goml_m_std_p_serde_p_Value
    _v1_0 string
}

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec__goml_m_std_p_serde_p_Value
    _v1_0 string
}

type _goml_m_Option____std_p_serde_p_Value struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_Value
}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_ValueDeserializeFrame
}

type _goml_m_Result____Vec_l_u8_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec_uint8
    _v1_0 string
}

type Result__bool__string struct {
    _tag int32
    _v0_0 bool
    _v1_0 string
}

type Result__i8__string struct {
    _tag int32
    _v0_0 int8
    _v1_0 string
}

type Result__i16__string struct {
    _tag int32
    _v0_0 int16
    _v1_0 string
}

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

type Result__i64__string struct {
    _tag int32
    _v0_0 int64
    _v1_0 string
}

type Result__u8__string struct {
    _tag int32
    _v0_0 uint8
    _v1_0 string
}

type Result__u16__string struct {
    _tag int32
    _v0_0 uint16
    _v1_0 string
}

type Result__u32__string struct {
    _tag int32
    _v0_0 uint32
    _v1_0 string
}

type Result__u64__string struct {
    _tag int32
    _v0_0 uint64
    _v1_0 string
}

type Result__char__string struct {
    _tag int32
    _v0_0 rune
    _v1_0 string
}

type Result__Option__isize__string struct {
    _tag int32
    _v0_0 Option__isize
    _v1_0 string
}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string struct {
    _tag int32
    _v0_0 _goml_m_Option____std_p_serde_p_FieldKey
    _v1_0 string
}

type _goml_m_Result____std_p_serde_p_VariantKey____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_serde_p_VariantKey
    _v1_0 string
}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_ValueSerializeFrame
}

type _goml_m_Option_____o_isize_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_string_c_string_q_ struct {
    _tag int32
    _v1_0 Tuple2_6string_6string
}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string struct {
    _tag int32
    _v0_0 Tuple3_4bool_4bool_4bool
    _v1_0 string
}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string struct {
    _tag int32
    _v0_0 Tuple3_4bool_4bool_14Option__string
    _v1_0 string
}

type Result__Option__string__string struct {
    _tag int32
    _v0_0 Option__string
    _v1_0 string
}

type _goml_m_Result____std_p_json_p_Value____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_json_p_Value
    _v1_0 string
}

type _goml_m_Option_____o_char_c_isize_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__u32 struct {
    _tag int32
    _v1_0 uint32
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

type _goml_m_Option____std_p_json_p_Value struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_Value
}

type Option__bool struct {
    _tag int32
    _v1_0 bool
}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r_ struct {
    _tag int32
    _v1_0 *_goml_vec__goml_m_std_p_json_p_Value
}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_JsonSerializeFrame
}

func _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new() _goml_m_std_p_text_p_StringBuilder {
    var t2918 [0]uint8 = [0]uint8{}
    var t2919 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(t2918)
    var t2920 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t2919,
    }
    return t2920
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8747 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8747
    var t2934 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2934, length__5)
    var for_index1 int = 0
    Loop_loop2936:
    for {
        var t2937 bool = for_index1 < length__5
        if t2937 {
            var for_item3 int = for_index1
            var t2938 int = for_index1 + 1
            for_index1 = t2938
            var t2939 *_goml_vec_uint8 = self__3.values
            var t2940 uint8
            var inline8743 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2940 = inline8743
            vec_push__Vec_5uint8(t2939, t2940)
            continue
        } else {
            break Loop_loop2936
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2943 string
    var inline8749 string = char_to_string(value__8)
    t2943 = inline8749
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2943)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4651 string = "" + message__201
    var t4652 string = t4651 + " at byte "
    var t4653 *ref_int_x = value__200.index
    var t4654 int
    var inline10084 int = ref_get__Ref_3int(t4653)
    t4654 = inline10084
    var t4655 string
    var inline10082 string = _goml_runtime_core_int_to_string(t4654)
    t4655 = inline10082
    var t4656 string = t4652 + t4655
    return t4656
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4671:
    for {
        var t4679 *ref_int_x = value__203.index
        var t4680 int
        var inline10105 int = ref_get__Ref_3int(t4679)
        t4680 = inline10105
        var t4681 string = value__203.input
        var t4682 int
        var inline10103 int = _goml_runtime_core_string_len(t4681)
        t4682 = inline10103
        var t4683 bool = t4680 < t4682
        var jp4673 bool
        if t4683 {
            var t4684 string = value__203.input
            var t4685 *ref_int_x = value__203.index
            var t4686 int
            var inline10097 int = ref_get__Ref_3int(t4685)
            t4686 = inline10097
            var t4687 uint8
            var inline10095 uint8 = _goml_runtime_core_string_byte_get(t4684, t4686)
            t4687 = inline10095
            var inline10086 bool = t4687 == 9
            var inline10088 bool
            if inline10086 {
                inline10088 = true
            } else {
                var inline10093 bool = t4687 == 10
                inline10088 = inline10093
            }
            var inline10090 bool
            if inline10088 {
                inline10090 = true
            } else {
                var inline10092 bool = t4687 == 13
                inline10090 = inline10092
            }
            if inline10090 {
                jp4673 = true
            } else {
                var inline10091 bool = t4687 == 32
                jp4673 = inline10091
            }
        } else {
            jp4673 = false
        }
        if jp4673 {
            var t4674 *ref_int_x = value__203.index
            var t4675 *ref_int_x = value__203.index
            var t4676 int
            var inline10101 int = ref_get__Ref_3int(t4675)
            t4676 = inline10101
            var t4677 int = t4676 + 1
            ref_set__Ref_3int(t4674, t4677)
            continue
        } else {
            break Loop_loop4671
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__u32 {
    var t4718 bool = value__204 >= 48
    var jp4694 bool
    if t4718 {
        var t4719 bool = value__204 <= 57
        jp4694 = t4719
    } else {
        jp4694 = false
    }
    if jp4694 {
        var t4695 uint8 = value__204 - 48
        var t4696 uint32 = uint32(uint8(t4695))
        var t4697 Option__u32 = Option__u32{
            _tag: 1,
            _v1_0: t4696,
        }
        return t4697
    } else {
        var t4716 bool = value__204 >= 65
        var jp4701 bool
        if t4716 {
            var t4717 bool = value__204 <= 70
            jp4701 = t4717
        } else {
            jp4701 = false
        }
        if jp4701 {
            var t4702 uint8 = value__204 - 65
            var t4703 uint8 = t4702 + 10
            var t4704 uint32 = uint32(uint8(t4703))
            var t4705 Option__u32 = Option__u32{
                _tag: 1,
                _v1_0: t4704,
            }
            return t4705
        } else {
            var t4714 bool = value__204 >= 97
            var jp4709 bool
            if t4714 {
                var t4715 bool = value__204 <= 102
                jp4709 = t4715
            } else {
                jp4709 = false
            }
            if jp4709 {
                var t4710 uint8 = value__204 - 97
                var t4711 uint8 = t4710 + 10
                var t4712 uint32 = uint32(uint8(t4711))
                var t4713 Option__u32 = Option__u32{
                    _tag: 1,
                    _v1_0: t4712,
                }
                return t4713
            } else {
                return Option__u32{
                    _tag: 0,
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__u32__string {
    var t4724 *ref_int_x = value__205.index
    var t4725 int
    var inline10133 int = ref_get__Ref_3int(t4724)
    t4725 = inline10133
    var t4726 int = t4725 + 4
    var t4727 string = value__205.input
    var t4728 int
    var inline10131 int = _goml_runtime_core_string_len(t4727)
    t4728 = inline10131
    var t4729 bool = t4726 > t4728
    if t4729 {
        var t4730 string
        var inline10107 string = "incomplete unicode escape"
        var inline10108 string = "" + inline10107
        var inline10109 string = inline10108 + " at byte "
        var inline10110 *ref_int_x = value__205.index
        var inline10111 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10110)
        var inline10112 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10111)
        var inline10113 string = inline10109 + inline10112
        t4730 = inline10113
        var t4731 Result__u32__string = Result__u32__string{
            _tag: 1,
            _v1_0: t4730,
        }
        return t4731
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4738:
        for {
            var t4739 bool = for_index744 < for_limit745
            if t4739 {
                var for_item746 int = for_index744
                var t4740 int = for_index744 + 1
                for_index744 = t4740
                var t4741 string = value__205.input
                var t4742 *ref_int_x = value__205.index
                var t4743 int
                var inline10125 int = ref_get__Ref_3int(t4742)
                t4743 = inline10125
                var t4744 int = t4743 + for_item746
                var t4745 uint8
                var inline10123 uint8 = _goml_runtime_core_string_byte_get(t4741, t4744)
                t4745 = inline10123
                var mtmp748 Option__u32 = _goml_m_std_p_json_p_hex__digit(t4745)
                switch mtmp748._tag {
                case 0:
                    var t4747 string
                    var inline10115 string = "invalid unicode escape"
                    var inline10116 string = "" + inline10115
                    var inline10117 string = inline10116 + " at byte "
                    var inline10118 *ref_int_x = value__205.index
                    var inline10119 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10118)
                    var inline10120 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10119)
                    var inline10121 string = inline10117 + inline10120
                    t4747 = inline10121
                    var t4748 Result__u32__string = Result__u32__string{
                        _tag: 1,
                        _v1_0: t4747,
                    }
                    return t4748
                case 1:
                    var x749 uint32 = mtmp748._v1_0
                    var t4749 uint32 = result__206 * 16
                    var t4750 uint32 = t4749 + x749
                    result__206 = t4750
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4738
            }
        }
        var t4733 *ref_int_x = value__205.index
        var t4734 *ref_int_x = value__205.index
        var t4735 int
        var inline10129 int = ref_get__Ref_3int(t4734)
        t4735 = inline10129
        var t4736 int = t4735 + 4
        ref_set__Ref_3int(t4733, t4736)
        var t4737 Result__u32__string = Result__u32__string{
            _tag: 0,
            _v0_0: result__206,
        }
        return t4737
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline10146 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline10146
    switch mtmp753._tag {
    case 0:
        var t4755 string
        var inline10135 string = "invalid unicode codepoint"
        var inline10136 string = "" + inline10135
        var inline10137 string = inline10136 + " at byte "
        var inline10138 *ref_int_x = value__209.index
        var inline10139 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10138)
        var inline10140 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10139)
        var inline10141 string = inline10137 + inline10140
        t4755 = inline10141
        var t4756 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: t4755,
        }
        return t4756
    case 1:
        var x754 rune = mtmp753._v1_0
        var inline10143 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10143)
        var t4757 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t4757
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__u32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4761 uint32
    switch mtmp756._tag {
    case 0:
        var x757 uint32 = mtmp756._v0_0
        jp4761 = x757
        var t4821 bool = jp4761 >= 55296
        var jp4765 bool
        if t4821 {
            var t4822 bool = jp4761 <= 56319
            jp4765 = t4822
        } else {
            jp4765 = false
        }
        if jp4765 {
            var t4801 *ref_int_x = value__213.index
            var t4802 int
            var inline10186 int = ref_get__Ref_3int(t4801)
            t4802 = inline10186
            var t4803 int = t4802 + 2
            var t4804 string = value__213.input
            var t4805 int
            var inline10184 int = _goml_runtime_core_string_len(t4804)
            t4805 = inline10184
            var t4806 bool = t4803 > t4805
            var jp4794 bool
            if t4806 {
                jp4794 = true
            } else {
                var t4807 string = value__213.input
                var t4808 *ref_int_x = value__213.index
                var t4809 int
                var inline10150 int = ref_get__Ref_3int(t4808)
                t4809 = inline10150
                var t4810 uint8
                var inline10148 uint8 = _goml_runtime_core_string_byte_get(t4807, t4809)
                t4810 = inline10148
                var t4811 bool = t4810 != 92
                jp4794 = t4811
            }
            var jp4769 bool
            if jp4794 {
                jp4769 = true
            } else {
                var t4795 string = value__213.input
                var t4796 *ref_int_x = value__213.index
                var t4797 int
                var inline10154 int = ref_get__Ref_3int(t4796)
                t4797 = inline10154
                var t4798 int = t4797 + 1
                var t4799 uint8
                var inline10152 uint8 = _goml_runtime_core_string_byte_get(t4795, t4798)
                t4799 = inline10152
                var t4800 bool = t4799 != 117
                jp4769 = t4800
            }
            if jp4769 {
                var t4770 string
                var inline10156 string = "missing low surrogate"
                var inline10157 string = "" + inline10156
                var inline10158 string = inline10157 + " at byte "
                var inline10159 *ref_int_x = value__213.index
                var inline10160 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10159)
                var inline10161 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10160)
                var inline10162 string = inline10158 + inline10161
                t4770 = inline10162
                var t4771 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t4770,
                }
                return t4771
            } else {
                var t4772 *ref_int_x = value__213.index
                var t4773 *ref_int_x = value__213.index
                var t4774 int
                var inline10182 int = ref_get__Ref_3int(t4773)
                t4774 = inline10182
                var t4775 int = t4774 + 2
                ref_set__Ref_3int(t4772, t4775)
                var mtmp760 Result__u32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4777 uint32
                switch mtmp760._tag {
                case 0:
                    var x761 uint32 = mtmp760._v0_0
                    jp4777 = x761
                    var t4790 bool = jp4777 < 56320
                    var jp4781 bool
                    if t4790 {
                        jp4781 = true
                    } else {
                        var t4791 bool = jp4777 > 57343
                        jp4781 = t4791
                    }
                    if jp4781 {
                        var t4782 string
                        var inline10164 string = "invalid low surrogate"
                        var inline10165 string = "" + inline10164
                        var inline10166 string = inline10165 + " at byte "
                        var inline10167 *ref_int_x = value__213.index
                        var inline10168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10167)
                        var inline10169 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10168)
                        var inline10170 string = inline10166 + inline10169
                        t4782 = inline10170
                        var t4783 Result__unit__string = Result__unit__string{
                            _tag: 1,
                            _v1_0: t4782,
                        }
                        return t4783
                    } else {
                        var t4784 uint32 = jp4761 - 55296
                        var t4785 uint32 = t4784 * 1024
                        var t4786 uint32 = 65536 + t4785
                        var t4787 uint32 = t4786 + jp4777
                        var t4788 uint32 = t4787 - 56320
                        var inline10172 Option__char = char_from_u32(t4788)
                        switch inline10172._tag {
                        case 0:
                            var inline10173 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10174 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: inline10173,
                            }
                            return inline10174
                        case 1:
                            var inline10175 rune = inline10172._v1_0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10175)
                            var inline10178 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return inline10178
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x762 string = mtmp760._v1_0
                    var t4792 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x762,
                    }
                    return t4792
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4819 bool = jp4761 >= 56320
            var jp4815 bool
            if t4819 {
                var t4820 bool = jp4761 <= 57343
                jp4815 = t4820
            } else {
                jp4815 = false
            }
            if jp4815 {
                var t4816 string
                var inline10188 string = "unexpected low surrogate"
                var inline10189 string = "" + inline10188
                var inline10190 string = inline10189 + " at byte "
                var inline10191 *ref_int_x = value__213.index
                var inline10192 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10191)
                var inline10193 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10192)
                var inline10194 string = inline10190 + inline10193
                t4816 = inline10194
                var t4817 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t4816,
                }
                return t4817
            } else {
                var t4818 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4761)
                return t4818
            }
        }
    case 1:
        var x758 string = mtmp756._v1_0
        var t4823 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x758,
        }
        return t4823
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4939 *ref_int_x = value__217.index
    var t4940 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4939)
    var t4941 string = value__217.input
    var t4942 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4941)
    var t4943 bool = t4940 >= t4942
    var jp4931 bool
    if t4943 {
        jp4931 = true
    } else {
        var t4944 string = value__217.input
        var t4945 *ref_int_x = value__217.index
        var t4946 int
        var inline10198 int = ref_get__Ref_3int(t4945)
        t4946 = inline10198
        var t4947 uint8
        var inline10196 uint8 = _goml_runtime_core_string_byte_get(t4944, t4946)
        t4947 = inline10196
        var t4948 bool = t4947 != 34
        jp4931 = t4948
    }
    if jp4931 {
        var t4932 string
        var inline10200 string = "expected string"
        var inline10201 string = "" + inline10200
        var inline10202 string = inline10201 + " at byte "
        var inline10203 *ref_int_x = value__217.index
        var inline10204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10203)
        var inline10205 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10204)
        var inline10206 string = inline10202 + inline10205
        t4932 = inline10206
        var t4933 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t4932,
        }
        return t4933
    } else {
        var t4934 *ref_int_x = value__217.index
        var t4935 *ref_int_x = value__217.index
        var t4936 int
        var inline10210 int = ref_get__Ref_3int(t4935)
        t4936 = inline10210
        var t4937 int = t4936 + 1
        ref_set__Ref_3int(t4934, t4937)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4827 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4827)
        Loop_loop4831:
        for {
            var t4832 *ref_int_x = value__217.index
            var t4833 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4832)
            var t4834 string = value__217.input
            var t4835 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4834)
            var t4836 bool = t4833 < t4835
            if t4836 {
                var t4837 string = value__217.input
                var t4838 *ref_int_x = value__217.index
                var t4839 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4838)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4837, t4839)
                var t4841 bool = byte__220 == 34
                if t4841 {
                    var t4849 *ref_int_x = value__217.index
                    var t4850 int
                    var inline10225 int = ref_get__Ref_3int(t4849)
                    t4850 = inline10225
                    var t4851 bool = segment__219 < t4850
                    if t4851 {
                        var t4852 string = value__217.input
                        var t4853 *ref_int_x = value__217.index
                        var t4854 int
                        var inline10214 int = ref_get__Ref_3int(t4853)
                        t4854 = inline10214
                        var t4855 string
                        var inline10212 string = string_byte_slice(t4852, segment__219, t4854)
                        t4855 = inline10212
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4855)
                    } else {}
                    var t4843 *ref_int_x = value__217.index
                    var t4844 *ref_int_x = value__217.index
                    var t4845 int
                    var inline10223 int = ref_get__Ref_3int(t4844)
                    t4845 = inline10223
                    var t4846 int = t4845 + 1
                    ref_set__Ref_3int(t4843, t4846)
                    var t4847 string
                    var inline10216 *_goml_vec_uint8 = builder__218.values
                    var inline10217 Tuple2_4bool_6string = string_from_utf8(inline10216)
                    var inline10218 string = inline10217._1
                    t4847 = inline10218
                    var t4848 Result__string__string = Result__string__string{
                        _tag: 0,
                        _v0_0: t4847,
                    }
                    return t4848
                } else {
                    var t4858 bool = byte__220 == 92
                    if t4858 {
                        var t4913 *ref_int_x = value__217.index
                        var t4914 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4913)
                        var t4915 bool = segment__219 < t4914
                        if t4915 {
                            var t4916 string = value__217.input
                            var t4917 *ref_int_x = value__217.index
                            var t4918 int
                            var inline10229 int = ref_get__Ref_3int(t4917)
                            t4918 = inline10229
                            var t4919 string
                            var inline10227 string = string_byte_slice(t4916, segment__219, t4918)
                            t4919 = inline10227
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4919)
                        } else {}
                        var t4860 *ref_int_x = value__217.index
                        var t4861 *ref_int_x = value__217.index
                        var t4862 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4861)
                        var t4863 int = t4862 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4860, t4863)
                        var t4906 *ref_int_x = value__217.index
                        var t4907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4906)
                        var t4908 string = value__217.input
                        var t4909 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4908)
                        var t4910 bool = t4907 >= t4909
                        if t4910 {
                            var t4911 string
                            var inline10231 string = "incomplete escape"
                            var inline10232 string = "" + inline10231
                            var inline10233 string = inline10232 + " at byte "
                            var inline10234 *ref_int_x = value__217.index
                            var inline10235 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10234)
                            var inline10236 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10235)
                            var inline10237 string = inline10233 + inline10236
                            t4911 = inline10237
                            var t4912 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t4911,
                            }
                            return t4912
                        } else {
                            var t4865 string = value__217.input
                            var t4866 *ref_int_x = value__217.index
                            var t4867 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4866)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4865, t4867)
                            var t4868 *ref_int_x = value__217.index
                            var t4869 *ref_int_x = value__217.index
                            var t4870 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4869)
                            var t4871 int = t4870 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4868, t4871)
                            var t4875 bool = escape__221 == 34
                            if t4875 {
                                var inline10239 rune = 34
                                var inline10240 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10239)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10240)
                                var t4873 *ref_int_x = value__217.index
                                var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                segment__219 = t4874
                                continue
                            } else {
                                var t4878 bool = escape__221 == 92
                                if t4878 {
                                    var inline10243 rune = 92
                                    var inline10244 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10243)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10244)
                                    var t4873 *ref_int_x = value__217.index
                                    var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                    segment__219 = t4874
                                    continue
                                } else {
                                    var t4881 bool = escape__221 == 47
                                    if t4881 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4873 *ref_int_x = value__217.index
                                        var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                        segment__219 = t4874
                                        continue
                                    } else {
                                        var t4884 bool = escape__221 == 98
                                        if t4884 {
                                            var mtmp770 Option__char = char_from_u32(8)
                                            switch mtmp770._tag {
                                            case 0:
                                                var t4873 *ref_int_x = value__217.index
                                                var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                segment__219 = t4874
                                                continue
                                            case 1:
                                                var x771 rune = mtmp770._v1_0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4873 *ref_int_x = value__217.index
                                                var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                segment__219 = t4874
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4888 bool = escape__221 == 102
                                            if t4888 {
                                                var mtmp772 Option__char = char_from_u32(12)
                                                switch mtmp772._tag {
                                                case 0:
                                                    var t4873 *ref_int_x = value__217.index
                                                    var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                    segment__219 = t4874
                                                    continue
                                                case 1:
                                                    var x773 rune = mtmp772._v1_0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4873 *ref_int_x = value__217.index
                                                    var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                    segment__219 = t4874
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4892 bool = escape__221 == 110
                                                if t4892 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4873 *ref_int_x = value__217.index
                                                    var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                    segment__219 = t4874
                                                    continue
                                                } else {
                                                    var t4895 bool = escape__221 == 114
                                                    if t4895 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4873 *ref_int_x = value__217.index
                                                        var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                        segment__219 = t4874
                                                        continue
                                                    } else {
                                                        var t4898 bool = escape__221 == 116
                                                        if t4898 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4873 *ref_int_x = value__217.index
                                                            var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                            segment__219 = t4874
                                                            continue
                                                        } else {
                                                            var t4901 bool = escape__221 == 117
                                                            if t4901 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774._tag {
                                                                case 0:
                                                                    var t4873 *ref_int_x = value__217.index
                                                                    var t4874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4873)
                                                                    segment__219 = t4874
                                                                    continue
                                                                case 1:
                                                                    var x776 string = mtmp774._v1_0
                                                                    var t4903 Result__string__string = Result__string__string{
                                                                        _tag: 1,
                                                                        _v1_0: x776,
                                                                    }
                                                                    return t4903
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4904 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4905 Result__string__string = Result__string__string{
                                                                    _tag: 1,
                                                                    _v1_0: t4904,
                                                                }
                                                                return t4905
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        var t4922 bool = byte__220 < 32
                        if t4922 {
                            var t4923 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4924 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t4923,
                            }
                            return t4924
                        } else {
                            var t4925 *ref_int_x = value__217.index
                            var t4926 *ref_int_x = value__217.index
                            var t4927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4926)
                            var t4928 int = t4927 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4925, t4928)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4831
            }
        }
        var t4829 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4830 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t4829,
        }
        return t4830
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4957 *ref_int_x = value__225.index
    var start__226 int
    var inline10264 int = ref_get__Ref_3int(t4957)
    start__226 = inline10264
    Loop_loop4962:
    for {
        var t4970 *ref_int_x = value__225.index
        var t4971 int
        var inline10260 int = ref_get__Ref_3int(t4970)
        t4971 = inline10260
        var t4972 string = value__225.input
        var t4973 int
        var inline10258 int = _goml_runtime_core_string_len(t4972)
        t4973 = inline10258
        var t4974 bool = t4971 < t4973
        var jp4964 bool
        if t4974 {
            var t4975 string = value__225.input
            var t4976 *ref_int_x = value__225.index
            var t4977 int
            var inline10252 int = ref_get__Ref_3int(t4976)
            t4977 = inline10252
            var t4978 uint8
            var inline10250 uint8 = _goml_runtime_core_string_byte_get(t4975, t4977)
            t4978 = inline10250
            var inline10247 bool = t4978 >= 48
            if inline10247 {
                var inline10248 bool = t4978 <= 57
                jp4964 = inline10248
            } else {
                jp4964 = false
            }
        } else {
            jp4964 = false
        }
        if jp4964 {
            var t4965 *ref_int_x = value__225.index
            var t4966 *ref_int_x = value__225.index
            var t4967 int
            var inline10256 int = ref_get__Ref_3int(t4966)
            t4967 = inline10256
            var t4968 int = t4967 + 1
            ref_set__Ref_3int(t4965, t4968)
            continue
        } else {
            break Loop_loop4962
        }
    }
    var t4959 *ref_int_x = value__225.index
    var t4960 int
    var inline10262 int = ref_get__Ref_3int(t4959)
    t4960 = inline10262
    var t4961 bool = t4960 > start__226
    return t4961
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4982 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4982)
    var t5103 string = value__227.input
    var t5104 *ref_int_x = value__227.index
    var t5105 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5104)
    var t5106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5103, t5105)
    var t5107 bool = t5106 == 45
    if t5107 {
        var t5108 *ref_int_x = value__227.index
        var t5109 *ref_int_x = value__227.index
        var t5110 int
        var inline10268 int = ref_get__Ref_3int(t5109)
        t5110 = inline10268
        var t5111 int = t5110 + 1
        ref_set__Ref_3int(t5108, t5111)
    } else {}
    var t5066 *ref_int_x = value__227.index
    var t5067 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5066)
    var t5068 string = value__227.input
    var t5069 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5068)
    var t5070 bool = t5067 >= t5069
    if t5070 {
        var t5071 string
        var inline10270 string = "incomplete number"
        var inline10271 string = "" + inline10270
        var inline10272 string = inline10271 + " at byte "
        var inline10273 *ref_int_x = value__227.index
        var inline10274 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10273)
        var inline10275 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10274)
        var inline10276 string = inline10272 + inline10275
        t5071 = inline10276
        var t5072 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5071,
        }
        return t5072
    } else {
        var t5074 string = value__227.input
        var t5075 *ref_int_x = value__227.index
        var t5076 int
        var inline10311 int = ref_get__Ref_3int(t5075)
        t5076 = inline10311
        var t5077 uint8
        var inline10309 uint8 = _goml_runtime_core_string_byte_get(t5074, t5076)
        t5077 = inline10309
        var t5078 bool = t5077 == 48
        if t5078 {
            var t5079 *ref_int_x = value__227.index
            var t5080 *ref_int_x = value__227.index
            var t5081 int
            var inline10299 int = ref_get__Ref_3int(t5080)
            t5081 = inline10299
            var t5082 int = t5081 + 1
            ref_set__Ref_3int(t5079, t5082)
            var t5088 *ref_int_x = value__227.index
            var t5089 int
            var inline10295 int = ref_get__Ref_3int(t5088)
            t5089 = inline10295
            var t5090 string = value__227.input
            var t5091 int
            var inline10293 int = _goml_runtime_core_string_len(t5090)
            t5091 = inline10293
            var t5092 bool = t5089 < t5091
            var jp5085 bool
            if t5092 {
                var t5093 string = value__227.input
                var t5094 *ref_int_x = value__227.index
                var t5095 int
                var inline10283 int = ref_get__Ref_3int(t5094)
                t5095 = inline10283
                var t5096 uint8
                var inline10281 uint8 = _goml_runtime_core_string_byte_get(t5093, t5095)
                t5096 = inline10281
                var inline10278 bool = t5096 >= 48
                if inline10278 {
                    var inline10279 bool = t5096 <= 57
                    jp5085 = inline10279
                } else {
                    jp5085 = false
                }
            } else {
                jp5085 = false
            }
            if jp5085 {
                var t5086 string
                var inline10285 string = "invalid leading zero"
                var inline10286 string = "" + inline10285
                var inline10287 string = inline10286 + " at byte "
                var inline10288 *ref_int_x = value__227.index
                var inline10289 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10288)
                var inline10290 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10289)
                var inline10291 string = inline10287 + inline10290
                t5086 = inline10291
                var t5087 Result__string__string = Result__string__string{
                    _tag: 1,
                    _v1_0: t5086,
                }
                return t5087
            } else {
                var t5056 *ref_int_x = value__227.index
                var t5057 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5056)
                var t5058 string = value__227.input
                var t5059 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5058)
                var t5060 bool = t5057 < t5059
                var jp5046 bool
                if t5060 {
                    var t5061 string = value__227.input
                    var t5062 *ref_int_x = value__227.index
                    var t5063 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5062)
                    var t5064 uint8
                    var inline10313 uint8 = _goml_runtime_core_string_byte_get(t5061, t5063)
                    t5064 = inline10313
                    var t5065 bool = t5064 == 46
                    jp5046 = t5065
                } else {
                    jp5046 = false
                }
                if jp5046 {
                    var t5047 *ref_int_x = value__227.index
                    var t5048 *ref_int_x = value__227.index
                    var t5049 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5048)
                    var t5050 int = t5049 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5047, t5050)
                    var t5052 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5053 bool = !t5052
                    if t5053 {
                        var t5054 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5055 Result__string__string = Result__string__string{
                            _tag: 1,
                            _v1_0: t5054,
                        }
                        return t5055
                    } else {
                        var t5028 *ref_int_x = value__227.index
                        var t5029 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5028)
                        var t5030 string = value__227.input
                        var t5031 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5030)
                        var t5032 bool = t5029 < t5031
                        var jp4993 bool
                        if t5032 {
                            var t5035 string = value__227.input
                            var t5036 *ref_int_x = value__227.index
                            var t5037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5036)
                            var t5038 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5035, t5037)
                            var t5039 bool = t5038 == 101
                            if t5039 {
                                jp4993 = true
                            } else {
                                var t5040 string = value__227.input
                                var t5041 *ref_int_x = value__227.index
                                var t5042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5041)
                                var t5043 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5040, t5042)
                                var t5044 bool = t5043 == 69
                                jp4993 = t5044
                            }
                        } else {
                            jp4993 = false
                        }
                        if jp4993 {
                            var t4994 *ref_int_x = value__227.index
                            var t4995 *ref_int_x = value__227.index
                            var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4995)
                            var t4997 int = t4996 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4994, t4997)
                            var t5011 *ref_int_x = value__227.index
                            var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5011)
                            var t5013 string = value__227.input
                            var t5014 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5013)
                            var t5015 bool = t5012 < t5014
                            var jp5005 bool
                            if t5015 {
                                var t5018 string = value__227.input
                                var t5019 *ref_int_x = value__227.index
                                var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5019)
                                var t5021 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5018, t5020)
                                var t5022 bool = t5021 == 43
                                if t5022 {
                                    jp5005 = true
                                } else {
                                    var t5023 string = value__227.input
                                    var t5024 *ref_int_x = value__227.index
                                    var t5025 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5024)
                                    var t5026 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5023, t5025)
                                    var t5027 bool = t5026 == 45
                                    jp5005 = t5027
                                }
                            } else {
                                jp5005 = false
                            }
                            if jp5005 {
                                var t5006 *ref_int_x = value__227.index
                                var t5007 *ref_int_x = value__227.index
                                var t5008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5007)
                                var t5009 int = t5008 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5006, t5009)
                            } else {}
                            var t5000 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t5001 bool = !t5000
                            if t5001 {
                                var t5002 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t5003 Result__string__string = Result__string__string{
                                    _tag: 1,
                                    _v1_0: t5002,
                                }
                                return t5003
                            } else {
                                var t4987 string = value__227.input
                                var t4988 *ref_int_x = value__227.index
                                var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                                var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                                var t4991 Result__string__string = Result__string__string{
                                    _tag: 0,
                                    _v0_0: t4990,
                                }
                                return t4991
                            }
                        } else {
                            var t4987 string = value__227.input
                            var t4988 *ref_int_x = value__227.index
                            var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                            var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                            var t4991 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t4990,
                            }
                            return t4991
                        }
                    }
                } else {
                    var t5028 *ref_int_x = value__227.index
                    var t5029 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5028)
                    var t5030 string = value__227.input
                    var t5031 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5030)
                    var t5032 bool = t5029 < t5031
                    var jp4993 bool
                    if t5032 {
                        var t5035 string = value__227.input
                        var t5036 *ref_int_x = value__227.index
                        var t5037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5036)
                        var t5038 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5035, t5037)
                        var t5039 bool = t5038 == 101
                        if t5039 {
                            jp4993 = true
                        } else {
                            var t5040 string = value__227.input
                            var t5041 *ref_int_x = value__227.index
                            var t5042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5041)
                            var t5043 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5040, t5042)
                            var t5044 bool = t5043 == 69
                            jp4993 = t5044
                        }
                    } else {
                        jp4993 = false
                    }
                    if jp4993 {
                        var t4994 *ref_int_x = value__227.index
                        var t4995 *ref_int_x = value__227.index
                        var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4995)
                        var t4997 int = t4996 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4994, t4997)
                        var t5011 *ref_int_x = value__227.index
                        var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5011)
                        var t5013 string = value__227.input
                        var t5014 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5013)
                        var t5015 bool = t5012 < t5014
                        var jp5005 bool
                        if t5015 {
                            var t5018 string = value__227.input
                            var t5019 *ref_int_x = value__227.index
                            var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5019)
                            var t5021 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5018, t5020)
                            var t5022 bool = t5021 == 43
                            if t5022 {
                                jp5005 = true
                            } else {
                                var t5023 string = value__227.input
                                var t5024 *ref_int_x = value__227.index
                                var t5025 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5024)
                                var t5026 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5023, t5025)
                                var t5027 bool = t5026 == 45
                                jp5005 = t5027
                            }
                        } else {
                            jp5005 = false
                        }
                        if jp5005 {
                            var t5006 *ref_int_x = value__227.index
                            var t5007 *ref_int_x = value__227.index
                            var t5008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5007)
                            var t5009 int = t5008 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5006, t5009)
                        } else {}
                        var t5000 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t5001 bool = !t5000
                        if t5001 {
                            var t5002 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t5003 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5002,
                            }
                            return t5003
                        } else {
                            var t4987 string = value__227.input
                            var t4988 *ref_int_x = value__227.index
                            var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                            var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                            var t4991 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t4990,
                            }
                            return t4991
                        }
                    } else {
                        var t4987 string = value__227.input
                        var t4988 *ref_int_x = value__227.index
                        var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                        var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                        var t4991 Result__string__string = Result__string__string{
                            _tag: 0,
                            _v0_0: t4990,
                        }
                        return t4991
                    }
                }
            }
        } else {
            var t5099 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5100 bool = !t5099
            if t5100 {
                var t5101 string
                var inline10301 string = "expected number"
                var inline10302 string = "" + inline10301
                var inline10303 string = inline10302 + " at byte "
                var inline10304 *ref_int_x = value__227.index
                var inline10305 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10304)
                var inline10306 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10305)
                var inline10307 string = inline10303 + inline10306
                t5101 = inline10307
                var t5102 Result__string__string = Result__string__string{
                    _tag: 1,
                    _v1_0: t5101,
                }
                return t5102
            } else {
                var t5056 *ref_int_x = value__227.index
                var t5057 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5056)
                var t5058 string = value__227.input
                var t5059 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5058)
                var t5060 bool = t5057 < t5059
                var jp5046 bool
                if t5060 {
                    var t5061 string = value__227.input
                    var t5062 *ref_int_x = value__227.index
                    var t5063 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5062)
                    var t5064 uint8
                    var inline10313 uint8 = _goml_runtime_core_string_byte_get(t5061, t5063)
                    t5064 = inline10313
                    var t5065 bool = t5064 == 46
                    jp5046 = t5065
                } else {
                    jp5046 = false
                }
                if jp5046 {
                    var t5047 *ref_int_x = value__227.index
                    var t5048 *ref_int_x = value__227.index
                    var t5049 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5048)
                    var t5050 int = t5049 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5047, t5050)
                    var t5052 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5053 bool = !t5052
                    if t5053 {
                        var t5054 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5055 Result__string__string = Result__string__string{
                            _tag: 1,
                            _v1_0: t5054,
                        }
                        return t5055
                    } else {
                        var t5028 *ref_int_x = value__227.index
                        var t5029 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5028)
                        var t5030 string = value__227.input
                        var t5031 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5030)
                        var t5032 bool = t5029 < t5031
                        var jp4993 bool
                        if t5032 {
                            var t5035 string = value__227.input
                            var t5036 *ref_int_x = value__227.index
                            var t5037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5036)
                            var t5038 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5035, t5037)
                            var t5039 bool = t5038 == 101
                            if t5039 {
                                jp4993 = true
                            } else {
                                var t5040 string = value__227.input
                                var t5041 *ref_int_x = value__227.index
                                var t5042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5041)
                                var t5043 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5040, t5042)
                                var t5044 bool = t5043 == 69
                                jp4993 = t5044
                            }
                        } else {
                            jp4993 = false
                        }
                        if jp4993 {
                            var t4994 *ref_int_x = value__227.index
                            var t4995 *ref_int_x = value__227.index
                            var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4995)
                            var t4997 int = t4996 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4994, t4997)
                            var t5011 *ref_int_x = value__227.index
                            var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5011)
                            var t5013 string = value__227.input
                            var t5014 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5013)
                            var t5015 bool = t5012 < t5014
                            var jp5005 bool
                            if t5015 {
                                var t5018 string = value__227.input
                                var t5019 *ref_int_x = value__227.index
                                var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5019)
                                var t5021 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5018, t5020)
                                var t5022 bool = t5021 == 43
                                if t5022 {
                                    jp5005 = true
                                } else {
                                    var t5023 string = value__227.input
                                    var t5024 *ref_int_x = value__227.index
                                    var t5025 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5024)
                                    var t5026 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5023, t5025)
                                    var t5027 bool = t5026 == 45
                                    jp5005 = t5027
                                }
                            } else {
                                jp5005 = false
                            }
                            if jp5005 {
                                var t5006 *ref_int_x = value__227.index
                                var t5007 *ref_int_x = value__227.index
                                var t5008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5007)
                                var t5009 int = t5008 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5006, t5009)
                            } else {}
                            var t5000 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t5001 bool = !t5000
                            if t5001 {
                                var t5002 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t5003 Result__string__string = Result__string__string{
                                    _tag: 1,
                                    _v1_0: t5002,
                                }
                                return t5003
                            } else {
                                var t4987 string = value__227.input
                                var t4988 *ref_int_x = value__227.index
                                var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                                var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                                var t4991 Result__string__string = Result__string__string{
                                    _tag: 0,
                                    _v0_0: t4990,
                                }
                                return t4991
                            }
                        } else {
                            var t4987 string = value__227.input
                            var t4988 *ref_int_x = value__227.index
                            var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                            var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                            var t4991 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t4990,
                            }
                            return t4991
                        }
                    }
                } else {
                    var t5028 *ref_int_x = value__227.index
                    var t5029 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5028)
                    var t5030 string = value__227.input
                    var t5031 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5030)
                    var t5032 bool = t5029 < t5031
                    var jp4993 bool
                    if t5032 {
                        var t5035 string = value__227.input
                        var t5036 *ref_int_x = value__227.index
                        var t5037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5036)
                        var t5038 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5035, t5037)
                        var t5039 bool = t5038 == 101
                        if t5039 {
                            jp4993 = true
                        } else {
                            var t5040 string = value__227.input
                            var t5041 *ref_int_x = value__227.index
                            var t5042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5041)
                            var t5043 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5040, t5042)
                            var t5044 bool = t5043 == 69
                            jp4993 = t5044
                        }
                    } else {
                        jp4993 = false
                    }
                    if jp4993 {
                        var t4994 *ref_int_x = value__227.index
                        var t4995 *ref_int_x = value__227.index
                        var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4995)
                        var t4997 int = t4996 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t4994, t4997)
                        var t5011 *ref_int_x = value__227.index
                        var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5011)
                        var t5013 string = value__227.input
                        var t5014 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5013)
                        var t5015 bool = t5012 < t5014
                        var jp5005 bool
                        if t5015 {
                            var t5018 string = value__227.input
                            var t5019 *ref_int_x = value__227.index
                            var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5019)
                            var t5021 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5018, t5020)
                            var t5022 bool = t5021 == 43
                            if t5022 {
                                jp5005 = true
                            } else {
                                var t5023 string = value__227.input
                                var t5024 *ref_int_x = value__227.index
                                var t5025 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5024)
                                var t5026 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5023, t5025)
                                var t5027 bool = t5026 == 45
                                jp5005 = t5027
                            }
                        } else {
                            jp5005 = false
                        }
                        if jp5005 {
                            var t5006 *ref_int_x = value__227.index
                            var t5007 *ref_int_x = value__227.index
                            var t5008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5007)
                            var t5009 int = t5008 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5006, t5009)
                        } else {}
                        var t5000 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t5001 bool = !t5000
                        if t5001 {
                            var t5002 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t5003 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5002,
                            }
                            return t5003
                        } else {
                            var t4987 string = value__227.input
                            var t4988 *ref_int_x = value__227.index
                            var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                            var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                            var t4991 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t4990,
                            }
                            return t4991
                        }
                    } else {
                        var t4987 string = value__227.input
                        var t4988 *ref_int_x = value__227.index
                        var t4989 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t4988)
                        var t4990 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4987, start__228, t4989)
                        var t4991 Result__string__string = Result__string__string{
                            _tag: 0,
                            _v0_0: t4990,
                        }
                        return t4991
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5134 *ref_int_x = value__230.index
    var t5135 int
    var inline10341 int = ref_get__Ref_3int(t5134)
    t5135 = inline10341
    var t5136 int
    var inline10339 int = _goml_runtime_core_string_len(expected__231)
    t5136 = inline10339
    var t5137 int = t5135 + t5136
    var t5138 string = value__230.input
    var t5139 int
    var inline10337 int = _goml_runtime_core_string_len(t5138)
    t5139 = inline10337
    var t5140 bool = t5137 <= t5139
    var jp5125 bool
    if t5140 {
        var t5141 string = value__230.input
        var t5142 *ref_int_x = value__230.index
        var t5143 int
        var inline10321 int = ref_get__Ref_3int(t5142)
        t5143 = inline10321
        var t5144 *ref_int_x = value__230.index
        var t5145 int
        var inline10319 int = ref_get__Ref_3int(t5144)
        t5145 = inline10319
        var t5146 int
        var inline10317 int = _goml_runtime_core_string_len(expected__231)
        t5146 = inline10317
        var t5147 int = t5145 + t5146
        var t5148 string
        var inline10315 string = string_byte_slice(t5141, t5143, t5147)
        t5148 = inline10315
        var t5149 bool = t5148 == expected__231
        jp5125 = t5149
    } else {
        jp5125 = false
    }
    if jp5125 {
        var t5126 *ref_int_x = value__230.index
        var t5127 *ref_int_x = value__230.index
        var t5128 int
        var inline10327 int = ref_get__Ref_3int(t5127)
        t5128 = inline10327
        var t5129 int
        var inline10325 int = _goml_runtime_core_string_len(expected__231)
        t5129 = inline10325
        var t5130 int = t5128 + t5129
        ref_set__Ref_3int(t5126, t5130)
        var t5131 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: result__232,
        }
        return t5131
    } else {
        var t5132 string
        var inline10329 string = "invalid literal"
        var inline10330 string = "" + inline10329
        var inline10331 string = inline10330 + " at byte "
        var inline10332 *ref_int_x = value__230.index
        var inline10333 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10332)
        var inline10334 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10333)
        var inline10335 string = inline10331 + inline10334
        t5132 = inline10335
        var t5133 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5132,
        }
        return t5133
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5152 *ref_int_x = value__233.index
    var t5153 *ref_int_x = value__233.index
    var t5154 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5153)
    var t5155 int = t5154 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5152, t5155)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var t5156 [0]_goml_m_std_p_json_p_Value = [0]_goml_m_std_p_json_p_Value{}
    var result__234 *_goml_vec__goml_m_std_p_json_p_Value = func(values [0]_goml_m_std_p_json_p_Value) *_goml_vec__goml_m_std_p_json_p_Value {
        return &_goml_vec__goml_m_std_p_json_p_Value{
            items: values[0:len(values)],
        }
    }(t5156)
    var t5211 *ref_int_x = value__233.index
    var t5212 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5211)
    var t5213 string = value__233.input
    var t5214 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5213)
    var t5215 bool = t5212 < t5214
    var jp5204 bool
    if t5215 {
        var t5216 string = value__233.input
        var t5217 *ref_int_x = value__233.index
        var t5218 int
        var inline10345 int = ref_get__Ref_3int(t5217)
        t5218 = inline10345
        var t5219 uint8
        var inline10343 uint8 = _goml_runtime_core_string_byte_get(t5216, t5218)
        t5219 = inline10343
        var t5220 bool = t5219 == 93
        jp5204 = t5220
    } else {
        jp5204 = false
    }
    if jp5204 {
        var t5205 *ref_int_x = value__233.index
        var t5206 *ref_int_x = value__233.index
        var t5207 int
        var inline10349 int = ref_get__Ref_3int(t5206)
        t5207 = inline10349
        var t5208 int = t5207 + 1
        ref_set__Ref_3int(t5205, t5208)
        var t5209 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: result__234,
        }
        var t5210 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: t5209,
        }
        return t5210
    } else {
        Loop_loop5161:
        for {
            var t5162 *ref_int_x = value__233.index
            var t5163 int
            var inline10391 int = ref_get__Ref_3int(t5162)
            t5163 = inline10391
            var t5164 string = value__233.input
            var t5165 int
            var inline10389 int = _goml_runtime_core_string_len(t5164)
            t5165 = inline10389
            var t5166 bool = t5163 < t5165
            if t5166 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5168 _goml_m_std_p_json_p_Value
                switch mtmp797._tag {
                case 0:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797._v0_0
                    jp5168 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(result__234, jp5168)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5170 *ref_int_x = value__233.index
                    var t5171 int
                    var inline10385 int = ref_get__Ref_3int(t5170)
                    t5171 = inline10385
                    var t5172 string = value__233.input
                    var t5173 int
                    var inline10383 int = _goml_runtime_core_string_len(t5172)
                    t5173 = inline10383
                    var t5174 bool = t5171 >= t5173
                    if t5174 {
                        var t5175 string
                        var inline10351 string = "unterminated array"
                        var inline10352 string = "" + inline10351
                        var inline10353 string = inline10352 + " at byte "
                        var inline10354 *ref_int_x = value__233.index
                        var inline10355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10354)
                        var inline10356 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10355)
                        var inline10357 string = inline10353 + inline10356
                        t5175 = inline10357
                        var t5176 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                            _tag: 1,
                            _v1_0: t5175,
                        }
                        return t5176
                    } else {
                        var t5178 string = value__233.input
                        var t5179 *ref_int_x = value__233.index
                        var t5180 int
                        var inline10381 int = ref_get__Ref_3int(t5179)
                        t5180 = inline10381
                        var t5181 uint8
                        var inline10379 uint8 = _goml_runtime_core_string_byte_get(t5178, t5180)
                        t5181 = inline10379
                        var t5182 bool = t5181 == 93
                        if t5182 {
                            var t5183 *ref_int_x = value__233.index
                            var t5184 *ref_int_x = value__233.index
                            var t5185 int
                            var inline10361 int = ref_get__Ref_3int(t5184)
                            t5185 = inline10361
                            var t5186 int = t5185 + 1
                            ref_set__Ref_3int(t5183, t5186)
                            var t5187 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: result__234,
                            }
                            var t5188 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                _tag: 0,
                                _v0_0: t5187,
                            }
                            return t5188
                        } else {
                            var t5190 string = value__233.input
                            var t5191 *ref_int_x = value__233.index
                            var t5192 int
                            var inline10377 int = ref_get__Ref_3int(t5191)
                            t5192 = inline10377
                            var t5193 uint8
                            var inline10375 uint8 = _goml_runtime_core_string_byte_get(t5190, t5192)
                            t5193 = inline10375
                            var t5194 bool = t5193 == 44
                            if t5194 {
                                var t5195 *ref_int_x = value__233.index
                                var t5196 *ref_int_x = value__233.index
                                var t5197 int
                                var inline10365 int = ref_get__Ref_3int(t5196)
                                t5197 = inline10365
                                var t5198 int = t5197 + 1
                                ref_set__Ref_3int(t5195, t5198)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5200 string
                                var inline10367 string = "expected array separator"
                                var inline10368 string = "" + inline10367
                                var inline10369 string = inline10368 + " at byte "
                                var inline10370 *ref_int_x = value__233.index
                                var inline10371 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10370)
                                var inline10372 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10371)
                                var inline10373 string = inline10369 + inline10372
                                t5200 = inline10373
                                var t5201 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                    _tag: 1,
                                    _v1_0: t5200,
                                }
                                return t5201
                            }
                        }
                    }
                case 1:
                    var x799 string = mtmp797._v1_0
                    var t5202 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: x799,
                    }
                    return t5202
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5161
            }
        }
        var t5159 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5160 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5159,
        }
        return t5160
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5223 *ref_int_x = value__236.index
    var t5224 *ref_int_x = value__236.index
    var t5225 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5224)
    var t5226 int = t5225 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5223, t5226)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var t5227 [0]Tuple2_6string_26_goml_m_std_p_json_p_Value = [0]Tuple2_6string_26_goml_m_std_p_json_p_Value{}
    var result__237 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = func(values [0]Tuple2_6string_26_goml_m_std_p_json_p_Value) *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
        return &_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value{
            items: values[0:len(values)],
        }
    }(t5227)
    var t5306 *ref_int_x = value__236.index
    var t5307 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5306)
    var t5308 string = value__236.input
    var t5309 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5308)
    var t5310 bool = t5307 < t5309
    var jp5299 bool
    if t5310 {
        var t5311 string = value__236.input
        var t5312 *ref_int_x = value__236.index
        var t5313 int
        var inline10395 int = ref_get__Ref_3int(t5312)
        t5313 = inline10395
        var t5314 uint8
        var inline10393 uint8 = _goml_runtime_core_string_byte_get(t5311, t5313)
        t5314 = inline10393
        var t5315 bool = t5314 == 125
        jp5299 = t5315
    } else {
        jp5299 = false
    }
    if jp5299 {
        var t5300 *ref_int_x = value__236.index
        var t5301 *ref_int_x = value__236.index
        var t5302 int
        var inline10399 int = ref_get__Ref_3int(t5301)
        t5302 = inline10399
        var t5303 int = t5302 + 1
        ref_set__Ref_3int(t5300, t5303)
        var t5304 _goml_m_std_p_json_p_Value = Object{
            _0: result__237,
        }
        var t5305 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: t5304,
        }
        return t5305
    } else {
        Loop_loop5232:
        for {
            var t5233 *ref_int_x = value__236.index
            var t5234 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5233)
            var t5235 string = value__236.input
            var t5236 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5235)
            var t5237 bool = t5234 < t5236
            if t5237 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5239 string
                switch mtmp809._tag {
                case 0:
                    var x810 string = mtmp809._v0_0
                    jp5239 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5287 *ref_int_x = value__236.index
                    var t5288 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5287)
                    var t5289 string = value__236.input
                    var t5290 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5289)
                    var t5291 bool = t5288 >= t5290
                    var jp5279 bool
                    if t5291 {
                        jp5279 = true
                    } else {
                        var t5292 string = value__236.input
                        var t5293 *ref_int_x = value__236.index
                        var t5294 int
                        var inline10403 int = ref_get__Ref_3int(t5293)
                        t5294 = inline10403
                        var t5295 uint8
                        var inline10401 uint8 = _goml_runtime_core_string_byte_get(t5292, t5294)
                        t5295 = inline10401
                        var t5296 bool = t5295 != 58
                        jp5279 = t5296
                    }
                    if jp5279 {
                        var t5280 string
                        var inline10405 string = "expected object colon"
                        var inline10406 string = "" + inline10405
                        var inline10407 string = inline10406 + " at byte "
                        var inline10408 *ref_int_x = value__236.index
                        var inline10409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10408)
                        var inline10410 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10409)
                        var inline10411 string = inline10407 + inline10410
                        t5280 = inline10411
                        var t5281 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                            _tag: 1,
                            _v1_0: t5280,
                        }
                        return t5281
                    } else {
                        var t5282 *ref_int_x = value__236.index
                        var t5283 *ref_int_x = value__236.index
                        var t5284 int
                        var inline10415 int = ref_get__Ref_3int(t5283)
                        t5284 = inline10415
                        var t5285 int = t5284 + 1
                        ref_set__Ref_3int(t5282, t5285)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5242 _goml_m_std_p_json_p_Value
                        switch mtmp815._tag {
                        case 0:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815._v0_0
                            jp5242 = x816
                            var t5243 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5239,
                                _1: jp5242,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__237, t5243)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5245 *ref_int_x = value__236.index
                            var t5246 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5245)
                            var t5247 string = value__236.input
                            var t5248 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5247)
                            var t5249 bool = t5246 >= t5248
                            if t5249 {
                                var t5250 string
                                var inline10417 string = "unterminated object"
                                var inline10418 string = "" + inline10417
                                var inline10419 string = inline10418 + " at byte "
                                var inline10420 *ref_int_x = value__236.index
                                var inline10421 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10420)
                                var inline10422 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10421)
                                var inline10423 string = inline10419 + inline10422
                                t5250 = inline10423
                                var t5251 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                    _tag: 1,
                                    _v1_0: t5250,
                                }
                                return t5251
                            } else {
                                var t5253 string = value__236.input
                                var t5254 *ref_int_x = value__236.index
                                var t5255 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5254)
                                var t5256 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5253, t5255)
                                var t5257 bool = t5256 == 125
                                if t5257 {
                                    var t5258 *ref_int_x = value__236.index
                                    var t5259 *ref_int_x = value__236.index
                                    var t5260 int
                                    var inline10427 int = ref_get__Ref_3int(t5259)
                                    t5260 = inline10427
                                    var t5261 int = t5260 + 1
                                    ref_set__Ref_3int(t5258, t5261)
                                    var t5262 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__237,
                                    }
                                    var t5263 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                        _tag: 0,
                                        _v0_0: t5262,
                                    }
                                    return t5263
                                } else {
                                    var t5265 string = value__236.input
                                    var t5266 *ref_int_x = value__236.index
                                    var t5267 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5266)
                                    var t5268 uint8
                                    var inline10441 uint8 = _goml_runtime_core_string_byte_get(t5265, t5267)
                                    t5268 = inline10441
                                    var t5269 bool = t5268 == 44
                                    if t5269 {
                                        var t5270 *ref_int_x = value__236.index
                                        var t5271 *ref_int_x = value__236.index
                                        var t5272 int
                                        var inline10431 int = ref_get__Ref_3int(t5271)
                                        t5272 = inline10431
                                        var t5273 int = t5272 + 1
                                        ref_set__Ref_3int(t5270, t5273)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5275 string
                                        var inline10433 string = "expected object separator"
                                        var inline10434 string = "" + inline10433
                                        var inline10435 string = inline10434 + " at byte "
                                        var inline10436 *ref_int_x = value__236.index
                                        var inline10437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10436)
                                        var inline10438 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10437)
                                        var inline10439 string = inline10435 + inline10438
                                        t5275 = inline10439
                                        var t5276 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                            _tag: 1,
                                            _v1_0: t5275,
                                        }
                                        return t5276
                                    }
                                }
                            }
                        case 1:
                            var x817 string = mtmp815._v1_0
                            var t5277 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                _tag: 1,
                                _v1_0: x817,
                            }
                            return t5277
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x811 string = mtmp809._v1_0
                    var t5297 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: x811,
                    }
                    return t5297
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5232
            }
        }
        var t5230 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5231 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5230,
        }
        return t5231
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5320 *ref_int_x = value__240.index
    var t5321 int
    var inline10479 int = ref_get__Ref_3int(t5320)
    t5321 = inline10479
    var t5322 string = value__240.input
    var t5323 int
    var inline10477 int = _goml_runtime_core_string_len(t5322)
    t5323 = inline10477
    var t5324 bool = t5321 >= t5323
    if t5324 {
        var t5325 string
        var inline10443 string = "expected JSON value"
        var inline10444 string = "" + inline10443
        var inline10445 string = inline10444 + " at byte "
        var inline10446 *ref_int_x = value__240.index
        var inline10447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10446)
        var inline10448 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10447)
        var inline10449 string = inline10445 + inline10448
        t5325 = inline10449
        var t5326 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5325,
        }
        return t5326
    } else {
        var t5327 string = value__240.input
        var t5328 *ref_int_x = value__240.index
        var t5329 int
        var inline10475 int = ref_get__Ref_3int(t5328)
        t5329 = inline10475
        var mtmp824 uint8
        var inline10473 uint8 = _goml_runtime_core_string_byte_get(t5327, t5329)
        mtmp824 = inline10473
        switch mtmp824 {
        case 123:
            var t5332 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5332
        case 91:
            var t5333 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5333
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825._tag {
            case 0:
                var x826 string = mtmp825._v0_0
                var t5336 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5337 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 0,
                    _v0_0: t5336,
                }
                return t5337
            case 1:
                var x827 string = mtmp825._v1_0
                var t5338 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 1,
                    _v1_0: x827,
                }
                return t5338
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5339 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5340 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5339)
            return t5340
        case 102:
            var t5341 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5342 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5341)
            return t5342
        case 110:
            var t5343 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5343
        default:
            var t5351 bool = mtmp824 == 45
            var jp5347 bool
            if t5351 {
                jp5347 = true
            } else {
                var inline10451 bool = mtmp824 >= 48
                if inline10451 {
                    var inline10452 bool = mtmp824 <= 57
                    jp5347 = inline10452
                } else {
                    jp5347 = false
                }
            }
            if jp5347 {
                var inline10454 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10456 string
                switch inline10454._tag {
                case 0:
                    var inline10459 string = inline10454._v0_0
                    inline10456 = inline10459
                    var inline10457 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10456,
                    }
                    var inline10458 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 0,
                        _v0_0: inline10457,
                    }
                    return inline10458
                case 1:
                    var inline10461 string = inline10454._v1_0
                    var inline10463 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: inline10461,
                    }
                    return inline10463
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5349 string
                var inline10465 string = "unexpected JSON token"
                var inline10466 string = "" + inline10465
                var inline10467 string = inline10466 + " at byte "
                var inline10468 *ref_int_x = value__240.index
                var inline10469 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10468)
                var inline10470 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10469)
                var inline10471 string = inline10467 + inline10470
                t5349 = inline10471
                var t5350 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 1,
                    _v1_0: t5349,
                }
                return t5350
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10493 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var inline10494 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10493,
    }
    parser__245 = inline10494
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5356 _goml_m_std_p_json_p_Value
    switch mtmp828._tag {
    case 0:
        var x829 _goml_m_std_p_json_p_Value = mtmp828._v0_0
        jp5356 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5359 *ref_int_x = parser__245.index
        var t5360 int
        var inline10491 int = ref_get__Ref_3int(t5359)
        t5360 = inline10491
        var t5361 int
        var inline10489 int = _goml_runtime_core_string_len(input__244)
        t5361 = inline10489
        var t5362 bool = t5360 == t5361
        if t5362 {
            var t5363 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                _tag: 0,
                _v0_0: jp5356,
            }
            return t5363
        } else {
            var t5364 string
            var inline10481 string = "trailing JSON data"
            var inline10482 string = "" + inline10481
            var inline10483 string = inline10482 + " at byte "
            var inline10484 *ref_int_x = parser__245.index
            var inline10485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline10484)
            var inline10486 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline10485)
            var inline10487 string = inline10483 + inline10486
            t5364 = inline10487
            var t5365 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                _tag: 1,
                _v1_0: t5364,
            }
            return t5365
        }
    case 1:
        var x830 string = mtmp828._v1_0
        var t5366 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: x830,
        }
        return t5366
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10527 rune = 34
    var inline10528 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10527)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10528)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10525 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10525
    Loop_loop5380:
    for {
        var t5381 bool = for_index833 < for_limit834
        if t5381 {
            var for_item835 int = for_index833
            var t5382 int = for_index833 + 1
            for_index833 = t5382
            var byte__252 uint8
            var inline10513 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10513
            var t5435 bool = byte__252 == 34
            var jp5433 bool
            if t5435 {
                jp5433 = true
            } else {
                var t5436 bool = byte__252 == 92
                jp5433 = t5436
            }
            var jp5430 bool
            if jp5433 {
                jp5430 = true
            } else {
                var t5434 bool = byte__252 == 8
                jp5430 = t5434
            }
            var jp5427 bool
            if jp5430 {
                jp5427 = true
            } else {
                var t5431 bool = byte__252 == 9
                jp5427 = t5431
            }
            var jp5424 bool
            if jp5427 {
                jp5424 = true
            } else {
                var t5428 bool = byte__252 == 10
                jp5424 = t5428
            }
            var jp5421 bool
            if jp5424 {
                jp5421 = true
            } else {
                var t5425 bool = byte__252 == 12
                jp5421 = t5425
            }
            var jp5418 bool
            if jp5421 {
                jp5418 = true
            } else {
                var t5422 bool = byte__252 == 13
                jp5418 = t5422
            }
            var jp5385 bool
            if jp5418 {
                jp5385 = true
            } else {
                var t5419 bool = byte__252 < 32
                jp5385 = t5419
            }
            if jp5385 {
                var t5414 bool = start__250 < for_item835
                if t5414 {
                    var t5415 string
                    var inline10499 string = string_byte_slice(value__249, start__250, for_item835)
                    t5415 = inline10499
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5415)
                } else {}
                var t5389 bool = byte__252 == 34
                if t5389 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5392 bool = byte__252 == 92
                    if t5392 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5395 bool = byte__252 == 8
                        if t5395 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5398 bool = byte__252 == 9
                            if t5398 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5401 bool = byte__252 == 10
                                if t5401 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5404 bool = byte__252 == 12
                                    if t5404 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5407 bool = byte__252 == 13
                                        if t5407 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5409 uint8 = byte__252 / 16
                                            var t5410 rune
                                            var inline10510 int = int(uint8(t5409))
                                            var inline10511 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10510)
                                            t5410 = inline10511
                                            var inline10507 string = _goml_m_inherent_i_char_i_char_i_to__string(t5410)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10507)
                                            var t5411_rhs uint8 = 16
                                            var t5411 uint8 = byte__252 % t5411_rhs
                                            var t5412 rune
                                            var inline10504 int = int(uint8(t5411))
                                            var inline10505 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10504)
                                            t5412 = inline10505
                                            var inline10501 string = _goml_m_inherent_i_char_i_char_i_to__string(t5412)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10501)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5388 int = for_item835 + 1
                start__250 = t5388
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5380
        }
    }
    var t5375 int
    var inline10523 int = _goml_runtime_core_string_len(value__249)
    t5375 = inline10523
    var t5376 bool = start__250 < t5375
    if t5376 {
        var t5377 int
        var inline10517 int = _goml_runtime_core_string_len(value__249)
        t5377 = inline10517
        var t5378 string
        var inline10515 string = string_byte_slice(value__249, start__250, t5377)
        t5378 = inline10515
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5378)
    } else {}
    var inline10519 rune = 34
    var inline10520 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10519)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10520)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10543 rune = 123
        var inline10544 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10543)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10544)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5441:
        for {
            var t5442 bool = for_index852 < for_limit851
            if t5442 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5443 int = for_index852 + 1
                for_index852 = t5443
                var t5449 bool = index__256 > 0
                if t5449 {
                    var inline10531 rune = 44
                    var inline10532 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10531)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10532)
                } else {}
                var t5445 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5445)
                var inline10535 rune = 58
                var inline10536 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10535)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10536)
                var t5446 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5446)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5447 int = compound_old859 + compound_value860
                index__256 = t5447
                continue
            } else {
                break Loop_loop5441
            }
        }
        var inline10539 rune = 125
        var inline10540 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10539)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10540)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10555 rune = 91
        var inline10556 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10555)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10556)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5453:
        for {
            var t5454 bool = for_index866 < for_limit865
            if t5454 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5455 int = for_index866 + 1
                for_index866 = t5455
                var t5459 bool = index__259 > 0
                if t5459 {
                    var inline10547 rune = 44
                    var inline10548 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10547)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10548)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5457 int = compound_old871 + compound_value872
                index__259 = t5457
                continue
            } else {
                break Loop_loop5453
            }
        }
        var inline10551 rune = 93
        var inline10552 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10551)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10552)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_String:
        var x846 string = value__254.(_goml_m_std_p_json_p_Value_String)._0
        _goml_m_std_p_json_p_write__json__string(builder__253, x846)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Number:
        var x847 string = value__254.(_goml_m_std_p_json_p_Value_Number)._0
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, x847)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Bool:
        var x848 bool = value__254.(_goml_m_std_p_json_p_Value_Bool)._0
        var jp5464 string
        if x848 {
            jp5464 = "true"
        } else {
            jp5464 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5464)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__264 _goml_m_std_p_json_p_Value) string {
    var builder__265 _goml_m_std_p_text_p_StringBuilder
    var inline10564 [0]uint8 = [0]uint8{}
    var inline10565 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(inline10564)
    var inline10566 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10565,
    }
    builder__265 = inline10566
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10559 *_goml_vec_uint8 = builder__265.values
    var inline10560 Tuple2_4bool_6string = string_from_utf8(inline10559)
    var inline10561 string = inline10560._1
    return inline10561
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5475:
        for {
            var t5476 bool = for_index883 < for_limit882
            if t5476 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5477 int = for_index883 + 1
                for_index883 = t5477
                var t5479 string = for_item884._0
                var t5480 bool = t5479 == name__267
                if t5480 {
                    var t5481 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5482 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value{
                        _tag: 1,
                        _v1_0: t5481,
                    }
                    return t5482
                } else {
                    continue
                }
            } else {
                break Loop_loop5475
            }
        }
        return _goml_m_Option____std_p_json_p_Value{
            _tag: 0,
        }
    default:
        return _goml_m_Option____std_p_json_p_Value{
            _tag: 0,
        }
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__isize {
    var t5492 int
    var inline10577 int = _goml_runtime_core_string_len(value__272)
    t5492 = inline10577
    var t5493 bool = t5492 == 0
    if t5493 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var t5494 uint8
        var inline10574 int = 0
        var inline10575 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10574)
        t5494 = inline10575
        var negative__273 bool = t5494 == 45
        var jp5496 int
        if negative__273 {
            jp5496 = 1
        } else {
            jp5496 = 0
        }
        var index__274 int = jp5496
        var result__275 int = 0
        var t5517 int
        var inline10572 int = _goml_runtime_core_string_len(value__272)
        t5517 = inline10572
        var t5518 bool = index__274 == t5517
        if t5518 {
            return Option__isize{
                _tag: 0,
            }
        } else {
            Loop_loop5503:
            for {
                var t5504 int
                var inline10570 int = _goml_runtime_core_string_len(value__272)
                t5504 = inline10570
                var t5505 bool = index__274 < t5504
                if t5505 {
                    var byte__276 uint8
                    var inline10568 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10568
                    var t5515 bool = byte__276 < 48
                    var jp5510 bool
                    if t5515 {
                        jp5510 = true
                    } else {
                        var t5516 bool = byte__276 > 57
                        jp5510 = t5516
                    }
                    if jp5510 {
                        return Option__isize{
                            _tag: 0,
                        }
                    } else {
                        var t5511 int = result__275 * 10
                        var t5512 uint8 = byte__276 - 48
                        var t5513 int = int(uint8(t5512))
                        var t5514 int = t5511 + t5513
                        result__275 = t5514
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5507 int = compound_old895 + compound_value896
                        index__274 = t5507
                        continue
                    }
                } else {
                    break Loop_loop5503
                }
            }
            var jp5500 int
            if negative__273 {
                var t5502 int = 0 - result__275
                jp5500 = t5502
            } else {
                jp5500 = result__275
            }
            var t5501 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: jp5500,
            }
            return t5501
        }
    }
}

func main0() struct{} {
    var mtmp411 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6295 _goml_m_std_p_json_p_Value
    switch mtmp411._tag {
    case 0:
        var x412 _goml_m_std_p_json_p_Value = mtmp411._v0_0
        jp6295 = x412
        var mtmp415 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "name")
        switch mtmp415._tag {
        case 0:
            var inline11015 string = "missing name"
            var inline11016 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11015)
            _goml_runtime_core_string_println(inline11016)
            var mtmp420 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "version")
            switch mtmp420._tag {
            case 0:
                var inline11030 string = "missing version"
                var inline11031 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11030)
                _goml_runtime_core_string_println(inline11031)
            case 1:
                var x421 _goml_m_std_p_json_p_Value = mtmp420._v1_0
                var mtmp422 Option__isize
                switch x421.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline11041 string = x421.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline11043 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline11041)
                    mtmp422 = inline11043
                default:
                    mtmp422 = Option__isize{
                        _tag: 0,
                    }
                }
                switch mtmp422._tag {
                case 0:
                    var inline11034 string = "invalid version"
                    var inline11035 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11034)
                    _goml_runtime_core_string_println(inline11035)
                case 1:
                    var x423 int = mtmp422._v1_0
                    var inline11038 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x423)
                    _goml_runtime_core_string_println(inline11038)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp425 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "stable")
            switch mtmp425._tag {
            case 0:
                var inline11045 string = "missing stable"
                var inline11046 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11045)
                _goml_runtime_core_string_println(inline11046)
                var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                println__T_string(t6299)
                return struct{}{}
            case 1:
                var x426 _goml_m_std_p_json_p_Value = mtmp425._v1_0
                var commute_field11595 bool
                switch x426.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline11056 bool = x426.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11595 = inline11056
                    var inline11053 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11595)
                    _goml_runtime_core_string_println(inline11053)
                    var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                    println__T_string(t6299)
                    return struct{}{}
                default:
                    var inline11049 string = "invalid stable"
                    var inline11050 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11049)
                    _goml_runtime_core_string_println(inline11050)
                    var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                    println__T_string(t6299)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case 1:
            var x416 _goml_m_std_p_json_p_Value = mtmp415._v1_0
            var commute_field11601 string
            switch x416.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline11026 string = x416.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11601 = inline11026
                var inline11023 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11601)
                _goml_runtime_core_string_println(inline11023)
                var mtmp420 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "version")
                switch mtmp420._tag {
                case 0:
                    var inline11030 string = "missing version"
                    var inline11031 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11030)
                    _goml_runtime_core_string_println(inline11031)
                case 1:
                    var x421 _goml_m_std_p_json_p_Value = mtmp420._v1_0
                    var mtmp422 Option__isize
                    switch x421.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11041 string = x421.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11043 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline11041)
                        mtmp422 = inline11043
                    default:
                        mtmp422 = Option__isize{
                            _tag: 0,
                        }
                    }
                    switch mtmp422._tag {
                    case 0:
                        var inline11034 string = "invalid version"
                        var inline11035 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11034)
                        _goml_runtime_core_string_println(inline11035)
                    case 1:
                        var x423 int = mtmp422._v1_0
                        var inline11038 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x423)
                        _goml_runtime_core_string_println(inline11038)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp425 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "stable")
                switch mtmp425._tag {
                case 0:
                    var inline11045 string = "missing stable"
                    var inline11046 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11045)
                    _goml_runtime_core_string_println(inline11046)
                    var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                    println__T_string(t6299)
                    return struct{}{}
                case 1:
                    var x426 _goml_m_std_p_json_p_Value = mtmp425._v1_0
                    var commute_field11595 bool
                    switch x426.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11056 bool = x426.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11595 = inline11056
                        var inline11053 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11595)
                        _goml_runtime_core_string_println(inline11053)
                        var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                        println__T_string(t6299)
                        return struct{}{}
                    default:
                        var inline11049 string = "invalid stable"
                        var inline11050 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11049)
                        _goml_runtime_core_string_println(inline11050)
                        var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                        println__T_string(t6299)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline11019 string = "invalid name"
                var inline11020 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11019)
                _goml_runtime_core_string_println(inline11020)
                var mtmp420 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "version")
                switch mtmp420._tag {
                case 0:
                    var inline11030 string = "missing version"
                    var inline11031 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11030)
                    _goml_runtime_core_string_println(inline11031)
                case 1:
                    var x421 _goml_m_std_p_json_p_Value = mtmp420._v1_0
                    var mtmp422 Option__isize
                    switch x421.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11041 string = x421.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11043 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline11041)
                        mtmp422 = inline11043
                    default:
                        mtmp422 = Option__isize{
                            _tag: 0,
                        }
                    }
                    switch mtmp422._tag {
                    case 0:
                        var inline11034 string = "invalid version"
                        var inline11035 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11034)
                        _goml_runtime_core_string_println(inline11035)
                    case 1:
                        var x423 int = mtmp422._v1_0
                        var inline11038 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x423)
                        _goml_runtime_core_string_println(inline11038)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp425 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6295, "stable")
                switch mtmp425._tag {
                case 0:
                    var inline11045 string = "missing stable"
                    var inline11046 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11045)
                    _goml_runtime_core_string_println(inline11046)
                    var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                    println__T_string(t6299)
                    return struct{}{}
                case 1:
                    var x426 _goml_m_std_p_json_p_Value = mtmp425._v1_0
                    var commute_field11595 bool
                    switch x426.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11056 bool = x426.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11595 = inline11056
                        var inline11053 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11595)
                        _goml_runtime_core_string_println(inline11053)
                        var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                        println__T_string(t6299)
                        return struct{}{}
                    default:
                        var inline11049 string = "invalid stable"
                        var inline11050 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11049)
                        _goml_runtime_core_string_println(inline11050)
                        var t6299 string = _goml_m_std_p_json_p_encode(jp6295)
                        println__T_string(t6299)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x413 string = mtmp411._v1_0
        var inline11012 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x413)
        _goml_runtime_core_string_println(inline11012)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6352:
    for {
        var t6353 int
        var inline11067 int = _goml_runtime_core_string_len(x12)
        t6353 = inline11067
        var t6354 bool = index__26 < t6353
        if t6354 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6356 int = compound_old17 + x16
                index__26 = t6356
                continue
            } else {
                var t6358 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6358
            }
        } else {
            break Loop_loop6352
        }
    }
    var t6351 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6351
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t6388 string = _goml_runtime_core_int_to_string(self__32)
    return t6388
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline11077 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline11078 bool = inline11077._0
    var inline11079 rune = inline11077._1
    if inline11078 {
        return inline11079
    } else {
        var inline11082 rune = _goml_runtime_core_string_get("", -1)
        return inline11082
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__431 int) *ref_int_x {
    var t6467 *ref_int_x = ref__Ref_3int(value__431)
    return t6467
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__432 *ref_int_x) int {
    var t6470 int = ref_get__Ref_3int(self__432)
    return t6470
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__34 rune) string {
    var inline11089 uint32 = uint32(rune(self__34))
    var inline11090 bool = utf8_valid_scalar(inline11089)
    if inline11090 {
        var inline11091 string = _goml_runtime_core_char_to_string(self__34)
        return inline11091
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t6529 int = _goml_runtime_core_string_len(self__36)
    return t6529
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t6532 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t6532
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline11389 bool = string_is_char_boundary(self__41, start__42)
    var inline11391 bool
    if inline11389 {
        var inline11394 bool = string_is_char_boundary(self__41, end__43)
        inline11391 = inline11394
    } else {
        inline11391 = false
    }
    if inline11391 {
        var inline11392 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline11392
    } else {
        var inline11393 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline11393
    }
}

func char_from_u32(value__2 uint32) Option__char {
    var inline11400 bool = utf8_valid_scalar(value__2)
    if inline11400 {
        var inline11401 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11402 rune = inline11401._1
        var inline11404 Option__char = Option__char{
            _tag: 1,
            _v1_0: inline11402,
        }
        return inline11404
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__258 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__259 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__258, elem__259)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6899 string
    t6899 = value__1
    _goml_runtime_core_string_println(t6899)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t7025 bool = index__6 < 0
    var jp7023 bool
    if t7025 {
        jp7023 = true
    } else {
        var t7026 bool = index__6 >= length__7
        jp7023 = t7026
    }
    if jp7023 {
        var inline11415 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11415
    } else {
        var t6910 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6910))
        var t6913 bool = first__8 < 128
        if t6913 {
            var inline11417 int = 1
            var inline11418 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11418._tag {
            case 0:
                var inline11419 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11419
            case 1:
                var inline11420 rune = inline11418._v1_0
                var inline11422 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11420,
                    _2: inline11417,
                }
                return inline11422
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6917 bool = first__8 < 194
            if t6917 {
                var inline11424 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11424
            } else {
                var t6921 bool = first__8 < 224
                if t6921 {
                    var t6934 int = length__7 - index__6
                    var t6935 bool = t6934 < 2
                    if t6935 {
                        var inline11426 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11426
                    } else {
                        var t6923 int = index__6 + 1
                        var t6924 uint8
                        var inline11440 uint8 = _goml_runtime_core_string_byte_get(value__5, t6923)
                        t6924 = inline11440
                        var second__9 uint32 = uint32(uint8(t6924))
                        var t6927 bool
                        var inline11437 bool = second__9 < 128
                        if inline11437 {
                            t6927 = true
                        } else {
                            var inline11438 bool = second__9 > 191
                            t6927 = inline11438
                        }
                        if t6927 {
                            var inline11428 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11428
                        } else {
                            var t6929_rhs uint32 = 31
                            var t6929 uint32 = first__8 & t6929_rhs
                            var t6930_rhs int = 6
                            var t6930 uint32 = t6929 << t6930_rhs
                            var t6931_rhs uint32 = 63
                            var t6931 uint32 = second__9 & t6931_rhs
                            var t6932 uint32 = t6930 | t6931
                            var inline11430 int = 2
                            var inline11431 Option__char = __goml_builtin_char_from_uint32(t6932)
                            switch inline11431._tag {
                            case 0:
                                var inline11432 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11432
                            case 1:
                                var inline11433 rune = inline11431._v1_0
                                var inline11435 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11433,
                                    _2: inline11430,
                                }
                                return inline11435
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6939 bool = first__8 < 240
                    if t6939 {
                        var t6972 int = length__7 - index__6
                        var t6973 bool = t6972 < 3
                        if t6973 {
                            var inline11442 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11442
                        } else {
                            var t6941 int = index__6 + 1
                            var t6942 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6941)
                            var second__10 uint32 = uint32(uint8(t6942))
                            var t6943 int = index__6 + 2
                            var t6944 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6943)
                            var third__11 uint32 = uint32(uint8(t6944))
                            var t6970 bool = utf8_invalid_continuation(second__10)
                            var jp6965 bool
                            if t6970 {
                                jp6965 = true
                            } else {
                                var inline11444 bool = third__11 < 128
                                if inline11444 {
                                    jp6965 = true
                                } else {
                                    var inline11445 bool = third__11 > 191
                                    jp6965 = inline11445
                                }
                            }
                            var jp6959 bool
                            if jp6965 {
                                jp6959 = true
                            } else {
                                var t6968 bool = first__8 == 224
                                if t6968 {
                                    var t6969 bool = second__10 < 160
                                    jp6959 = t6969
                                } else {
                                    jp6959 = false
                                }
                            }
                            var jp6948 bool
                            if jp6959 {
                                jp6948 = true
                            } else {
                                var t6962 bool = first__8 == 237
                                if t6962 {
                                    var t6963 bool = second__10 >= 160
                                    jp6948 = t6963
                                } else {
                                    jp6948 = false
                                }
                            }
                            if jp6948 {
                                var inline11447 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11447
                            } else {
                                var t6950_rhs uint32 = 15
                                var t6950 uint32 = first__8 & t6950_rhs
                                var t6951_rhs int = 12
                                var t6951 uint32 = t6950 << t6951_rhs
                                var t6952_rhs uint32 = 63
                                var t6952 uint32 = second__10 & t6952_rhs
                                var t6953_rhs int = 6
                                var t6953 uint32 = t6952 << t6953_rhs
                                var t6954 uint32 = t6951 | t6953
                                var t6955_rhs uint32 = 63
                                var t6955 uint32 = third__11 & t6955_rhs
                                var t6956 uint32 = t6954 | t6955
                                var inline11449 int = 3
                                var inline11450 Option__char = __goml_builtin_char_from_uint32(t6956)
                                switch inline11450._tag {
                                case 0:
                                    var inline11451 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11451
                                case 1:
                                    var inline11452 rune = inline11450._v1_0
                                    var inline11454 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11452,
                                        _2: inline11449,
                                    }
                                    return inline11454
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6977 bool = first__8 < 245
                        if t6977 {
                            var t7018 int = length__7 - index__6
                            var t7019 bool = t7018 < 4
                            if t7019 {
                                var t7020 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t7020
                            } else {
                                var t6979 int = index__6 + 1
                                var t6980 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6979)
                                var second__12 uint32 = uint32(uint8(t6980))
                                var t6981 int = index__6 + 2
                                var t6982 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6981)
                                var third__13 uint32 = uint32(uint8(t6982))
                                var t6983 int = index__6 + 3
                                var t6984 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6983)
                                var fourth__14 uint32 = uint32(uint8(t6984))
                                var t7016 bool = utf8_invalid_continuation(second__12)
                                var jp7014 bool
                                if t7016 {
                                    jp7014 = true
                                } else {
                                    var t7017 bool = utf8_invalid_continuation(third__13)
                                    jp7014 = t7017
                                }
                                var jp7008 bool
                                if jp7014 {
                                    jp7008 = true
                                } else {
                                    var t7015 bool = utf8_invalid_continuation(fourth__14)
                                    jp7008 = t7015
                                }
                                var jp7002 bool
                                if jp7008 {
                                    jp7002 = true
                                } else {
                                    var t7011 bool = first__8 == 240
                                    if t7011 {
                                        var t7012 bool = second__12 < 144
                                        jp7002 = t7012
                                    } else {
                                        jp7002 = false
                                    }
                                }
                                var jp6988 bool
                                if jp7002 {
                                    jp6988 = true
                                } else {
                                    var t7005 bool = first__8 == 244
                                    if t7005 {
                                        var t7006 bool = second__12 > 143
                                        jp6988 = t7006
                                    } else {
                                        jp6988 = false
                                    }
                                }
                                if jp6988 {
                                    var t6989 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6989
                                } else {
                                    var t6990_rhs uint32 = 7
                                    var t6990 uint32 = first__8 & t6990_rhs
                                    var t6991_rhs int = 18
                                    var t6991 uint32 = t6990 << t6991_rhs
                                    var t6992_rhs uint32 = 63
                                    var t6992 uint32 = second__12 & t6992_rhs
                                    var t6993_rhs int = 12
                                    var t6993 uint32 = t6992 << t6993_rhs
                                    var t6994 uint32 = t6991 | t6993
                                    var t6995_rhs uint32 = 63
                                    var t6995 uint32 = third__13 & t6995_rhs
                                    var t6996_rhs int = 6
                                    var t6996 uint32 = t6995 << t6996_rhs
                                    var t6997 uint32 = t6994 | t6996
                                    var t6998_rhs uint32 = 63
                                    var t6998 uint32 = fourth__14 & t6998_rhs
                                    var t6999 uint32 = t6997 | t6998
                                    var t7000 Tuple3_4bool_4char_3int = utf8_valid_decode(t6999, 4)
                                    return t7000
                                }
                            }
                        } else {
                            var t7021 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t7021
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t7046 uint32 = uint32(rune(value__29))
    var t7047 bool
    var inline11456 bool = t7046 <= 1114111
    if inline11456 {
        var inline11457 bool = t7046 >= 55296
        var inline11459 bool
        if inline11457 {
            var inline11461 bool = t7046 <= 57343
            inline11459 = inline11461
        } else {
            inline11459 = false
        }
        var inline11460 bool = !inline11459
        t7047 = inline11460
    } else {
        t7047 = false
    }
    if t7047 {
        var t7048 string = _goml_runtime_core_char_to_string(value__29)
        return t7048
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t7177 bool = string_is_char_boundary(value__21, start__22)
    var jp7174 bool
    if t7177 {
        var t7178 bool = string_is_char_boundary(value__21, end__23)
        jp7174 = t7178
    } else {
        jp7174 = false
    }
    if jp7174 {
        var t7175 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t7175
    } else {
        var t7176 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t7176
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t7185 bool
    var inline11489 bool = value__30 <= 1114111
    if inline11489 {
        var inline11490 bool = value__30 >= 55296
        var inline11492 bool
        if inline11490 {
            var inline11494 bool = value__30 <= 57343
            inline11492 = inline11494
        } else {
            inline11492 = false
        }
        var inline11493 bool = !inline11492
        t7185 = inline11493
    } else {
        t7185 = false
    }
    if t7185 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t7186 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t7186
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t7191 string = _goml_runtime_core_int_to_string(self__151)
    return t7191
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t7194 string = _goml_runtime_core_bool_to_string(self__148)
    return t7194
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t7197 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t7197
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11638 rune
    var inline11498 bool = utf8_valid_scalar(value__0)
    if inline11498 {
        var inline11499 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11500 rune = inline11499._1
        commute_field11638 = inline11500
        var t7203 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11638,
            _2: width__1,
        }
        return t7203
    } else {
        var inline11496 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11496
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t7208 bool = value__3 < 128
    if t7208 {
        return true
    } else {
        var t7209 bool = value__3 > 191
        return t7209
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t7214 bool = value__4 <= 1114111
    if t7214 {
        var t7218 bool = value__4 >= 55296
        var jp7216 bool
        if t7218 {
            var t7219 bool = value__4 <= 57343
            jp7216 = t7219
        } else {
            jp7216 = false
        }
        var t7217 bool = !jp7216
        return t7217
    } else {
        return false
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t7233 bool = index__16 < 0
    var jp7225 bool
    if t7233 {
        jp7225 = true
    } else {
        var t7234 int
        var inline11504 int = _goml_runtime_core_string_len(value__15)
        t7234 = inline11504
        var t7235 bool = index__16 > t7234
        jp7225 = t7235
    }
    if jp7225 {
        return false
    } else {
        var t7228 int
        var inline11508 int = _goml_runtime_core_string_len(value__15)
        t7228 = inline11508
        var t7229 bool = index__16 == t7228
        if t7229 {
            return true
        } else {
            var t7230 uint8
            var inline11506 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t7230 = inline11506
            var t7231_rhs uint8 = 192
            var t7231 uint8 = t7230 & t7231_rhs
            var t7232 bool = t7231 != 128
            return t7232
        }
    }
}

func main() {
    main0()
}
