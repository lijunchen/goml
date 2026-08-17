package main

import (
    _goml_context "context"
    _goml_fmt "fmt"
    _goml_slices "slices"
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
    vec.items = _goml_slices.Grow(vec.items, int(additional))
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

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
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
    raw_os_code_value Option__int
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

type _goml_m_FnIterator_____o_int_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_int_c_char_q_
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type FnIterator__int struct {
    next_fn func() Option__int
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

type Option__uint8 struct {
    _tag int32
    _v1_0 uint8
}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

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

type Result__int__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Result__uint__string struct {
    _tag int32
    _v0_0 uint
    _v1_0 string
}

type Result__float32__string struct {
    _tag int32
    _v0_0 float32
    _v1_0 string
}

type Result__float64__string struct {
    _tag int32
    _v0_0 float64
    _v1_0 string
}

type _goml_m_Result____int____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____int____std_p_num_p_ParseIntError()
}

type _goml_m_Result____int____std_p_num_p_ParseIntError_Ok struct {
    _0 int
}

func (_ _goml_m_Result____int____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____int____std_p_num_p_ParseIntError() {}

type _goml_m_Result____int____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____int____std_p_num_p_ParseIntError_Err) is_goml_m_Result____int____std_p_num_p_ParseIntError() {}

type _goml_m_Result____uint____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____uint____std_p_num_p_ParseIntError()
}

type _goml_m_Result____uint____std_p_num_p_ParseIntError_Ok struct {
    _0 uint
}

func (_ _goml_m_Result____uint____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____uint____std_p_num_p_ParseIntError() {}

type _goml_m_Result____uint____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____uint____std_p_num_p_ParseIntError_Err) is_goml_m_Result____uint____std_p_num_p_ParseIntError() {}

type _goml_m_Result____float32____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____float32____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____float32____std_p_num_p_ParseFloatError_Ok struct {
    _0 float32
}

func (_ _goml_m_Result____float32____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____float32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____float32____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____float32____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____float32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____float64____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____float64____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____float64____std_p_num_p_ParseFloatError_Ok struct {
    _0 float64
}

func (_ _goml_m_Result____float64____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____float64____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____float64____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____float64____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____float64____std_p_num_p_ParseFloatError() {}

type Option__int64 struct {
    _tag int32
    _v1_0 int64
}

type _goml_m_Option____std_p_serde_p_FieldKey interface {
    is_goml_m_Option____std_p_serde_p_FieldKey()
}

type _goml_m_Option____std_p_serde_p_FieldKey_None struct {}

func (_ _goml_m_Option____std_p_serde_p_FieldKey_None) is_goml_m_Option____std_p_serde_p_FieldKey() {}

type _goml_m_Option____std_p_serde_p_FieldKey_Some struct {
    _0 _goml_m_std_p_serde_p_FieldKey
}

func (_ _goml_m_Option____std_p_serde_p_FieldKey_Some) is_goml_m_Option____std_p_serde_p_FieldKey() {}

type _goml_m_Result____std_p_serde_p_Value____string interface {
    is_goml_m_Result____std_p_serde_p_Value____string()
}

type _goml_m_Result____std_p_serde_p_Value____string_Ok struct {
    _0 _goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Result____std_p_serde_p_Value____string_Ok) is_goml_m_Result____std_p_serde_p_Value____string() {}

type _goml_m_Result____std_p_serde_p_Value____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_serde_p_Value____string_Err) is_goml_m_Result____std_p_serde_p_Value____string() {}

type _goml_m_Result_____o_string_c__h4bbf09ce886e073b1c182b1babf9f12e_r__q_____string interface {
    is_goml_m_Result_____o_string__h05dd8cd8e038ae5a53bbc5c6c05aa6f2_r__q_____string()
}

type _goml_m_Result_____o_string_c__hecaffbd6b72fdeaeefff7411bf378c56_q_____string_Ok struct {
    _0 Tuple3_6string_3int_50Vec_44Tuple2_6string_27_goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Result_____o_string_c__hecaffbd6b72fdeaeefff7411bf378c56_q_____string_Ok) is_goml_m_Result_____o_string__h05dd8cd8e038ae5a53bbc5c6c05aa6f2_r__q_____string() {}

type _goml_m_Result_____o_string_c__h2bc7fa489d9ad0d4700220a31aaeb507______string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_string_c__h2bc7fa489d9ad0d4700220a31aaeb507______string_Err) is_goml_m_Result_____o_string__h05dd8cd8e038ae5a53bbc5c6c05aa6f2_r__q_____string() {}

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

type _goml_m_Result____Vec_l_uint8_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec_uint8
    _v1_0 string
}

type Result__bool__string struct {
    _tag int32
    _v0_0 bool
    _v1_0 string
}

type Result__int8__string struct {
    _tag int32
    _v0_0 int8
    _v1_0 string
}

type Result__int16__string struct {
    _tag int32
    _v0_0 int16
    _v1_0 string
}

type Result__int32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

type Result__int64__string struct {
    _tag int32
    _v0_0 int64
    _v1_0 string
}

type Result__uint8__string struct {
    _tag int32
    _v0_0 uint8
    _v1_0 string
}

type Result__uint16__string struct {
    _tag int32
    _v0_0 uint16
    _v1_0 string
}

type Result__uint32__string struct {
    _tag int32
    _v0_0 uint32
    _v1_0 string
}

type Result__uint64__string struct {
    _tag int32
    _v0_0 uint64
    _v1_0 string
}

type Result__char__string struct {
    _tag int32
    _v0_0 rune
    _v1_0 string
}

type Result__Option__int__string interface {
    isResult__Option__int__string()
}

type Result__Option__int__string_Ok struct {
    _0 Option__int
}

func (_ Result__Option__int__string_Ok) isResult__Option__int__string() {}

type Result__Option__int__string_Err struct {
    _0 string
}

func (_ Result__Option__int__string_Err) isResult__Option__int__string() {}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string interface {
    is_goml_m_Result____Option____std_p_serde_p_FieldKey____string()
}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Ok struct {
    _0 _goml_m_Option____std_p_serde_p_FieldKey
}

func (_ _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Ok) is_goml_m_Result____Option____std_p_serde_p_FieldKey____string() {}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Err) is_goml_m_Result____Option____std_p_serde_p_FieldKey____string() {}

type _goml_m_Result____std_p_serde_p_VariantKey____string interface {
    is_goml_m_Result____std_p_serde_p_VariantKey____string()
}

type _goml_m_Result____std_p_serde_p_VariantKey____string_Ok struct {
    _0 _goml_m_std_p_serde_p_VariantKey
}

func (_ _goml_m_Result____std_p_serde_p_VariantKey____string_Ok) is_goml_m_Result____std_p_serde_p_VariantKey____string() {}

type _goml_m_Result____std_p_serde_p_VariantKey____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_serde_p_VariantKey____string_Err) is_goml_m_Result____std_p_serde_p_VariantKey____string() {}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_ValueSerializeFrame
}

type _goml_m_Option_____o_int_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string interface {
    is_goml_m_Result_____o_bool_c_bool_c_bool_q_____string()
}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Ok struct {
    _0 Tuple3_4bool_4bool_4bool
}

func (_ _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Ok) is_goml_m_Result_____o_bool_c_bool_c_bool_q_____string() {}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Err) is_goml_m_Result_____o_bool_c_bool_c_bool_q_____string() {}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string interface {
    is_goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string()
}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Ok struct {
    _0 Tuple3_4bool_4bool_14Option__string
}

func (_ _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Ok) is_goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string() {}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Err) is_goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string() {}

type Result__Option__string__string interface {
    isResult__Option__string__string()
}

type Result__Option__string__string_Ok struct {
    _0 Option__string
}

func (_ Result__Option__string__string_Ok) isResult__Option__string__string() {}

type Result__Option__string__string_Err struct {
    _0 string
}

func (_ Result__Option__string__string_Err) isResult__Option__string__string() {}

type _goml_m_Result____std_p_json_p_Value____string interface {
    is_goml_m_Result____std_p_json_p_Value____string()
}

type _goml_m_Result____std_p_json_p_Value____string_Ok struct {
    _0 _goml_m_std_p_json_p_Value
}

func (_ _goml_m_Result____std_p_json_p_Value____string_Ok) is_goml_m_Result____std_p_json_p_Value____string() {}

type _goml_m_Result____std_p_json_p_Value____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_json_p_Value____string_Err) is_goml_m_Result____std_p_json_p_Value____string() {}

type _goml_m_Option_____o_char_c_int_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__uint32 struct {
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
    var t2909 [0]uint8 = [0]uint8{}
    var t2910 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(t2909)
    var t2911 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t2910,
    }
    return t2911
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8738 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8738
    var t2925 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2925, length__5)
    var for_index1 int = 0
    Loop_loop2927:
    for {
        var t2928 bool = for_index1 < length__5
        if t2928 {
            var for_item3 int = for_index1
            var t2929 int = for_index1 + 1
            for_index1 = t2929
            var t2930 *_goml_vec_uint8 = self__3.values
            var t2931 uint8
            var inline8734 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2931 = inline8734
            vec_push__Vec_5uint8(t2930, t2931)
            continue
        } else {
            break Loop_loop2927
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2934 string
    var inline8740 string = char_to_string(value__8)
    t2934 = inline8740
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2934)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4642 string = "" + message__201
    var t4643 string = t4642 + " at byte "
    var t4644 *ref_int_x = value__200.index
    var t4645 int
    var inline10075 int = ref_get__Ref_3int(t4644)
    t4645 = inline10075
    var t4646 string
    var inline10073 string = _goml_runtime_core_int_to_string(t4645)
    t4646 = inline10073
    var t4647 string = t4643 + t4646
    return t4647
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4662:
    for {
        var t4670 *ref_int_x = value__203.index
        var t4671 int
        var inline10096 int = ref_get__Ref_3int(t4670)
        t4671 = inline10096
        var t4672 string = value__203.input
        var t4673 int
        var inline10094 int = _goml_runtime_core_string_len(t4672)
        t4673 = inline10094
        var t4674 bool = t4671 < t4673
        var jp4664 bool
        if t4674 {
            var t4675 string = value__203.input
            var t4676 *ref_int_x = value__203.index
            var t4677 int
            var inline10088 int = ref_get__Ref_3int(t4676)
            t4677 = inline10088
            var t4678 uint8
            var inline10086 uint8 = _goml_runtime_core_string_byte_get(t4675, t4677)
            t4678 = inline10086
            var inline10077 bool = t4678 == 9
            var inline10079 bool
            if inline10077 {
                inline10079 = true
            } else {
                var inline10084 bool = t4678 == 10
                inline10079 = inline10084
            }
            var inline10081 bool
            if inline10079 {
                inline10081 = true
            } else {
                var inline10083 bool = t4678 == 13
                inline10081 = inline10083
            }
            if inline10081 {
                jp4664 = true
            } else {
                var inline10082 bool = t4678 == 32
                jp4664 = inline10082
            }
        } else {
            jp4664 = false
        }
        if jp4664 {
            var t4665 *ref_int_x = value__203.index
            var t4666 *ref_int_x = value__203.index
            var t4667 int
            var inline10092 int = ref_get__Ref_3int(t4666)
            t4667 = inline10092
            var t4668 int = t4667 + 1
            ref_set__Ref_3int(t4665, t4668)
            continue
        } else {
            break Loop_loop4662
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4709 bool = value__204 >= 48
    var jp4685 bool
    if t4709 {
        var t4710 bool = value__204 <= 57
        jp4685 = t4710
    } else {
        jp4685 = false
    }
    if jp4685 {
        var t4686 uint8 = value__204 - 48
        var t4687 uint32 = uint32(uint8(t4686))
        var t4688 Option__uint32 = Option__uint32{
            _tag: 1,
            _v1_0: t4687,
        }
        return t4688
    } else {
        var t4707 bool = value__204 >= 65
        var jp4692 bool
        if t4707 {
            var t4708 bool = value__204 <= 70
            jp4692 = t4708
        } else {
            jp4692 = false
        }
        if jp4692 {
            var t4693 uint8 = value__204 - 65
            var t4694 uint8 = t4693 + 10
            var t4695 uint32 = uint32(uint8(t4694))
            var t4696 Option__uint32 = Option__uint32{
                _tag: 1,
                _v1_0: t4695,
            }
            return t4696
        } else {
            var t4705 bool = value__204 >= 97
            var jp4700 bool
            if t4705 {
                var t4706 bool = value__204 <= 102
                jp4700 = t4706
            } else {
                jp4700 = false
            }
            if jp4700 {
                var t4701 uint8 = value__204 - 97
                var t4702 uint8 = t4701 + 10
                var t4703 uint32 = uint32(uint8(t4702))
                var t4704 Option__uint32 = Option__uint32{
                    _tag: 1,
                    _v1_0: t4703,
                }
                return t4704
            } else {
                return Option__uint32{
                    _tag: 0,
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4715 *ref_int_x = value__205.index
    var t4716 int
    var inline10124 int = ref_get__Ref_3int(t4715)
    t4716 = inline10124
    var t4717 int = t4716 + 4
    var t4718 string = value__205.input
    var t4719 int
    var inline10122 int = _goml_runtime_core_string_len(t4718)
    t4719 = inline10122
    var t4720 bool = t4717 > t4719
    if t4720 {
        var t4721 string
        var inline10098 string = "incomplete unicode escape"
        var inline10099 string = "" + inline10098
        var inline10100 string = inline10099 + " at byte "
        var inline10101 *ref_int_x = value__205.index
        var inline10102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10101)
        var inline10103 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10102)
        var inline10104 string = inline10100 + inline10103
        t4721 = inline10104
        var t4722 Result__uint32__string = Result__uint32__string{
            _tag: 1,
            _v1_0: t4721,
        }
        return t4722
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4729:
        for {
            var t4730 bool = for_index744 < for_limit745
            if t4730 {
                var for_item746 int = for_index744
                var t4731 int = for_index744 + 1
                for_index744 = t4731
                var t4732 string = value__205.input
                var t4733 *ref_int_x = value__205.index
                var t4734 int
                var inline10116 int = ref_get__Ref_3int(t4733)
                t4734 = inline10116
                var t4735 int = t4734 + for_item746
                var t4736 uint8
                var inline10114 uint8 = _goml_runtime_core_string_byte_get(t4732, t4735)
                t4736 = inline10114
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4736)
                switch mtmp748._tag {
                case 0:
                    var t4738 string
                    var inline10106 string = "invalid unicode escape"
                    var inline10107 string = "" + inline10106
                    var inline10108 string = inline10107 + " at byte "
                    var inline10109 *ref_int_x = value__205.index
                    var inline10110 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10109)
                    var inline10111 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10110)
                    var inline10112 string = inline10108 + inline10111
                    t4738 = inline10112
                    var t4739 Result__uint32__string = Result__uint32__string{
                        _tag: 1,
                        _v1_0: t4738,
                    }
                    return t4739
                case 1:
                    var x749 uint32 = mtmp748._v1_0
                    var t4740 uint32 = result__206 * 16
                    var t4741 uint32 = t4740 + x749
                    result__206 = t4741
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4729
            }
        }
        var t4724 *ref_int_x = value__205.index
        var t4725 *ref_int_x = value__205.index
        var t4726 int
        var inline10120 int = ref_get__Ref_3int(t4725)
        t4726 = inline10120
        var t4727 int = t4726 + 4
        ref_set__Ref_3int(t4724, t4727)
        var t4728 Result__uint32__string = Result__uint32__string{
            _tag: 0,
            _v0_0: result__206,
        }
        return t4728
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline10137 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline10137
    switch mtmp753._tag {
    case 0:
        var t4746 string
        var inline10126 string = "invalid unicode codepoint"
        var inline10127 string = "" + inline10126
        var inline10128 string = inline10127 + " at byte "
        var inline10129 *ref_int_x = value__209.index
        var inline10130 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10129)
        var inline10131 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10130)
        var inline10132 string = inline10128 + inline10131
        t4746 = inline10132
        var t4747 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: t4746,
        }
        return t4747
    case 1:
        var x754 rune = mtmp753._v1_0
        var inline10134 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10134)
        var t4748 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t4748
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4752 uint32
    switch mtmp756._tag {
    case 0:
        var x757 uint32 = mtmp756._v0_0
        jp4752 = x757
        var t4812 bool = jp4752 >= 55296
        var jp4756 bool
        if t4812 {
            var t4813 bool = jp4752 <= 56319
            jp4756 = t4813
        } else {
            jp4756 = false
        }
        if jp4756 {
            var t4792 *ref_int_x = value__213.index
            var t4793 int
            var inline10177 int = ref_get__Ref_3int(t4792)
            t4793 = inline10177
            var t4794 int = t4793 + 2
            var t4795 string = value__213.input
            var t4796 int
            var inline10175 int = _goml_runtime_core_string_len(t4795)
            t4796 = inline10175
            var t4797 bool = t4794 > t4796
            var jp4785 bool
            if t4797 {
                jp4785 = true
            } else {
                var t4798 string = value__213.input
                var t4799 *ref_int_x = value__213.index
                var t4800 int
                var inline10141 int = ref_get__Ref_3int(t4799)
                t4800 = inline10141
                var t4801 uint8
                var inline10139 uint8 = _goml_runtime_core_string_byte_get(t4798, t4800)
                t4801 = inline10139
                var t4802 bool = t4801 != 92
                jp4785 = t4802
            }
            var jp4760 bool
            if jp4785 {
                jp4760 = true
            } else {
                var t4786 string = value__213.input
                var t4787 *ref_int_x = value__213.index
                var t4788 int
                var inline10145 int = ref_get__Ref_3int(t4787)
                t4788 = inline10145
                var t4789 int = t4788 + 1
                var t4790 uint8
                var inline10143 uint8 = _goml_runtime_core_string_byte_get(t4786, t4789)
                t4790 = inline10143
                var t4791 bool = t4790 != 117
                jp4760 = t4791
            }
            if jp4760 {
                var t4761 string
                var inline10147 string = "missing low surrogate"
                var inline10148 string = "" + inline10147
                var inline10149 string = inline10148 + " at byte "
                var inline10150 *ref_int_x = value__213.index
                var inline10151 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10150)
                var inline10152 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10151)
                var inline10153 string = inline10149 + inline10152
                t4761 = inline10153
                var t4762 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t4761,
                }
                return t4762
            } else {
                var t4763 *ref_int_x = value__213.index
                var t4764 *ref_int_x = value__213.index
                var t4765 int
                var inline10173 int = ref_get__Ref_3int(t4764)
                t4765 = inline10173
                var t4766 int = t4765 + 2
                ref_set__Ref_3int(t4763, t4766)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4768 uint32
                switch mtmp760._tag {
                case 0:
                    var x761 uint32 = mtmp760._v0_0
                    jp4768 = x761
                    var t4781 bool = jp4768 < 56320
                    var jp4772 bool
                    if t4781 {
                        jp4772 = true
                    } else {
                        var t4782 bool = jp4768 > 57343
                        jp4772 = t4782
                    }
                    if jp4772 {
                        var t4773 string
                        var inline10155 string = "invalid low surrogate"
                        var inline10156 string = "" + inline10155
                        var inline10157 string = inline10156 + " at byte "
                        var inline10158 *ref_int_x = value__213.index
                        var inline10159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10158)
                        var inline10160 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10159)
                        var inline10161 string = inline10157 + inline10160
                        t4773 = inline10161
                        var t4774 Result__unit__string = Result__unit__string{
                            _tag: 1,
                            _v1_0: t4773,
                        }
                        return t4774
                    } else {
                        var t4775 uint32 = jp4752 - 55296
                        var t4776 uint32 = t4775 * 1024
                        var t4777 uint32 = 65536 + t4776
                        var t4778 uint32 = t4777 + jp4768
                        var t4779 uint32 = t4778 - 56320
                        var inline10163 Option__char = char_from_uint32(t4779)
                        switch inline10163._tag {
                        case 0:
                            var inline10164 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10165 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: inline10164,
                            }
                            return inline10165
                        case 1:
                            var inline10166 rune = inline10163._v1_0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10166)
                            var inline10169 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return inline10169
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x762 string = mtmp760._v1_0
                    var t4783 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x762,
                    }
                    return t4783
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4810 bool = jp4752 >= 56320
            var jp4806 bool
            if t4810 {
                var t4811 bool = jp4752 <= 57343
                jp4806 = t4811
            } else {
                jp4806 = false
            }
            if jp4806 {
                var t4807 string
                var inline10179 string = "unexpected low surrogate"
                var inline10180 string = "" + inline10179
                var inline10181 string = inline10180 + " at byte "
                var inline10182 *ref_int_x = value__213.index
                var inline10183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10182)
                var inline10184 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10183)
                var inline10185 string = inline10181 + inline10184
                t4807 = inline10185
                var t4808 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t4807,
                }
                return t4808
            } else {
                var t4809 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4752)
                return t4809
            }
        }
    case 1:
        var x758 string = mtmp756._v1_0
        var t4814 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x758,
        }
        return t4814
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4930 *ref_int_x = value__217.index
    var t4931 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4930)
    var t4932 string = value__217.input
    var t4933 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4932)
    var t4934 bool = t4931 >= t4933
    var jp4922 bool
    if t4934 {
        jp4922 = true
    } else {
        var t4935 string = value__217.input
        var t4936 *ref_int_x = value__217.index
        var t4937 int
        var inline10189 int = ref_get__Ref_3int(t4936)
        t4937 = inline10189
        var t4938 uint8
        var inline10187 uint8 = _goml_runtime_core_string_byte_get(t4935, t4937)
        t4938 = inline10187
        var t4939 bool = t4938 != 34
        jp4922 = t4939
    }
    if jp4922 {
        var t4923 string
        var inline10191 string = "expected string"
        var inline10192 string = "" + inline10191
        var inline10193 string = inline10192 + " at byte "
        var inline10194 *ref_int_x = value__217.index
        var inline10195 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10194)
        var inline10196 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10195)
        var inline10197 string = inline10193 + inline10196
        t4923 = inline10197
        var t4924 Result__string__string = Result__string__string_Err{
            _0: t4923,
        }
        return t4924
    } else {
        var t4925 *ref_int_x = value__217.index
        var t4926 *ref_int_x = value__217.index
        var t4927 int
        var inline10201 int = ref_get__Ref_3int(t4926)
        t4927 = inline10201
        var t4928 int = t4927 + 1
        ref_set__Ref_3int(t4925, t4928)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4818 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4818)
        Loop_loop4822:
        for {
            var t4823 *ref_int_x = value__217.index
            var t4824 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4823)
            var t4825 string = value__217.input
            var t4826 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4825)
            var t4827 bool = t4824 < t4826
            if t4827 {
                var t4828 string = value__217.input
                var t4829 *ref_int_x = value__217.index
                var t4830 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4829)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4828, t4830)
                var t4832 bool = byte__220 == 34
                if t4832 {
                    var t4840 *ref_int_x = value__217.index
                    var t4841 int
                    var inline10216 int = ref_get__Ref_3int(t4840)
                    t4841 = inline10216
                    var t4842 bool = segment__219 < t4841
                    if t4842 {
                        var t4843 string = value__217.input
                        var t4844 *ref_int_x = value__217.index
                        var t4845 int
                        var inline10205 int = ref_get__Ref_3int(t4844)
                        t4845 = inline10205
                        var t4846 string
                        var inline10203 string = string_byte_slice(t4843, segment__219, t4845)
                        t4846 = inline10203
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4846)
                    } else {}
                    var t4834 *ref_int_x = value__217.index
                    var t4835 *ref_int_x = value__217.index
                    var t4836 int
                    var inline10214 int = ref_get__Ref_3int(t4835)
                    t4836 = inline10214
                    var t4837 int = t4836 + 1
                    ref_set__Ref_3int(t4834, t4837)
                    var t4838 string
                    var inline10207 *_goml_vec_uint8 = builder__218.values
                    var inline10208 Tuple2_4bool_6string = string_from_utf8(inline10207)
                    var inline10209 string = inline10208._1
                    t4838 = inline10209
                    var t4839 Result__string__string = Result__string__string_Ok{
                        _0: t4838,
                    }
                    return t4839
                } else {
                    var t4849 bool = byte__220 == 92
                    if t4849 {
                        var t4904 *ref_int_x = value__217.index
                        var t4905 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4904)
                        var t4906 bool = segment__219 < t4905
                        if t4906 {
                            var t4907 string = value__217.input
                            var t4908 *ref_int_x = value__217.index
                            var t4909 int
                            var inline10220 int = ref_get__Ref_3int(t4908)
                            t4909 = inline10220
                            var t4910 string
                            var inline10218 string = string_byte_slice(t4907, segment__219, t4909)
                            t4910 = inline10218
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4910)
                        } else {}
                        var t4851 *ref_int_x = value__217.index
                        var t4852 *ref_int_x = value__217.index
                        var t4853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4852)
                        var t4854 int = t4853 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4851, t4854)
                        var t4897 *ref_int_x = value__217.index
                        var t4898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4897)
                        var t4899 string = value__217.input
                        var t4900 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4899)
                        var t4901 bool = t4898 >= t4900
                        if t4901 {
                            var t4902 string
                            var inline10222 string = "incomplete escape"
                            var inline10223 string = "" + inline10222
                            var inline10224 string = inline10223 + " at byte "
                            var inline10225 *ref_int_x = value__217.index
                            var inline10226 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10225)
                            var inline10227 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10226)
                            var inline10228 string = inline10224 + inline10227
                            t4902 = inline10228
                            var t4903 Result__string__string = Result__string__string_Err{
                                _0: t4902,
                            }
                            return t4903
                        } else {
                            var t4856 string = value__217.input
                            var t4857 *ref_int_x = value__217.index
                            var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4856, t4858)
                            var t4859 *ref_int_x = value__217.index
                            var t4860 *ref_int_x = value__217.index
                            var t4861 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4860)
                            var t4862 int = t4861 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4859, t4862)
                            var t4866 bool = escape__221 == 34
                            if t4866 {
                                var inline10230 rune = 34
                                var inline10231 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10230)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10231)
                                var t4864 *ref_int_x = value__217.index
                                var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                segment__219 = t4865
                                continue
                            } else {
                                var t4869 bool = escape__221 == 92
                                if t4869 {
                                    var inline10234 rune = 92
                                    var inline10235 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10234)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10235)
                                    var t4864 *ref_int_x = value__217.index
                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                    segment__219 = t4865
                                    continue
                                } else {
                                    var t4872 bool = escape__221 == 47
                                    if t4872 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4864 *ref_int_x = value__217.index
                                        var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                        segment__219 = t4865
                                        continue
                                    } else {
                                        var t4875 bool = escape__221 == 98
                                        if t4875 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770._tag {
                                            case 0:
                                                var t4864 *ref_int_x = value__217.index
                                                var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                segment__219 = t4865
                                                continue
                                            case 1:
                                                var x771 rune = mtmp770._v1_0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4864 *ref_int_x = value__217.index
                                                var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                segment__219 = t4865
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4879 bool = escape__221 == 102
                                            if t4879 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772._tag {
                                                case 0:
                                                    var t4864 *ref_int_x = value__217.index
                                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                    segment__219 = t4865
                                                    continue
                                                case 1:
                                                    var x773 rune = mtmp772._v1_0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4864 *ref_int_x = value__217.index
                                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                    segment__219 = t4865
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4883 bool = escape__221 == 110
                                                if t4883 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4864 *ref_int_x = value__217.index
                                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                    segment__219 = t4865
                                                    continue
                                                } else {
                                                    var t4886 bool = escape__221 == 114
                                                    if t4886 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4864 *ref_int_x = value__217.index
                                                        var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                        segment__219 = t4865
                                                        continue
                                                    } else {
                                                        var t4889 bool = escape__221 == 116
                                                        if t4889 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4864 *ref_int_x = value__217.index
                                                            var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                            segment__219 = t4865
                                                            continue
                                                        } else {
                                                            var t4892 bool = escape__221 == 117
                                                            if t4892 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774._tag {
                                                                case 0:
                                                                    var t4864 *ref_int_x = value__217.index
                                                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                                    segment__219 = t4865
                                                                    continue
                                                                case 1:
                                                                    var x776 string = mtmp774._v1_0
                                                                    var t4894 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4894
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4895 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4896 Result__string__string = Result__string__string_Err{
                                                                    _0: t4895,
                                                                }
                                                                return t4896
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
                        var t4913 bool = byte__220 < 32
                        if t4913 {
                            var t4914 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4915 Result__string__string = Result__string__string_Err{
                                _0: t4914,
                            }
                            return t4915
                        } else {
                            var t4916 *ref_int_x = value__217.index
                            var t4917 *ref_int_x = value__217.index
                            var t4918 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4917)
                            var t4919 int = t4918 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4916, t4919)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4822
            }
        }
        var t4820 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4821 Result__string__string = Result__string__string_Err{
            _0: t4820,
        }
        return t4821
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4948 *ref_int_x = value__225.index
    var start__226 int
    var inline10255 int = ref_get__Ref_3int(t4948)
    start__226 = inline10255
    Loop_loop4953:
    for {
        var t4961 *ref_int_x = value__225.index
        var t4962 int
        var inline10251 int = ref_get__Ref_3int(t4961)
        t4962 = inline10251
        var t4963 string = value__225.input
        var t4964 int
        var inline10249 int = _goml_runtime_core_string_len(t4963)
        t4964 = inline10249
        var t4965 bool = t4962 < t4964
        var jp4955 bool
        if t4965 {
            var t4966 string = value__225.input
            var t4967 *ref_int_x = value__225.index
            var t4968 int
            var inline10243 int = ref_get__Ref_3int(t4967)
            t4968 = inline10243
            var t4969 uint8
            var inline10241 uint8 = _goml_runtime_core_string_byte_get(t4966, t4968)
            t4969 = inline10241
            var inline10238 bool = t4969 >= 48
            if inline10238 {
                var inline10239 bool = t4969 <= 57
                jp4955 = inline10239
            } else {
                jp4955 = false
            }
        } else {
            jp4955 = false
        }
        if jp4955 {
            var t4956 *ref_int_x = value__225.index
            var t4957 *ref_int_x = value__225.index
            var t4958 int
            var inline10247 int = ref_get__Ref_3int(t4957)
            t4958 = inline10247
            var t4959 int = t4958 + 1
            ref_set__Ref_3int(t4956, t4959)
            continue
        } else {
            break Loop_loop4953
        }
    }
    var t4950 *ref_int_x = value__225.index
    var t4951 int
    var inline10253 int = ref_get__Ref_3int(t4950)
    t4951 = inline10253
    var t4952 bool = t4951 > start__226
    return t4952
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4973 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4973)
    var t5094 string = value__227.input
    var t5095 *ref_int_x = value__227.index
    var t5096 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5095)
    var t5097 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5094, t5096)
    var t5098 bool = t5097 == 45
    if t5098 {
        var t5099 *ref_int_x = value__227.index
        var t5100 *ref_int_x = value__227.index
        var t5101 int
        var inline10259 int = ref_get__Ref_3int(t5100)
        t5101 = inline10259
        var t5102 int = t5101 + 1
        ref_set__Ref_3int(t5099, t5102)
    } else {}
    var t5057 *ref_int_x = value__227.index
    var t5058 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5057)
    var t5059 string = value__227.input
    var t5060 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5059)
    var t5061 bool = t5058 >= t5060
    if t5061 {
        var t5062 string
        var inline10261 string = "incomplete number"
        var inline10262 string = "" + inline10261
        var inline10263 string = inline10262 + " at byte "
        var inline10264 *ref_int_x = value__227.index
        var inline10265 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10264)
        var inline10266 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10265)
        var inline10267 string = inline10263 + inline10266
        t5062 = inline10267
        var t5063 Result__string__string = Result__string__string_Err{
            _0: t5062,
        }
        return t5063
    } else {
        var t5065 string = value__227.input
        var t5066 *ref_int_x = value__227.index
        var t5067 int
        var inline10302 int = ref_get__Ref_3int(t5066)
        t5067 = inline10302
        var t5068 uint8
        var inline10300 uint8 = _goml_runtime_core_string_byte_get(t5065, t5067)
        t5068 = inline10300
        var t5069 bool = t5068 == 48
        if t5069 {
            var t5070 *ref_int_x = value__227.index
            var t5071 *ref_int_x = value__227.index
            var t5072 int
            var inline10290 int = ref_get__Ref_3int(t5071)
            t5072 = inline10290
            var t5073 int = t5072 + 1
            ref_set__Ref_3int(t5070, t5073)
            var t5079 *ref_int_x = value__227.index
            var t5080 int
            var inline10286 int = ref_get__Ref_3int(t5079)
            t5080 = inline10286
            var t5081 string = value__227.input
            var t5082 int
            var inline10284 int = _goml_runtime_core_string_len(t5081)
            t5082 = inline10284
            var t5083 bool = t5080 < t5082
            var jp5076 bool
            if t5083 {
                var t5084 string = value__227.input
                var t5085 *ref_int_x = value__227.index
                var t5086 int
                var inline10274 int = ref_get__Ref_3int(t5085)
                t5086 = inline10274
                var t5087 uint8
                var inline10272 uint8 = _goml_runtime_core_string_byte_get(t5084, t5086)
                t5087 = inline10272
                var inline10269 bool = t5087 >= 48
                if inline10269 {
                    var inline10270 bool = t5087 <= 57
                    jp5076 = inline10270
                } else {
                    jp5076 = false
                }
            } else {
                jp5076 = false
            }
            if jp5076 {
                var t5077 string
                var inline10276 string = "invalid leading zero"
                var inline10277 string = "" + inline10276
                var inline10278 string = inline10277 + " at byte "
                var inline10279 *ref_int_x = value__227.index
                var inline10280 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10279)
                var inline10281 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10280)
                var inline10282 string = inline10278 + inline10281
                t5077 = inline10282
                var t5078 Result__string__string = Result__string__string_Err{
                    _0: t5077,
                }
                return t5078
            } else {
                var t5047 *ref_int_x = value__227.index
                var t5048 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5047)
                var t5049 string = value__227.input
                var t5050 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5049)
                var t5051 bool = t5048 < t5050
                var jp5037 bool
                if t5051 {
                    var t5052 string = value__227.input
                    var t5053 *ref_int_x = value__227.index
                    var t5054 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5053)
                    var t5055 uint8
                    var inline10304 uint8 = _goml_runtime_core_string_byte_get(t5052, t5054)
                    t5055 = inline10304
                    var t5056 bool = t5055 == 46
                    jp5037 = t5056
                } else {
                    jp5037 = false
                }
                if jp5037 {
                    var t5038 *ref_int_x = value__227.index
                    var t5039 *ref_int_x = value__227.index
                    var t5040 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5039)
                    var t5041 int = t5040 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5038, t5041)
                    var t5043 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5044 bool = !t5043
                    if t5044 {
                        var t5045 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5046 Result__string__string = Result__string__string_Err{
                            _0: t5045,
                        }
                        return t5046
                    } else {
                        var t5019 *ref_int_x = value__227.index
                        var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5019)
                        var t5021 string = value__227.input
                        var t5022 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5021)
                        var t5023 bool = t5020 < t5022
                        var jp4984 bool
                        if t5023 {
                            var t5026 string = value__227.input
                            var t5027 *ref_int_x = value__227.index
                            var t5028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5027)
                            var t5029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5026, t5028)
                            var t5030 bool = t5029 == 101
                            if t5030 {
                                jp4984 = true
                            } else {
                                var t5031 string = value__227.input
                                var t5032 *ref_int_x = value__227.index
                                var t5033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5032)
                                var t5034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5031, t5033)
                                var t5035 bool = t5034 == 69
                                jp4984 = t5035
                            }
                        } else {
                            jp4984 = false
                        }
                        if jp4984 {
                            var t4985 *ref_int_x = value__227.index
                            var t4986 *ref_int_x = value__227.index
                            var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                            var t4988 int = t4987 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4985, t4988)
                            var t5002 *ref_int_x = value__227.index
                            var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                            var t5004 string = value__227.input
                            var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                            var t5006 bool = t5003 < t5005
                            var jp4996 bool
                            if t5006 {
                                var t5009 string = value__227.input
                                var t5010 *ref_int_x = value__227.index
                                var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                                var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                                var t5013 bool = t5012 == 43
                                if t5013 {
                                    jp4996 = true
                                } else {
                                    var t5014 string = value__227.input
                                    var t5015 *ref_int_x = value__227.index
                                    var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                                    var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                                    var t5018 bool = t5017 == 45
                                    jp4996 = t5018
                                }
                            } else {
                                jp4996 = false
                            }
                            if jp4996 {
                                var t4997 *ref_int_x = value__227.index
                                var t4998 *ref_int_x = value__227.index
                                var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                                var t5000 int = t4999 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4997, t5000)
                            } else {}
                            var t4991 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4992 bool = !t4991
                            if t4992 {
                                var t4993 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4994 Result__string__string = Result__string__string_Err{
                                    _0: t4993,
                                }
                                return t4994
                            } else {
                                var t4978 string = value__227.input
                                var t4979 *ref_int_x = value__227.index
                                var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                                var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                                var t4982 Result__string__string = Result__string__string_Ok{
                                    _0: t4981,
                                }
                                return t4982
                            }
                        } else {
                            var t4978 string = value__227.input
                            var t4979 *ref_int_x = value__227.index
                            var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                            var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                            var t4982 Result__string__string = Result__string__string_Ok{
                                _0: t4981,
                            }
                            return t4982
                        }
                    }
                } else {
                    var t5019 *ref_int_x = value__227.index
                    var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5019)
                    var t5021 string = value__227.input
                    var t5022 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5021)
                    var t5023 bool = t5020 < t5022
                    var jp4984 bool
                    if t5023 {
                        var t5026 string = value__227.input
                        var t5027 *ref_int_x = value__227.index
                        var t5028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5027)
                        var t5029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5026, t5028)
                        var t5030 bool = t5029 == 101
                        if t5030 {
                            jp4984 = true
                        } else {
                            var t5031 string = value__227.input
                            var t5032 *ref_int_x = value__227.index
                            var t5033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5032)
                            var t5034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5031, t5033)
                            var t5035 bool = t5034 == 69
                            jp4984 = t5035
                        }
                    } else {
                        jp4984 = false
                    }
                    if jp4984 {
                        var t4985 *ref_int_x = value__227.index
                        var t4986 *ref_int_x = value__227.index
                        var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                        var t4988 int = t4987 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4985, t4988)
                        var t5002 *ref_int_x = value__227.index
                        var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                        var t5004 string = value__227.input
                        var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                        var t5006 bool = t5003 < t5005
                        var jp4996 bool
                        if t5006 {
                            var t5009 string = value__227.input
                            var t5010 *ref_int_x = value__227.index
                            var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                            var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                            var t5013 bool = t5012 == 43
                            if t5013 {
                                jp4996 = true
                            } else {
                                var t5014 string = value__227.input
                                var t5015 *ref_int_x = value__227.index
                                var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                                var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                                var t5018 bool = t5017 == 45
                                jp4996 = t5018
                            }
                        } else {
                            jp4996 = false
                        }
                        if jp4996 {
                            var t4997 *ref_int_x = value__227.index
                            var t4998 *ref_int_x = value__227.index
                            var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                            var t5000 int = t4999 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4997, t5000)
                        } else {}
                        var t4991 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4992 bool = !t4991
                        if t4992 {
                            var t4993 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4994 Result__string__string = Result__string__string_Err{
                                _0: t4993,
                            }
                            return t4994
                        } else {
                            var t4978 string = value__227.input
                            var t4979 *ref_int_x = value__227.index
                            var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                            var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                            var t4982 Result__string__string = Result__string__string_Ok{
                                _0: t4981,
                            }
                            return t4982
                        }
                    } else {
                        var t4978 string = value__227.input
                        var t4979 *ref_int_x = value__227.index
                        var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                        var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                        var t4982 Result__string__string = Result__string__string_Ok{
                            _0: t4981,
                        }
                        return t4982
                    }
                }
            }
        } else {
            var t5090 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5091 bool = !t5090
            if t5091 {
                var t5092 string
                var inline10292 string = "expected number"
                var inline10293 string = "" + inline10292
                var inline10294 string = inline10293 + " at byte "
                var inline10295 *ref_int_x = value__227.index
                var inline10296 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10295)
                var inline10297 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10296)
                var inline10298 string = inline10294 + inline10297
                t5092 = inline10298
                var t5093 Result__string__string = Result__string__string_Err{
                    _0: t5092,
                }
                return t5093
            } else {
                var t5047 *ref_int_x = value__227.index
                var t5048 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5047)
                var t5049 string = value__227.input
                var t5050 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5049)
                var t5051 bool = t5048 < t5050
                var jp5037 bool
                if t5051 {
                    var t5052 string = value__227.input
                    var t5053 *ref_int_x = value__227.index
                    var t5054 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5053)
                    var t5055 uint8
                    var inline10304 uint8 = _goml_runtime_core_string_byte_get(t5052, t5054)
                    t5055 = inline10304
                    var t5056 bool = t5055 == 46
                    jp5037 = t5056
                } else {
                    jp5037 = false
                }
                if jp5037 {
                    var t5038 *ref_int_x = value__227.index
                    var t5039 *ref_int_x = value__227.index
                    var t5040 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5039)
                    var t5041 int = t5040 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5038, t5041)
                    var t5043 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5044 bool = !t5043
                    if t5044 {
                        var t5045 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5046 Result__string__string = Result__string__string_Err{
                            _0: t5045,
                        }
                        return t5046
                    } else {
                        var t5019 *ref_int_x = value__227.index
                        var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5019)
                        var t5021 string = value__227.input
                        var t5022 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5021)
                        var t5023 bool = t5020 < t5022
                        var jp4984 bool
                        if t5023 {
                            var t5026 string = value__227.input
                            var t5027 *ref_int_x = value__227.index
                            var t5028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5027)
                            var t5029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5026, t5028)
                            var t5030 bool = t5029 == 101
                            if t5030 {
                                jp4984 = true
                            } else {
                                var t5031 string = value__227.input
                                var t5032 *ref_int_x = value__227.index
                                var t5033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5032)
                                var t5034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5031, t5033)
                                var t5035 bool = t5034 == 69
                                jp4984 = t5035
                            }
                        } else {
                            jp4984 = false
                        }
                        if jp4984 {
                            var t4985 *ref_int_x = value__227.index
                            var t4986 *ref_int_x = value__227.index
                            var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                            var t4988 int = t4987 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4985, t4988)
                            var t5002 *ref_int_x = value__227.index
                            var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                            var t5004 string = value__227.input
                            var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                            var t5006 bool = t5003 < t5005
                            var jp4996 bool
                            if t5006 {
                                var t5009 string = value__227.input
                                var t5010 *ref_int_x = value__227.index
                                var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                                var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                                var t5013 bool = t5012 == 43
                                if t5013 {
                                    jp4996 = true
                                } else {
                                    var t5014 string = value__227.input
                                    var t5015 *ref_int_x = value__227.index
                                    var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                                    var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                                    var t5018 bool = t5017 == 45
                                    jp4996 = t5018
                                }
                            } else {
                                jp4996 = false
                            }
                            if jp4996 {
                                var t4997 *ref_int_x = value__227.index
                                var t4998 *ref_int_x = value__227.index
                                var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                                var t5000 int = t4999 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4997, t5000)
                            } else {}
                            var t4991 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4992 bool = !t4991
                            if t4992 {
                                var t4993 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4994 Result__string__string = Result__string__string_Err{
                                    _0: t4993,
                                }
                                return t4994
                            } else {
                                var t4978 string = value__227.input
                                var t4979 *ref_int_x = value__227.index
                                var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                                var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                                var t4982 Result__string__string = Result__string__string_Ok{
                                    _0: t4981,
                                }
                                return t4982
                            }
                        } else {
                            var t4978 string = value__227.input
                            var t4979 *ref_int_x = value__227.index
                            var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                            var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                            var t4982 Result__string__string = Result__string__string_Ok{
                                _0: t4981,
                            }
                            return t4982
                        }
                    }
                } else {
                    var t5019 *ref_int_x = value__227.index
                    var t5020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5019)
                    var t5021 string = value__227.input
                    var t5022 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5021)
                    var t5023 bool = t5020 < t5022
                    var jp4984 bool
                    if t5023 {
                        var t5026 string = value__227.input
                        var t5027 *ref_int_x = value__227.index
                        var t5028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5027)
                        var t5029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5026, t5028)
                        var t5030 bool = t5029 == 101
                        if t5030 {
                            jp4984 = true
                        } else {
                            var t5031 string = value__227.input
                            var t5032 *ref_int_x = value__227.index
                            var t5033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5032)
                            var t5034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5031, t5033)
                            var t5035 bool = t5034 == 69
                            jp4984 = t5035
                        }
                    } else {
                        jp4984 = false
                    }
                    if jp4984 {
                        var t4985 *ref_int_x = value__227.index
                        var t4986 *ref_int_x = value__227.index
                        var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                        var t4988 int = t4987 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4985, t4988)
                        var t5002 *ref_int_x = value__227.index
                        var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                        var t5004 string = value__227.input
                        var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                        var t5006 bool = t5003 < t5005
                        var jp4996 bool
                        if t5006 {
                            var t5009 string = value__227.input
                            var t5010 *ref_int_x = value__227.index
                            var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                            var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                            var t5013 bool = t5012 == 43
                            if t5013 {
                                jp4996 = true
                            } else {
                                var t5014 string = value__227.input
                                var t5015 *ref_int_x = value__227.index
                                var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                                var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                                var t5018 bool = t5017 == 45
                                jp4996 = t5018
                            }
                        } else {
                            jp4996 = false
                        }
                        if jp4996 {
                            var t4997 *ref_int_x = value__227.index
                            var t4998 *ref_int_x = value__227.index
                            var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                            var t5000 int = t4999 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4997, t5000)
                        } else {}
                        var t4991 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4992 bool = !t4991
                        if t4992 {
                            var t4993 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4994 Result__string__string = Result__string__string_Err{
                                _0: t4993,
                            }
                            return t4994
                        } else {
                            var t4978 string = value__227.input
                            var t4979 *ref_int_x = value__227.index
                            var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                            var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                            var t4982 Result__string__string = Result__string__string_Ok{
                                _0: t4981,
                            }
                            return t4982
                        }
                    } else {
                        var t4978 string = value__227.input
                        var t4979 *ref_int_x = value__227.index
                        var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                        var t4981 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4978, start__228, t4980)
                        var t4982 Result__string__string = Result__string__string_Ok{
                            _0: t4981,
                        }
                        return t4982
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5125 *ref_int_x = value__230.index
    var t5126 int
    var inline10332 int = ref_get__Ref_3int(t5125)
    t5126 = inline10332
    var t5127 int
    var inline10330 int = _goml_runtime_core_string_len(expected__231)
    t5127 = inline10330
    var t5128 int = t5126 + t5127
    var t5129 string = value__230.input
    var t5130 int
    var inline10328 int = _goml_runtime_core_string_len(t5129)
    t5130 = inline10328
    var t5131 bool = t5128 <= t5130
    var jp5116 bool
    if t5131 {
        var t5132 string = value__230.input
        var t5133 *ref_int_x = value__230.index
        var t5134 int
        var inline10312 int = ref_get__Ref_3int(t5133)
        t5134 = inline10312
        var t5135 *ref_int_x = value__230.index
        var t5136 int
        var inline10310 int = ref_get__Ref_3int(t5135)
        t5136 = inline10310
        var t5137 int
        var inline10308 int = _goml_runtime_core_string_len(expected__231)
        t5137 = inline10308
        var t5138 int = t5136 + t5137
        var t5139 string
        var inline10306 string = string_byte_slice(t5132, t5134, t5138)
        t5139 = inline10306
        var t5140 bool = t5139 == expected__231
        jp5116 = t5140
    } else {
        jp5116 = false
    }
    if jp5116 {
        var t5117 *ref_int_x = value__230.index
        var t5118 *ref_int_x = value__230.index
        var t5119 int
        var inline10318 int = ref_get__Ref_3int(t5118)
        t5119 = inline10318
        var t5120 int
        var inline10316 int = _goml_runtime_core_string_len(expected__231)
        t5120 = inline10316
        var t5121 int = t5119 + t5120
        ref_set__Ref_3int(t5117, t5121)
        var t5122 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t5122
    } else {
        var t5123 string
        var inline10320 string = "invalid literal"
        var inline10321 string = "" + inline10320
        var inline10322 string = inline10321 + " at byte "
        var inline10323 *ref_int_x = value__230.index
        var inline10324 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10323)
        var inline10325 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10324)
        var inline10326 string = inline10322 + inline10325
        t5123 = inline10326
        var t5124 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5123,
        }
        return t5124
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5143 *ref_int_x = value__233.index
    var t5144 *ref_int_x = value__233.index
    var t5145 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5144)
    var t5146 int = t5145 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5143, t5146)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var t5147 [0]_goml_m_std_p_json_p_Value = [0]_goml_m_std_p_json_p_Value{}
    var result__234 *_goml_vec__goml_m_std_p_json_p_Value = func(values [0]_goml_m_std_p_json_p_Value) *_goml_vec__goml_m_std_p_json_p_Value {
        return &_goml_vec__goml_m_std_p_json_p_Value{
            items: values[0:len(values)],
        }
    }(t5147)
    var t5202 *ref_int_x = value__233.index
    var t5203 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5202)
    var t5204 string = value__233.input
    var t5205 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5204)
    var t5206 bool = t5203 < t5205
    var jp5195 bool
    if t5206 {
        var t5207 string = value__233.input
        var t5208 *ref_int_x = value__233.index
        var t5209 int
        var inline10336 int = ref_get__Ref_3int(t5208)
        t5209 = inline10336
        var t5210 uint8
        var inline10334 uint8 = _goml_runtime_core_string_byte_get(t5207, t5209)
        t5210 = inline10334
        var t5211 bool = t5210 == 93
        jp5195 = t5211
    } else {
        jp5195 = false
    }
    if jp5195 {
        var t5196 *ref_int_x = value__233.index
        var t5197 *ref_int_x = value__233.index
        var t5198 int
        var inline10340 int = ref_get__Ref_3int(t5197)
        t5198 = inline10340
        var t5199 int = t5198 + 1
        ref_set__Ref_3int(t5196, t5199)
        var t5200 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: result__234,
        }
        var t5201 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5200,
        }
        return t5201
    } else {
        Loop_loop5152:
        for {
            var t5153 *ref_int_x = value__233.index
            var t5154 int
            var inline10382 int = ref_get__Ref_3int(t5153)
            t5154 = inline10382
            var t5155 string = value__233.input
            var t5156 int
            var inline10380 int = _goml_runtime_core_string_len(t5155)
            t5156 = inline10380
            var t5157 bool = t5154 < t5156
            if t5157 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5159 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp5159 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(result__234, jp5159)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5161 *ref_int_x = value__233.index
                    var t5162 int
                    var inline10376 int = ref_get__Ref_3int(t5161)
                    t5162 = inline10376
                    var t5163 string = value__233.input
                    var t5164 int
                    var inline10374 int = _goml_runtime_core_string_len(t5163)
                    t5164 = inline10374
                    var t5165 bool = t5162 >= t5164
                    if t5165 {
                        var t5166 string
                        var inline10342 string = "unterminated array"
                        var inline10343 string = "" + inline10342
                        var inline10344 string = inline10343 + " at byte "
                        var inline10345 *ref_int_x = value__233.index
                        var inline10346 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10345)
                        var inline10347 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10346)
                        var inline10348 string = inline10344 + inline10347
                        t5166 = inline10348
                        var t5167 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5166,
                        }
                        return t5167
                    } else {
                        var t5169 string = value__233.input
                        var t5170 *ref_int_x = value__233.index
                        var t5171 int
                        var inline10372 int = ref_get__Ref_3int(t5170)
                        t5171 = inline10372
                        var t5172 uint8
                        var inline10370 uint8 = _goml_runtime_core_string_byte_get(t5169, t5171)
                        t5172 = inline10370
                        var t5173 bool = t5172 == 93
                        if t5173 {
                            var t5174 *ref_int_x = value__233.index
                            var t5175 *ref_int_x = value__233.index
                            var t5176 int
                            var inline10352 int = ref_get__Ref_3int(t5175)
                            t5176 = inline10352
                            var t5177 int = t5176 + 1
                            ref_set__Ref_3int(t5174, t5177)
                            var t5178 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: result__234,
                            }
                            var t5179 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t5178,
                            }
                            return t5179
                        } else {
                            var t5181 string = value__233.input
                            var t5182 *ref_int_x = value__233.index
                            var t5183 int
                            var inline10368 int = ref_get__Ref_3int(t5182)
                            t5183 = inline10368
                            var t5184 uint8
                            var inline10366 uint8 = _goml_runtime_core_string_byte_get(t5181, t5183)
                            t5184 = inline10366
                            var t5185 bool = t5184 == 44
                            if t5185 {
                                var t5186 *ref_int_x = value__233.index
                                var t5187 *ref_int_x = value__233.index
                                var t5188 int
                                var inline10356 int = ref_get__Ref_3int(t5187)
                                t5188 = inline10356
                                var t5189 int = t5188 + 1
                                ref_set__Ref_3int(t5186, t5189)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5191 string
                                var inline10358 string = "expected array separator"
                                var inline10359 string = "" + inline10358
                                var inline10360 string = inline10359 + " at byte "
                                var inline10361 *ref_int_x = value__233.index
                                var inline10362 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10361)
                                var inline10363 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10362)
                                var inline10364 string = inline10360 + inline10363
                                t5191 = inline10364
                                var t5192 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5191,
                                }
                                return t5192
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t5193 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t5193
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5152
            }
        }
        var t5150 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5151 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5150,
        }
        return t5151
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5214 *ref_int_x = value__236.index
    var t5215 *ref_int_x = value__236.index
    var t5216 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5215)
    var t5217 int = t5216 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5214, t5217)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var t5218 [0]Tuple2_6string_26_goml_m_std_p_json_p_Value = [0]Tuple2_6string_26_goml_m_std_p_json_p_Value{}
    var result__237 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = func(values [0]Tuple2_6string_26_goml_m_std_p_json_p_Value) *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
        return &_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value{
            items: values[0:len(values)],
        }
    }(t5218)
    var t5297 *ref_int_x = value__236.index
    var t5298 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5297)
    var t5299 string = value__236.input
    var t5300 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5299)
    var t5301 bool = t5298 < t5300
    var jp5290 bool
    if t5301 {
        var t5302 string = value__236.input
        var t5303 *ref_int_x = value__236.index
        var t5304 int
        var inline10386 int = ref_get__Ref_3int(t5303)
        t5304 = inline10386
        var t5305 uint8
        var inline10384 uint8 = _goml_runtime_core_string_byte_get(t5302, t5304)
        t5305 = inline10384
        var t5306 bool = t5305 == 125
        jp5290 = t5306
    } else {
        jp5290 = false
    }
    if jp5290 {
        var t5291 *ref_int_x = value__236.index
        var t5292 *ref_int_x = value__236.index
        var t5293 int
        var inline10390 int = ref_get__Ref_3int(t5292)
        t5293 = inline10390
        var t5294 int = t5293 + 1
        ref_set__Ref_3int(t5291, t5294)
        var t5295 _goml_m_std_p_json_p_Value = Object{
            _0: result__237,
        }
        var t5296 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5295,
        }
        return t5296
    } else {
        Loop_loop5223:
        for {
            var t5224 *ref_int_x = value__236.index
            var t5225 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5224)
            var t5226 string = value__236.input
            var t5227 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5226)
            var t5228 bool = t5225 < t5227
            if t5228 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5230 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5230 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5278 *ref_int_x = value__236.index
                    var t5279 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5278)
                    var t5280 string = value__236.input
                    var t5281 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5280)
                    var t5282 bool = t5279 >= t5281
                    var jp5270 bool
                    if t5282 {
                        jp5270 = true
                    } else {
                        var t5283 string = value__236.input
                        var t5284 *ref_int_x = value__236.index
                        var t5285 int
                        var inline10394 int = ref_get__Ref_3int(t5284)
                        t5285 = inline10394
                        var t5286 uint8
                        var inline10392 uint8 = _goml_runtime_core_string_byte_get(t5283, t5285)
                        t5286 = inline10392
                        var t5287 bool = t5286 != 58
                        jp5270 = t5287
                    }
                    if jp5270 {
                        var t5271 string
                        var inline10396 string = "expected object colon"
                        var inline10397 string = "" + inline10396
                        var inline10398 string = inline10397 + " at byte "
                        var inline10399 *ref_int_x = value__236.index
                        var inline10400 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10399)
                        var inline10401 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10400)
                        var inline10402 string = inline10398 + inline10401
                        t5271 = inline10402
                        var t5272 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5271,
                        }
                        return t5272
                    } else {
                        var t5273 *ref_int_x = value__236.index
                        var t5274 *ref_int_x = value__236.index
                        var t5275 int
                        var inline10406 int = ref_get__Ref_3int(t5274)
                        t5275 = inline10406
                        var t5276 int = t5275 + 1
                        ref_set__Ref_3int(t5273, t5276)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5233 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5233 = x816
                            var t5234 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5230,
                                _1: jp5233,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__237, t5234)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5236 *ref_int_x = value__236.index
                            var t5237 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5236)
                            var t5238 string = value__236.input
                            var t5239 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5238)
                            var t5240 bool = t5237 >= t5239
                            if t5240 {
                                var t5241 string
                                var inline10408 string = "unterminated object"
                                var inline10409 string = "" + inline10408
                                var inline10410 string = inline10409 + " at byte "
                                var inline10411 *ref_int_x = value__236.index
                                var inline10412 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10411)
                                var inline10413 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10412)
                                var inline10414 string = inline10410 + inline10413
                                t5241 = inline10414
                                var t5242 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5241,
                                }
                                return t5242
                            } else {
                                var t5244 string = value__236.input
                                var t5245 *ref_int_x = value__236.index
                                var t5246 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5245)
                                var t5247 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5244, t5246)
                                var t5248 bool = t5247 == 125
                                if t5248 {
                                    var t5249 *ref_int_x = value__236.index
                                    var t5250 *ref_int_x = value__236.index
                                    var t5251 int
                                    var inline10418 int = ref_get__Ref_3int(t5250)
                                    t5251 = inline10418
                                    var t5252 int = t5251 + 1
                                    ref_set__Ref_3int(t5249, t5252)
                                    var t5253 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__237,
                                    }
                                    var t5254 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5253,
                                    }
                                    return t5254
                                } else {
                                    var t5256 string = value__236.input
                                    var t5257 *ref_int_x = value__236.index
                                    var t5258 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5257)
                                    var t5259 uint8
                                    var inline10432 uint8 = _goml_runtime_core_string_byte_get(t5256, t5258)
                                    t5259 = inline10432
                                    var t5260 bool = t5259 == 44
                                    if t5260 {
                                        var t5261 *ref_int_x = value__236.index
                                        var t5262 *ref_int_x = value__236.index
                                        var t5263 int
                                        var inline10422 int = ref_get__Ref_3int(t5262)
                                        t5263 = inline10422
                                        var t5264 int = t5263 + 1
                                        ref_set__Ref_3int(t5261, t5264)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5266 string
                                        var inline10424 string = "expected object separator"
                                        var inline10425 string = "" + inline10424
                                        var inline10426 string = inline10425 + " at byte "
                                        var inline10427 *ref_int_x = value__236.index
                                        var inline10428 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10427)
                                        var inline10429 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10428)
                                        var inline10430 string = inline10426 + inline10429
                                        t5266 = inline10430
                                        var t5267 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5266,
                                        }
                                        return t5267
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5268 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5268
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5288 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5288
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5223
            }
        }
        var t5221 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5222 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5221,
        }
        return t5222
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5311 *ref_int_x = value__240.index
    var t5312 int
    var inline10470 int = ref_get__Ref_3int(t5311)
    t5312 = inline10470
    var t5313 string = value__240.input
    var t5314 int
    var inline10468 int = _goml_runtime_core_string_len(t5313)
    t5314 = inline10468
    var t5315 bool = t5312 >= t5314
    if t5315 {
        var t5316 string
        var inline10434 string = "expected JSON value"
        var inline10435 string = "" + inline10434
        var inline10436 string = inline10435 + " at byte "
        var inline10437 *ref_int_x = value__240.index
        var inline10438 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10437)
        var inline10439 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10438)
        var inline10440 string = inline10436 + inline10439
        t5316 = inline10440
        var t5317 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5316,
        }
        return t5317
    } else {
        var t5318 string = value__240.input
        var t5319 *ref_int_x = value__240.index
        var t5320 int
        var inline10466 int = ref_get__Ref_3int(t5319)
        t5320 = inline10466
        var mtmp824 uint8
        var inline10464 uint8 = _goml_runtime_core_string_byte_get(t5318, t5320)
        mtmp824 = inline10464
        switch mtmp824 {
        case 123:
            var t5323 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5323
        case 91:
            var t5324 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5324
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5327 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5328 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5327,
                }
                return t5328
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5329 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5329
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5330 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5331 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5330)
            return t5331
        case 102:
            var t5332 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5333 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5332)
            return t5333
        case 110:
            var t5334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5334
        default:
            var t5342 bool = mtmp824 == 45
            var jp5338 bool
            if t5342 {
                jp5338 = true
            } else {
                var inline10442 bool = mtmp824 >= 48
                if inline10442 {
                    var inline10443 bool = mtmp824 <= 57
                    jp5338 = inline10443
                } else {
                    jp5338 = false
                }
            }
            if jp5338 {
                var inline10445 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10447 string
                switch inline10445.(type) {
                case Result__string__string_Ok:
                    var inline10450 string = inline10445.(Result__string__string_Ok)._0
                    inline10447 = inline10450
                    var inline10448 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10447,
                    }
                    var inline10449 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10448,
                    }
                    return inline10449
                case Result__string__string_Err:
                    var inline10452 string = inline10445.(Result__string__string_Err)._0
                    var inline10454 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10452,
                    }
                    return inline10454
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5340 string
                var inline10456 string = "unexpected JSON token"
                var inline10457 string = "" + inline10456
                var inline10458 string = inline10457 + " at byte "
                var inline10459 *ref_int_x = value__240.index
                var inline10460 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10459)
                var inline10461 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10460)
                var inline10462 string = inline10458 + inline10461
                t5340 = inline10462
                var t5341 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5340,
                }
                return t5341
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10484 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10485 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10484,
    }
    parser__245 = inline10485
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5347 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5347 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5350 *ref_int_x = parser__245.index
        var t5351 int
        var inline10482 int = ref_get__Ref_3int(t5350)
        t5351 = inline10482
        var t5352 int
        var inline10480 int = _goml_runtime_core_string_len(input__244)
        t5352 = inline10480
        var t5353 bool = t5351 == t5352
        if t5353 {
            var t5354 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5347,
            }
            return t5354
        } else {
            var t5355 string
            var inline10472 string = "trailing JSON data"
            var inline10473 string = "" + inline10472
            var inline10474 string = inline10473 + " at byte "
            var inline10475 *ref_int_x = parser__245.index
            var inline10476 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10475)
            var inline10477 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10476)
            var inline10478 string = inline10474 + inline10477
            t5355 = inline10478
            var t5356 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5355,
            }
            return t5356
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5357 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5357
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10518 rune = 34
    var inline10519 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10518)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10519)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10516 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10516
    Loop_loop5371:
    for {
        var t5372 bool = for_index833 < for_limit834
        if t5372 {
            var for_item835 int = for_index833
            var t5373 int = for_index833 + 1
            for_index833 = t5373
            var byte__252 uint8
            var inline10504 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10504
            var t5426 bool = byte__252 == 34
            var jp5424 bool
            if t5426 {
                jp5424 = true
            } else {
                var t5427 bool = byte__252 == 92
                jp5424 = t5427
            }
            var jp5421 bool
            if jp5424 {
                jp5421 = true
            } else {
                var t5425 bool = byte__252 == 8
                jp5421 = t5425
            }
            var jp5418 bool
            if jp5421 {
                jp5418 = true
            } else {
                var t5422 bool = byte__252 == 9
                jp5418 = t5422
            }
            var jp5415 bool
            if jp5418 {
                jp5415 = true
            } else {
                var t5419 bool = byte__252 == 10
                jp5415 = t5419
            }
            var jp5412 bool
            if jp5415 {
                jp5412 = true
            } else {
                var t5416 bool = byte__252 == 12
                jp5412 = t5416
            }
            var jp5409 bool
            if jp5412 {
                jp5409 = true
            } else {
                var t5413 bool = byte__252 == 13
                jp5409 = t5413
            }
            var jp5376 bool
            if jp5409 {
                jp5376 = true
            } else {
                var t5410 bool = byte__252 < 32
                jp5376 = t5410
            }
            if jp5376 {
                var t5405 bool = start__250 < for_item835
                if t5405 {
                    var t5406 string
                    var inline10490 string = string_byte_slice(value__249, start__250, for_item835)
                    t5406 = inline10490
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5406)
                } else {}
                var t5380 bool = byte__252 == 34
                if t5380 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5383 bool = byte__252 == 92
                    if t5383 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5386 bool = byte__252 == 8
                        if t5386 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5389 bool = byte__252 == 9
                            if t5389 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5392 bool = byte__252 == 10
                                if t5392 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5395 bool = byte__252 == 12
                                    if t5395 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5398 bool = byte__252 == 13
                                        if t5398 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5400 uint8 = byte__252 / 16
                                            var t5401 rune
                                            var inline10501 int = int(uint8(t5400))
                                            var inline10502 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10501)
                                            t5401 = inline10502
                                            var inline10498 string = _goml_m_inherent_i_char_i_char_i_to__string(t5401)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10498)
                                            var t5402_rhs uint8 = 16
                                            var t5402 uint8 = byte__252 % t5402_rhs
                                            var t5403 rune
                                            var inline10495 int = int(uint8(t5402))
                                            var inline10496 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10495)
                                            t5403 = inline10496
                                            var inline10492 string = _goml_m_inherent_i_char_i_char_i_to__string(t5403)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10492)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5379 int = for_item835 + 1
                start__250 = t5379
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5371
        }
    }
    var t5366 int
    var inline10514 int = _goml_runtime_core_string_len(value__249)
    t5366 = inline10514
    var t5367 bool = start__250 < t5366
    if t5367 {
        var t5368 int
        var inline10508 int = _goml_runtime_core_string_len(value__249)
        t5368 = inline10508
        var t5369 string
        var inline10506 string = string_byte_slice(value__249, start__250, t5368)
        t5369 = inline10506
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5369)
    } else {}
    var inline10510 rune = 34
    var inline10511 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10510)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10511)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10534 rune = 123
        var inline10535 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10534)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10535)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5432:
        for {
            var t5433 bool = for_index852 < for_limit851
            if t5433 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5434 int = for_index852 + 1
                for_index852 = t5434
                var t5440 bool = index__256 > 0
                if t5440 {
                    var inline10522 rune = 44
                    var inline10523 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10522)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10523)
                } else {}
                var t5436 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5436)
                var inline10526 rune = 58
                var inline10527 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10526)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10527)
                var t5437 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5437)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5438 int = compound_old859 + compound_value860
                index__256 = t5438
                continue
            } else {
                break Loop_loop5432
            }
        }
        var inline10530 rune = 125
        var inline10531 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10530)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10531)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10546 rune = 91
        var inline10547 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10546)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10547)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5444:
        for {
            var t5445 bool = for_index866 < for_limit865
            if t5445 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5446 int = for_index866 + 1
                for_index866 = t5446
                var t5450 bool = index__259 > 0
                if t5450 {
                    var inline10538 rune = 44
                    var inline10539 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10538)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10539)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5448 int = compound_old871 + compound_value872
                index__259 = t5448
                continue
            } else {
                break Loop_loop5444
            }
        }
        var inline10542 rune = 93
        var inline10543 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10542)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10543)
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
        var jp5455 string
        if x848 {
            jp5455 = "true"
        } else {
            jp5455 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5455)
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
    var inline10555 [0]uint8 = [0]uint8{}
    var inline10556 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(inline10555)
    var inline10557 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10556,
    }
    builder__265 = inline10557
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10550 *_goml_vec_uint8 = builder__265.values
    var inline10551 Tuple2_4bool_6string = string_from_utf8(inline10550)
    var inline10552 string = inline10551._1
    return inline10552
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5466:
        for {
            var t5467 bool = for_index883 < for_limit882
            if t5467 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5468 int = for_index883 + 1
                for_index883 = t5468
                var t5470 string = for_item884._0
                var t5471 bool = t5470 == name__267
                if t5471 {
                    var t5472 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5473 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value{
                        _tag: 1,
                        _v1_0: t5472,
                    }
                    return t5473
                } else {
                    continue
                }
            } else {
                break Loop_loop5466
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

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5483 int
    var inline10568 int = _goml_runtime_core_string_len(value__272)
    t5483 = inline10568
    var t5484 bool = t5483 == 0
    if t5484 {
        return Option__int{
            _tag: 0,
        }
    } else {
        var t5485 uint8
        var inline10565 int = 0
        var inline10566 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10565)
        t5485 = inline10566
        var negative__273 bool = t5485 == 45
        var jp5487 int
        if negative__273 {
            jp5487 = 1
        } else {
            jp5487 = 0
        }
        var index__274 int = jp5487
        var result__275 int = 0
        var t5508 int
        var inline10563 int = _goml_runtime_core_string_len(value__272)
        t5508 = inline10563
        var t5509 bool = index__274 == t5508
        if t5509 {
            return Option__int{
                _tag: 0,
            }
        } else {
            Loop_loop5494:
            for {
                var t5495 int
                var inline10561 int = _goml_runtime_core_string_len(value__272)
                t5495 = inline10561
                var t5496 bool = index__274 < t5495
                if t5496 {
                    var byte__276 uint8
                    var inline10559 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10559
                    var t5506 bool = byte__276 < 48
                    var jp5501 bool
                    if t5506 {
                        jp5501 = true
                    } else {
                        var t5507 bool = byte__276 > 57
                        jp5501 = t5507
                    }
                    if jp5501 {
                        return Option__int{
                            _tag: 0,
                        }
                    } else {
                        var t5502 int = result__275 * 10
                        var t5503 uint8 = byte__276 - 48
                        var t5504 int = int(uint8(t5503))
                        var t5505 int = t5502 + t5504
                        result__275 = t5505
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5498 int = compound_old895 + compound_value896
                        index__274 = t5498
                        continue
                    }
                } else {
                    break Loop_loop5494
                }
            }
            var jp5491 int
            if negative__273 {
                var t5493 int = 0 - result__275
                jp5491 = t5493
            } else {
                jp5491 = result__275
            }
            var t5492 Option__int = Option__int{
                _tag: 1,
                _v1_0: jp5491,
            }
            return t5492
        }
    }
}

func main0() struct{} {
    var mtmp408 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6286 _goml_m_std_p_json_p_Value
    switch mtmp408.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x409 _goml_m_std_p_json_p_Value = mtmp408.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6286 = x409
        var mtmp412 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "name")
        switch mtmp412._tag {
        case 0:
            var inline11006 string = "missing name"
            var inline11007 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11006)
            _goml_runtime_core_string_println(inline11007)
            var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "version")
            switch mtmp417._tag {
            case 0:
                var inline11021 string = "missing version"
                var inline11022 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11021)
                _goml_runtime_core_string_println(inline11022)
            case 1:
                var x418 _goml_m_std_p_json_p_Value = mtmp417._v1_0
                var mtmp419 Option__int
                switch x418.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline11032 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline11034 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11032)
                    mtmp419 = inline11034
                default:
                    mtmp419 = Option__int{
                        _tag: 0,
                    }
                }
                switch mtmp419._tag {
                case 0:
                    var inline11025 string = "invalid version"
                    var inline11026 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11025)
                    _goml_runtime_core_string_println(inline11026)
                case 1:
                    var x420 int = mtmp419._v1_0
                    var inline11029 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                    _goml_runtime_core_string_println(inline11029)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "stable")
            switch mtmp422._tag {
            case 0:
                var inline11036 string = "missing stable"
                var inline11037 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11036)
                _goml_runtime_core_string_println(inline11037)
                var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                println__T_string(t6290)
                return struct{}{}
            case 1:
                var x423 _goml_m_std_p_json_p_Value = mtmp422._v1_0
                var commute_field11586 bool
                switch x423.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline11047 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11586 = inline11047
                    var inline11044 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11586)
                    _goml_runtime_core_string_println(inline11044)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                default:
                    var inline11040 string = "invalid stable"
                    var inline11041 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11040)
                    _goml_runtime_core_string_println(inline11041)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case 1:
            var x413 _goml_m_std_p_json_p_Value = mtmp412._v1_0
            var commute_field11592 string
            switch x413.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline11017 string = x413.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11592 = inline11017
                var inline11014 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11592)
                _goml_runtime_core_string_println(inline11014)
                var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "version")
                switch mtmp417._tag {
                case 0:
                    var inline11021 string = "missing version"
                    var inline11022 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11021)
                    _goml_runtime_core_string_println(inline11022)
                case 1:
                    var x418 _goml_m_std_p_json_p_Value = mtmp417._v1_0
                    var mtmp419 Option__int
                    switch x418.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11032 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11034 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11032)
                        mtmp419 = inline11034
                    default:
                        mtmp419 = Option__int{
                            _tag: 0,
                        }
                    }
                    switch mtmp419._tag {
                    case 0:
                        var inline11025 string = "invalid version"
                        var inline11026 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11025)
                        _goml_runtime_core_string_println(inline11026)
                    case 1:
                        var x420 int = mtmp419._v1_0
                        var inline11029 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                        _goml_runtime_core_string_println(inline11029)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "stable")
                switch mtmp422._tag {
                case 0:
                    var inline11036 string = "missing stable"
                    var inline11037 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11036)
                    _goml_runtime_core_string_println(inline11037)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                case 1:
                    var x423 _goml_m_std_p_json_p_Value = mtmp422._v1_0
                    var commute_field11586 bool
                    switch x423.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11047 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11586 = inline11047
                        var inline11044 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11586)
                        _goml_runtime_core_string_println(inline11044)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    default:
                        var inline11040 string = "invalid stable"
                        var inline11041 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11040)
                        _goml_runtime_core_string_println(inline11041)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline11010 string = "invalid name"
                var inline11011 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11010)
                _goml_runtime_core_string_println(inline11011)
                var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "version")
                switch mtmp417._tag {
                case 0:
                    var inline11021 string = "missing version"
                    var inline11022 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11021)
                    _goml_runtime_core_string_println(inline11022)
                case 1:
                    var x418 _goml_m_std_p_json_p_Value = mtmp417._v1_0
                    var mtmp419 Option__int
                    switch x418.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11032 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11034 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11032)
                        mtmp419 = inline11034
                    default:
                        mtmp419 = Option__int{
                            _tag: 0,
                        }
                    }
                    switch mtmp419._tag {
                    case 0:
                        var inline11025 string = "invalid version"
                        var inline11026 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11025)
                        _goml_runtime_core_string_println(inline11026)
                    case 1:
                        var x420 int = mtmp419._v1_0
                        var inline11029 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                        _goml_runtime_core_string_println(inline11029)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "stable")
                switch mtmp422._tag {
                case 0:
                    var inline11036 string = "missing stable"
                    var inline11037 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11036)
                    _goml_runtime_core_string_println(inline11037)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                case 1:
                    var x423 _goml_m_std_p_json_p_Value = mtmp422._v1_0
                    var commute_field11586 bool
                    switch x423.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11047 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11586 = inline11047
                        var inline11044 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11586)
                        _goml_runtime_core_string_println(inline11044)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    default:
                        var inline11040 string = "invalid stable"
                        var inline11041 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11040)
                        _goml_runtime_core_string_println(inline11041)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x410 string = mtmp408.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline11003 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x410)
        _goml_runtime_core_string_println(inline11003)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6343:
    for {
        var t6344 int
        var inline11058 int = _goml_runtime_core_string_len(x12)
        t6344 = inline11058
        var t6345 bool = index__26 < t6344
        if t6345 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6347 int = compound_old17 + x16
                index__26 = t6347
                continue
            } else {
                var t6349 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6349
            }
        } else {
            break Loop_loop6343
        }
    }
    var t6342 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6342
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t6379 string = _goml_runtime_core_int_to_string(self__32)
    return t6379
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline11068 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline11069 bool = inline11068._0
    var inline11070 rune = inline11068._1
    if inline11069 {
        return inline11070
    } else {
        var inline11073 rune = _goml_runtime_core_string_get("", -1)
        return inline11073
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t6458 *ref_int_x = ref__Ref_3int(value__431)
    return t6458
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t6461 int = ref_get__Ref_3int(self__432)
    return t6461
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__34 rune) string {
    var inline11080 uint32 = uint32(rune(self__34))
    var inline11081 bool = utf8_valid_scalar(inline11080)
    if inline11081 {
        var inline11082 string = _goml_runtime_core_char_to_string(self__34)
        return inline11082
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t6520 int = _goml_runtime_core_string_len(self__36)
    return t6520
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t6523 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t6523
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline11380 bool = string_is_char_boundary(self__41, start__42)
    var inline11382 bool
    if inline11380 {
        var inline11385 bool = string_is_char_boundary(self__41, end__43)
        inline11382 = inline11385
    } else {
        inline11382 = false
    }
    if inline11382 {
        var inline11383 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline11383
    } else {
        var inline11384 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline11384
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline11391 bool = utf8_valid_scalar(value__2)
    if inline11391 {
        var inline11392 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11393 rune = inline11392._1
        var inline11395 Option__char = Option__char{
            _tag: 1,
            _v1_0: inline11393,
        }
        return inline11395
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
    var t6890 string
    t6890 = value__1
    _goml_runtime_core_string_println(t6890)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t7016 bool = index__6 < 0
    var jp7014 bool
    if t7016 {
        jp7014 = true
    } else {
        var t7017 bool = index__6 >= length__7
        jp7014 = t7017
    }
    if jp7014 {
        var inline11406 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11406
    } else {
        var t6901 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6901))
        var t6904 bool = first__8 < 128
        if t6904 {
            var inline11408 int = 1
            var inline11409 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11409._tag {
            case 0:
                var inline11410 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11410
            case 1:
                var inline11411 rune = inline11409._v1_0
                var inline11413 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11411,
                    _2: inline11408,
                }
                return inline11413
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6908 bool = first__8 < 194
            if t6908 {
                var inline11415 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11415
            } else {
                var t6912 bool = first__8 < 224
                if t6912 {
                    var t6925 int = length__7 - index__6
                    var t6926 bool = t6925 < 2
                    if t6926 {
                        var inline11417 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11417
                    } else {
                        var t6914 int = index__6 + 1
                        var t6915 uint8
                        var inline11431 uint8 = _goml_runtime_core_string_byte_get(value__5, t6914)
                        t6915 = inline11431
                        var second__9 uint32 = uint32(uint8(t6915))
                        var t6918 bool
                        var inline11428 bool = second__9 < 128
                        if inline11428 {
                            t6918 = true
                        } else {
                            var inline11429 bool = second__9 > 191
                            t6918 = inline11429
                        }
                        if t6918 {
                            var inline11419 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11419
                        } else {
                            var t6920_rhs uint32 = 31
                            var t6920 uint32 = first__8 & t6920_rhs
                            var t6921_rhs int = 6
                            var t6921 uint32 = t6920 << t6921_rhs
                            var t6922_rhs uint32 = 63
                            var t6922 uint32 = second__9 & t6922_rhs
                            var t6923 uint32 = t6921 | t6922
                            var inline11421 int = 2
                            var inline11422 Option__char = __goml_builtin_char_from_uint32(t6923)
                            switch inline11422._tag {
                            case 0:
                                var inline11423 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11423
                            case 1:
                                var inline11424 rune = inline11422._v1_0
                                var inline11426 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11424,
                                    _2: inline11421,
                                }
                                return inline11426
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6930 bool = first__8 < 240
                    if t6930 {
                        var t6963 int = length__7 - index__6
                        var t6964 bool = t6963 < 3
                        if t6964 {
                            var inline11433 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11433
                        } else {
                            var t6932 int = index__6 + 1
                            var t6933 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6932)
                            var second__10 uint32 = uint32(uint8(t6933))
                            var t6934 int = index__6 + 2
                            var t6935 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6934)
                            var third__11 uint32 = uint32(uint8(t6935))
                            var t6961 bool = utf8_invalid_continuation(second__10)
                            var jp6956 bool
                            if t6961 {
                                jp6956 = true
                            } else {
                                var inline11435 bool = third__11 < 128
                                if inline11435 {
                                    jp6956 = true
                                } else {
                                    var inline11436 bool = third__11 > 191
                                    jp6956 = inline11436
                                }
                            }
                            var jp6950 bool
                            if jp6956 {
                                jp6950 = true
                            } else {
                                var t6959 bool = first__8 == 224
                                if t6959 {
                                    var t6960 bool = second__10 < 160
                                    jp6950 = t6960
                                } else {
                                    jp6950 = false
                                }
                            }
                            var jp6939 bool
                            if jp6950 {
                                jp6939 = true
                            } else {
                                var t6953 bool = first__8 == 237
                                if t6953 {
                                    var t6954 bool = second__10 >= 160
                                    jp6939 = t6954
                                } else {
                                    jp6939 = false
                                }
                            }
                            if jp6939 {
                                var inline11438 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11438
                            } else {
                                var t6941_rhs uint32 = 15
                                var t6941 uint32 = first__8 & t6941_rhs
                                var t6942_rhs int = 12
                                var t6942 uint32 = t6941 << t6942_rhs
                                var t6943_rhs uint32 = 63
                                var t6943 uint32 = second__10 & t6943_rhs
                                var t6944_rhs int = 6
                                var t6944 uint32 = t6943 << t6944_rhs
                                var t6945 uint32 = t6942 | t6944
                                var t6946_rhs uint32 = 63
                                var t6946 uint32 = third__11 & t6946_rhs
                                var t6947 uint32 = t6945 | t6946
                                var inline11440 int = 3
                                var inline11441 Option__char = __goml_builtin_char_from_uint32(t6947)
                                switch inline11441._tag {
                                case 0:
                                    var inline11442 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11442
                                case 1:
                                    var inline11443 rune = inline11441._v1_0
                                    var inline11445 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11443,
                                        _2: inline11440,
                                    }
                                    return inline11445
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6968 bool = first__8 < 245
                        if t6968 {
                            var t7009 int = length__7 - index__6
                            var t7010 bool = t7009 < 4
                            if t7010 {
                                var t7011 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t7011
                            } else {
                                var t6970 int = index__6 + 1
                                var t6971 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6970)
                                var second__12 uint32 = uint32(uint8(t6971))
                                var t6972 int = index__6 + 2
                                var t6973 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6972)
                                var third__13 uint32 = uint32(uint8(t6973))
                                var t6974 int = index__6 + 3
                                var t6975 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6974)
                                var fourth__14 uint32 = uint32(uint8(t6975))
                                var t7007 bool = utf8_invalid_continuation(second__12)
                                var jp7005 bool
                                if t7007 {
                                    jp7005 = true
                                } else {
                                    var t7008 bool = utf8_invalid_continuation(third__13)
                                    jp7005 = t7008
                                }
                                var jp6999 bool
                                if jp7005 {
                                    jp6999 = true
                                } else {
                                    var t7006 bool = utf8_invalid_continuation(fourth__14)
                                    jp6999 = t7006
                                }
                                var jp6993 bool
                                if jp6999 {
                                    jp6993 = true
                                } else {
                                    var t7002 bool = first__8 == 240
                                    if t7002 {
                                        var t7003 bool = second__12 < 144
                                        jp6993 = t7003
                                    } else {
                                        jp6993 = false
                                    }
                                }
                                var jp6979 bool
                                if jp6993 {
                                    jp6979 = true
                                } else {
                                    var t6996 bool = first__8 == 244
                                    if t6996 {
                                        var t6997 bool = second__12 > 143
                                        jp6979 = t6997
                                    } else {
                                        jp6979 = false
                                    }
                                }
                                if jp6979 {
                                    var t6980 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6980
                                } else {
                                    var t6981_rhs uint32 = 7
                                    var t6981 uint32 = first__8 & t6981_rhs
                                    var t6982_rhs int = 18
                                    var t6982 uint32 = t6981 << t6982_rhs
                                    var t6983_rhs uint32 = 63
                                    var t6983 uint32 = second__12 & t6983_rhs
                                    var t6984_rhs int = 12
                                    var t6984 uint32 = t6983 << t6984_rhs
                                    var t6985 uint32 = t6982 | t6984
                                    var t6986_rhs uint32 = 63
                                    var t6986 uint32 = third__13 & t6986_rhs
                                    var t6987_rhs int = 6
                                    var t6987 uint32 = t6986 << t6987_rhs
                                    var t6988 uint32 = t6985 | t6987
                                    var t6989_rhs uint32 = 63
                                    var t6989 uint32 = fourth__14 & t6989_rhs
                                    var t6990 uint32 = t6988 | t6989
                                    var t6991 Tuple3_4bool_4char_3int = utf8_valid_decode(t6990, 4)
                                    return t6991
                                }
                            }
                        } else {
                            var t7012 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t7012
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t7037 uint32 = uint32(rune(value__29))
    var t7038 bool
    var inline11447 bool = t7037 <= 1114111
    if inline11447 {
        var inline11448 bool = t7037 >= 55296
        var inline11450 bool
        if inline11448 {
            var inline11452 bool = t7037 <= 57343
            inline11450 = inline11452
        } else {
            inline11450 = false
        }
        var inline11451 bool = !inline11450
        t7038 = inline11451
    } else {
        t7038 = false
    }
    if t7038 {
        var t7039 string = _goml_runtime_core_char_to_string(value__29)
        return t7039
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t7168 bool = string_is_char_boundary(value__21, start__22)
    var jp7165 bool
    if t7168 {
        var t7169 bool = string_is_char_boundary(value__21, end__23)
        jp7165 = t7169
    } else {
        jp7165 = false
    }
    if jp7165 {
        var t7166 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t7166
    } else {
        var t7167 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t7167
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t7176 bool
    var inline11480 bool = value__30 <= 1114111
    if inline11480 {
        var inline11481 bool = value__30 >= 55296
        var inline11483 bool
        if inline11481 {
            var inline11485 bool = value__30 <= 57343
            inline11483 = inline11485
        } else {
            inline11483 = false
        }
        var inline11484 bool = !inline11483
        t7176 = inline11484
    } else {
        t7176 = false
    }
    if t7176 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t7177 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t7177
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t7182 string = _goml_runtime_core_int_to_string(self__151)
    return t7182
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t7185 string = _goml_runtime_core_bool_to_string(self__148)
    return t7185
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t7188 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t7188
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11629 rune
    var inline11489 bool = utf8_valid_scalar(value__0)
    if inline11489 {
        var inline11490 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11491 rune = inline11490._1
        commute_field11629 = inline11491
        var t7194 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11629,
            _2: width__1,
        }
        return t7194
    } else {
        var inline11487 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11487
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t7199 bool = value__3 < 128
    if t7199 {
        return true
    } else {
        var t7200 bool = value__3 > 191
        return t7200
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t7205 bool = value__4 <= 1114111
    if t7205 {
        var t7209 bool = value__4 >= 55296
        var jp7207 bool
        if t7209 {
            var t7210 bool = value__4 <= 57343
            jp7207 = t7210
        } else {
            jp7207 = false
        }
        var t7208 bool = !jp7207
        return t7208
    } else {
        return false
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t7224 bool = index__16 < 0
    var jp7216 bool
    if t7224 {
        jp7216 = true
    } else {
        var t7225 int
        var inline11495 int = _goml_runtime_core_string_len(value__15)
        t7225 = inline11495
        var t7226 bool = index__16 > t7225
        jp7216 = t7226
    }
    if jp7216 {
        return false
    } else {
        var t7219 int
        var inline11499 int = _goml_runtime_core_string_len(value__15)
        t7219 = inline11499
        var t7220 bool = index__16 == t7219
        if t7220 {
            return true
        } else {
            var t7221 uint8
            var inline11497 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t7221 = inline11497
            var t7222_rhs uint8 = 192
            var t7222 uint8 = t7221 & t7222_rhs
            var t7223 bool = t7222 != 128
            return t7223
        }
    }
}

func main() {
    main0()
}
