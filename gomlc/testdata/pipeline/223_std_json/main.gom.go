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

type _goml_m_std_p_serde_p_FieldKey interface {
    is_goml_m_std_p_serde_p_FieldKey()
}

type Position struct {
    _0 int
}

func (_ Position) is_goml_m_std_p_serde_p_FieldKey() {}

type _goml_m_std_p_serde_p_FieldKey_Name struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_FieldKey_Name) is_goml_m_std_p_serde_p_FieldKey() {}

type _goml_m_std_p_serde_p_VariantKey interface {
    is_goml_m_std_p_serde_p_VariantKey()
}

type Index struct {
    _0 int
}

func (_ Index) is_goml_m_std_p_serde_p_VariantKey() {}

type _goml_m_std_p_serde_p_VariantKey_Name struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_VariantKey_Name) is_goml_m_std_p_serde_p_VariantKey() {}

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

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

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

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

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

type _goml_m_Result____std_p_bytes_p_Bytes____string interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____string()
}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Err) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__int__string interface {
    isResult__int__string()
}

type Result__int__string_Ok struct {
    _0 int
}

func (_ Result__int__string_Ok) isResult__int__string() {}

type Result__int__string_Err struct {
    _0 string
}

func (_ Result__int__string_Err) isResult__int__string() {}

type Result__uint__string interface {
    isResult__uint__string()
}

type Result__uint__string_Ok struct {
    _0 uint
}

func (_ Result__uint__string_Ok) isResult__uint__string() {}

type Result__uint__string_Err struct {
    _0 string
}

func (_ Result__uint__string_Err) isResult__uint__string() {}

type Result__float32__string interface {
    isResult__float32__string()
}

type Result__float32__string_Ok struct {
    _0 float32
}

func (_ Result__float32__string_Ok) isResult__float32__string() {}

type Result__float32__string_Err struct {
    _0 string
}

func (_ Result__float32__string_Err) isResult__float32__string() {}

type Result__float64__string interface {
    isResult__float64__string()
}

type Result__float64__string_Ok struct {
    _0 float64
}

func (_ Result__float64__string_Ok) isResult__float64__string() {}

type Result__float64__string_Err struct {
    _0 string
}

func (_ Result__float64__string_Err) isResult__float64__string() {}

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

type Option__int64 interface {
    isOption__int64()
}

type Option__int64_None struct {}

func (_ Option__int64_None) isOption__int64() {}

type Option__int64_Some struct {
    _0 int64
}

func (_ Option__int64_Some) isOption__int64() {}

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

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string interface {
    is_goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string()
}

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Ok struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Ok) is_goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string() {}

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Err) is_goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string() {}

type _goml_m_Option____std_p_serde_p_Value interface {
    is_goml_m_Option____std_p_serde_p_Value()
}

type _goml_m_Option____std_p_serde_p_Value_None struct {}

func (_ _goml_m_Option____std_p_serde_p_Value_None) is_goml_m_Option____std_p_serde_p_Value() {}

type _goml_m_Option____std_p_serde_p_Value_Some struct {
    _0 _goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Option____std_p_serde_p_Value_Some) is_goml_m_Option____std_p_serde_p_Value() {}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame interface {
    is_goml_m_Option____std_p_serde_p_ValueDeserializeFrame()
}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_None struct {}

func (_ _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_None) is_goml_m_Option____std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_Some struct {
    _0 _goml_m_std_p_serde_p_ValueDeserializeFrame
}

func (_ _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_Some) is_goml_m_Option____std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_Result____Vec_l_uint8_r_____string interface {
    is_goml_m_Result____Vec_l_uint8_r_____string()
}

type _goml_m_Result____Vec_l_uint8_r_____string_Ok struct {
    _0 *_goml_vec_uint8
}

func (_ _goml_m_Result____Vec_l_uint8_r_____string_Ok) is_goml_m_Result____Vec_l_uint8_r_____string() {}

type _goml_m_Result____Vec_l_uint8_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_uint8_r_____string_Err) is_goml_m_Result____Vec_l_uint8_r_____string() {}

type Result__bool__string interface {
    isResult__bool__string()
}

type Result__bool__string_Ok struct {
    _0 bool
}

func (_ Result__bool__string_Ok) isResult__bool__string() {}

type Result__bool__string_Err struct {
    _0 string
}

func (_ Result__bool__string_Err) isResult__bool__string() {}

type Result__int8__string interface {
    isResult__int8__string()
}

type Result__int8__string_Ok struct {
    _0 int8
}

func (_ Result__int8__string_Ok) isResult__int8__string() {}

type Result__int8__string_Err struct {
    _0 string
}

func (_ Result__int8__string_Err) isResult__int8__string() {}

type Result__int16__string interface {
    isResult__int16__string()
}

type Result__int16__string_Ok struct {
    _0 int16
}

func (_ Result__int16__string_Ok) isResult__int16__string() {}

type Result__int16__string_Err struct {
    _0 string
}

func (_ Result__int16__string_Err) isResult__int16__string() {}

type Result__int32__string interface {
    isResult__int32__string()
}

type Result__int32__string_Ok struct {
    _0 int32
}

func (_ Result__int32__string_Ok) isResult__int32__string() {}

type Result__int32__string_Err struct {
    _0 string
}

func (_ Result__int32__string_Err) isResult__int32__string() {}

type Result__int64__string interface {
    isResult__int64__string()
}

type Result__int64__string_Ok struct {
    _0 int64
}

func (_ Result__int64__string_Ok) isResult__int64__string() {}

type Result__int64__string_Err struct {
    _0 string
}

func (_ Result__int64__string_Err) isResult__int64__string() {}

type Result__uint8__string interface {
    isResult__uint8__string()
}

type Result__uint8__string_Ok struct {
    _0 uint8
}

func (_ Result__uint8__string_Ok) isResult__uint8__string() {}

type Result__uint8__string_Err struct {
    _0 string
}

func (_ Result__uint8__string_Err) isResult__uint8__string() {}

type Result__uint16__string interface {
    isResult__uint16__string()
}

type Result__uint16__string_Ok struct {
    _0 uint16
}

func (_ Result__uint16__string_Ok) isResult__uint16__string() {}

type Result__uint16__string_Err struct {
    _0 string
}

func (_ Result__uint16__string_Err) isResult__uint16__string() {}

type Result__uint32__string interface {
    isResult__uint32__string()
}

type Result__uint32__string_Ok struct {
    _0 uint32
}

func (_ Result__uint32__string_Ok) isResult__uint32__string() {}

type Result__uint32__string_Err struct {
    _0 string
}

func (_ Result__uint32__string_Err) isResult__uint32__string() {}

type Result__uint64__string interface {
    isResult__uint64__string()
}

type Result__uint64__string_Ok struct {
    _0 uint64
}

func (_ Result__uint64__string_Ok) isResult__uint64__string() {}

type Result__uint64__string_Err struct {
    _0 string
}

func (_ Result__uint64__string_Err) isResult__uint64__string() {}

type Result__char__string interface {
    isResult__char__string()
}

type Result__char__string_Ok struct {
    _0 rune
}

func (_ Result__char__string_Ok) isResult__char__string() {}

type Result__char__string_Err struct {
    _0 string
}

func (_ Result__char__string_Err) isResult__char__string() {}

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

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame interface {
    is_goml_m_Option____std_p_serde_p_ValueSerializeFrame()
}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame_None struct {}

func (_ _goml_m_Option____std_p_serde_p_ValueSerializeFrame_None) is_goml_m_Option____std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame_Some struct {
    _0 _goml_m_std_p_serde_p_ValueSerializeFrame
}

func (_ _goml_m_Option____std_p_serde_p_ValueSerializeFrame_Some) is_goml_m_Option____std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_Option_____o_int_c_char_q_ interface {
    is_goml_m_Option_____o_int_c_char_q_()
}

type _goml_m_Option_____o_int_c_char_q__None struct {}

func (_ _goml_m_Option_____o_int_c_char_q__None) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_int_c_char_q__Some struct {
    _0 Tuple2_3int_4char
}

func (_ _goml_m_Option_____o_int_c_char_q__Some) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame interface {
    is_goml_m_Option____std_p_json_p_JsonDeserializeFrame()
}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame_None struct {}

func (_ _goml_m_Option____std_p_json_p_JsonDeserializeFrame_None) is_goml_m_Option____std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame_Some struct {
    _0 _goml_m_std_p_json_p_JsonDeserializeFrame
}

func (_ _goml_m_Option____std_p_json_p_JsonDeserializeFrame_Some) is_goml_m_Option____std_p_json_p_JsonDeserializeFrame() {}

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

type _goml_m_Option_____o_char_c_int_q_ interface {
    is_goml_m_Option_____o_char_c_int_q_()
}

type _goml_m_Option_____o_char_c_int_q__None struct {}

func (_ _goml_m_Option_____o_char_c_int_q__None) is_goml_m_Option_____o_char_c_int_q_() {}

type _goml_m_Option_____o_char_c_int_q__Some struct {
    _0 Tuple2_4char_3int
}

func (_ _goml_m_Option_____o_char_c_int_q__Some) is_goml_m_Option_____o_char_c_int_q_() {}

type Option__uint32 interface {
    isOption__uint32()
}

type Option__uint32_None struct {}

func (_ Option__uint32_None) isOption__uint32() {}

type Option__uint32_Some struct {
    _0 uint32
}

func (_ Option__uint32_Some) isOption__uint32() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

type _goml_m_Option____std_p_json_p_Value interface {
    is_goml_m_Option____std_p_json_p_Value()
}

type _goml_m_Option____std_p_json_p_Value_None struct {}

func (_ _goml_m_Option____std_p_json_p_Value_None) is_goml_m_Option____std_p_json_p_Value() {}

type _goml_m_Option____std_p_json_p_Value_Some struct {
    _0 _goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option____std_p_json_p_Value_Some) is_goml_m_Option____std_p_json_p_Value() {}

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r_ interface {
    is_goml_m_Option____Vec_l_std_p_json_p_Value_r_()
}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r__None struct {}

func (_ _goml_m_Option____Vec_l_std_p_json_p_Value_r__None) is_goml_m_Option____Vec_l_std_p_json_p_Value_r_() {}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r__Some struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option____Vec_l_std_p_json_p_Value_r__Some) is_goml_m_Option____Vec_l_std_p_json_p_Value_r_() {}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame interface {
    is_goml_m_Option____std_p_json_p_JsonSerializeFrame()
}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame_None struct {}

func (_ _goml_m_Option____std_p_json_p_JsonSerializeFrame_None) is_goml_m_Option____std_p_json_p_JsonSerializeFrame() {}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame_Some struct {
    _0 _goml_m_std_p_json_p_JsonSerializeFrame
}

func (_ _goml_m_Option____std_p_json_p_JsonSerializeFrame_Some) is_goml_m_Option____std_p_json_p_JsonSerializeFrame() {}

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
    var inline8737 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8737
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
            var inline8733 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2931 = inline8733
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
    var inline8739 string = char_to_string(value__8)
    t2934 = inline8739
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2934)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4642 string = "" + message__201
    var t4643 string = t4642 + " at byte "
    var t4644 *ref_int_x = value__200.index
    var t4645 int
    var inline10074 int = ref_get__Ref_3int(t4644)
    t4645 = inline10074
    var t4646 string
    var inline10072 string = _goml_runtime_core_int_to_string(t4645)
    t4646 = inline10072
    var t4647 string = t4643 + t4646
    return t4647
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4662:
    for {
        var t4670 *ref_int_x = value__203.index
        var t4671 int
        var inline10095 int = ref_get__Ref_3int(t4670)
        t4671 = inline10095
        var t4672 string = value__203.input
        var t4673 int
        var inline10093 int = _goml_runtime_core_string_len(t4672)
        t4673 = inline10093
        var t4674 bool = t4671 < t4673
        var jp4664 bool
        if t4674 {
            var t4675 string = value__203.input
            var t4676 *ref_int_x = value__203.index
            var t4677 int
            var inline10087 int = ref_get__Ref_3int(t4676)
            t4677 = inline10087
            var t4678 uint8
            var inline10085 uint8 = _goml_runtime_core_string_byte_get(t4675, t4677)
            t4678 = inline10085
            var inline10076 bool = t4678 == 9
            var inline10078 bool
            if inline10076 {
                inline10078 = true
            } else {
                var inline10083 bool = t4678 == 10
                inline10078 = inline10083
            }
            var inline10080 bool
            if inline10078 {
                inline10080 = true
            } else {
                var inline10082 bool = t4678 == 13
                inline10080 = inline10082
            }
            if inline10080 {
                jp4664 = true
            } else {
                var inline10081 bool = t4678 == 32
                jp4664 = inline10081
            }
        } else {
            jp4664 = false
        }
        if jp4664 {
            var t4665 *ref_int_x = value__203.index
            var t4666 *ref_int_x = value__203.index
            var t4667 int
            var inline10091 int = ref_get__Ref_3int(t4666)
            t4667 = inline10091
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
        var t4688 Option__uint32 = Option__uint32_Some{
            _0: t4687,
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
            var t4696 Option__uint32 = Option__uint32_Some{
                _0: t4695,
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
                var t4704 Option__uint32 = Option__uint32_Some{
                    _0: t4703,
                }
                return t4704
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4715 *ref_int_x = value__205.index
    var t4716 int
    var inline10123 int = ref_get__Ref_3int(t4715)
    t4716 = inline10123
    var t4717 int = t4716 + 4
    var t4718 string = value__205.input
    var t4719 int
    var inline10121 int = _goml_runtime_core_string_len(t4718)
    t4719 = inline10121
    var t4720 bool = t4717 > t4719
    if t4720 {
        var t4721 string
        var inline10097 string = "incomplete unicode escape"
        var inline10098 string = "" + inline10097
        var inline10099 string = inline10098 + " at byte "
        var inline10100 *ref_int_x = value__205.index
        var inline10101 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10100)
        var inline10102 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10101)
        var inline10103 string = inline10099 + inline10102
        t4721 = inline10103
        var t4722 Result__uint32__string = Result__uint32__string_Err{
            _0: t4721,
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
                var inline10115 int = ref_get__Ref_3int(t4733)
                t4734 = inline10115
                var t4735 int = t4734 + for_item746
                var t4736 uint8
                var inline10113 uint8 = _goml_runtime_core_string_byte_get(t4732, t4735)
                t4736 = inline10113
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4736)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4738 string
                    var inline10105 string = "invalid unicode escape"
                    var inline10106 string = "" + inline10105
                    var inline10107 string = inline10106 + " at byte "
                    var inline10108 *ref_int_x = value__205.index
                    var inline10109 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10108)
                    var inline10110 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10109)
                    var inline10111 string = inline10107 + inline10110
                    t4738 = inline10111
                    var t4739 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4738,
                    }
                    return t4739
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
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
        var inline10119 int = ref_get__Ref_3int(t4725)
        t4726 = inline10119
        var t4727 int = t4726 + 4
        ref_set__Ref_3int(t4724, t4727)
        var t4728 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4728
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline10136 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline10136
    switch mtmp753.(type) {
    case Option__char_None:
        var t4746 string
        var inline10125 string = "invalid unicode codepoint"
        var inline10126 string = "" + inline10125
        var inline10127 string = inline10126 + " at byte "
        var inline10128 *ref_int_x = value__209.index
        var inline10129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10128)
        var inline10130 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10129)
        var inline10131 string = inline10127 + inline10130
        t4746 = inline10131
        var t4747 Result__unit__string = Result__unit__string_Err{
            _0: t4746,
        }
        return t4747
    case Option__char_Some:
        var x754 rune = mtmp753.(Option__char_Some)._0
        var inline10133 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10133)
        var t4748 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4748
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4752 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
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
            var inline10176 int = ref_get__Ref_3int(t4792)
            t4793 = inline10176
            var t4794 int = t4793 + 2
            var t4795 string = value__213.input
            var t4796 int
            var inline10174 int = _goml_runtime_core_string_len(t4795)
            t4796 = inline10174
            var t4797 bool = t4794 > t4796
            var jp4785 bool
            if t4797 {
                jp4785 = true
            } else {
                var t4798 string = value__213.input
                var t4799 *ref_int_x = value__213.index
                var t4800 int
                var inline10140 int = ref_get__Ref_3int(t4799)
                t4800 = inline10140
                var t4801 uint8
                var inline10138 uint8 = _goml_runtime_core_string_byte_get(t4798, t4800)
                t4801 = inline10138
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
                var inline10144 int = ref_get__Ref_3int(t4787)
                t4788 = inline10144
                var t4789 int = t4788 + 1
                var t4790 uint8
                var inline10142 uint8 = _goml_runtime_core_string_byte_get(t4786, t4789)
                t4790 = inline10142
                var t4791 bool = t4790 != 117
                jp4760 = t4791
            }
            if jp4760 {
                var t4761 string
                var inline10146 string = "missing low surrogate"
                var inline10147 string = "" + inline10146
                var inline10148 string = inline10147 + " at byte "
                var inline10149 *ref_int_x = value__213.index
                var inline10150 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10149)
                var inline10151 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10150)
                var inline10152 string = inline10148 + inline10151
                t4761 = inline10152
                var t4762 Result__unit__string = Result__unit__string_Err{
                    _0: t4761,
                }
                return t4762
            } else {
                var t4763 *ref_int_x = value__213.index
                var t4764 *ref_int_x = value__213.index
                var t4765 int
                var inline10172 int = ref_get__Ref_3int(t4764)
                t4765 = inline10172
                var t4766 int = t4765 + 2
                ref_set__Ref_3int(t4763, t4766)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4768 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
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
                        var inline10154 string = "invalid low surrogate"
                        var inline10155 string = "" + inline10154
                        var inline10156 string = inline10155 + " at byte "
                        var inline10157 *ref_int_x = value__213.index
                        var inline10158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10157)
                        var inline10159 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10158)
                        var inline10160 string = inline10156 + inline10159
                        t4773 = inline10160
                        var t4774 Result__unit__string = Result__unit__string_Err{
                            _0: t4773,
                        }
                        return t4774
                    } else {
                        var t4775 uint32 = jp4752 - 55296
                        var t4776 uint32 = t4775 * 1024
                        var t4777 uint32 = 65536 + t4776
                        var t4778 uint32 = t4777 + jp4768
                        var t4779 uint32 = t4778 - 56320
                        var inline10162 Option__char = char_from_uint32(t4779)
                        switch inline10162.(type) {
                        case Option__char_None:
                            var inline10163 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10164 Result__unit__string = Result__unit__string_Err{
                                _0: inline10163,
                            }
                            return inline10164
                        case Option__char_Some:
                            var inline10165 rune = inline10162.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10165)
                            var inline10168 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline10168
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4783 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
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
                var inline10178 string = "unexpected low surrogate"
                var inline10179 string = "" + inline10178
                var inline10180 string = inline10179 + " at byte "
                var inline10181 *ref_int_x = value__213.index
                var inline10182 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10181)
                var inline10183 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10182)
                var inline10184 string = inline10180 + inline10183
                t4807 = inline10184
                var t4808 Result__unit__string = Result__unit__string_Err{
                    _0: t4807,
                }
                return t4808
            } else {
                var t4809 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4752)
                return t4809
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4814 Result__unit__string = Result__unit__string_Err{
            _0: x758,
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
        var inline10188 int = ref_get__Ref_3int(t4936)
        t4937 = inline10188
        var t4938 uint8
        var inline10186 uint8 = _goml_runtime_core_string_byte_get(t4935, t4937)
        t4938 = inline10186
        var t4939 bool = t4938 != 34
        jp4922 = t4939
    }
    if jp4922 {
        var t4923 string
        var inline10190 string = "expected string"
        var inline10191 string = "" + inline10190
        var inline10192 string = inline10191 + " at byte "
        var inline10193 *ref_int_x = value__217.index
        var inline10194 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10193)
        var inline10195 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10194)
        var inline10196 string = inline10192 + inline10195
        t4923 = inline10196
        var t4924 Result__string__string = Result__string__string_Err{
            _0: t4923,
        }
        return t4924
    } else {
        var t4925 *ref_int_x = value__217.index
        var t4926 *ref_int_x = value__217.index
        var t4927 int
        var inline10200 int = ref_get__Ref_3int(t4926)
        t4927 = inline10200
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
                    var inline10215 int = ref_get__Ref_3int(t4840)
                    t4841 = inline10215
                    var t4842 bool = segment__219 < t4841
                    if t4842 {
                        var t4843 string = value__217.input
                        var t4844 *ref_int_x = value__217.index
                        var t4845 int
                        var inline10204 int = ref_get__Ref_3int(t4844)
                        t4845 = inline10204
                        var t4846 string
                        var inline10202 string = string_byte_slice(t4843, segment__219, t4845)
                        t4846 = inline10202
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4846)
                    } else {}
                    var t4834 *ref_int_x = value__217.index
                    var t4835 *ref_int_x = value__217.index
                    var t4836 int
                    var inline10213 int = ref_get__Ref_3int(t4835)
                    t4836 = inline10213
                    var t4837 int = t4836 + 1
                    ref_set__Ref_3int(t4834, t4837)
                    var t4838 string
                    var inline10206 *_goml_vec_uint8 = builder__218.values
                    var inline10207 Tuple2_4bool_6string = string_from_utf8(inline10206)
                    var inline10208 string = inline10207._1
                    t4838 = inline10208
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
                            var inline10219 int = ref_get__Ref_3int(t4908)
                            t4909 = inline10219
                            var t4910 string
                            var inline10217 string = string_byte_slice(t4907, segment__219, t4909)
                            t4910 = inline10217
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
                            var inline10221 string = "incomplete escape"
                            var inline10222 string = "" + inline10221
                            var inline10223 string = inline10222 + " at byte "
                            var inline10224 *ref_int_x = value__217.index
                            var inline10225 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10224)
                            var inline10226 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10225)
                            var inline10227 string = inline10223 + inline10226
                            t4902 = inline10227
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
                                var inline10229 rune = 34
                                var inline10230 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10229)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10230)
                                var t4864 *ref_int_x = value__217.index
                                var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                segment__219 = t4865
                                continue
                            } else {
                                var t4869 bool = escape__221 == 92
                                if t4869 {
                                    var inline10233 rune = 92
                                    var inline10234 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10233)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10234)
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
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4864 *ref_int_x = value__217.index
                                                var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                segment__219 = t4865
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
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
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4864 *ref_int_x = value__217.index
                                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                    segment__219 = t4865
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
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
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4864 *ref_int_x = value__217.index
                                                                    var t4865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4864)
                                                                    segment__219 = t4865
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
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
    var inline10254 int = ref_get__Ref_3int(t4948)
    start__226 = inline10254
    Loop_loop4953:
    for {
        var t4961 *ref_int_x = value__225.index
        var t4962 int
        var inline10250 int = ref_get__Ref_3int(t4961)
        t4962 = inline10250
        var t4963 string = value__225.input
        var t4964 int
        var inline10248 int = _goml_runtime_core_string_len(t4963)
        t4964 = inline10248
        var t4965 bool = t4962 < t4964
        var jp4955 bool
        if t4965 {
            var t4966 string = value__225.input
            var t4967 *ref_int_x = value__225.index
            var t4968 int
            var inline10242 int = ref_get__Ref_3int(t4967)
            t4968 = inline10242
            var t4969 uint8
            var inline10240 uint8 = _goml_runtime_core_string_byte_get(t4966, t4968)
            t4969 = inline10240
            var inline10237 bool = t4969 >= 48
            if inline10237 {
                var inline10238 bool = t4969 <= 57
                jp4955 = inline10238
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
            var inline10246 int = ref_get__Ref_3int(t4957)
            t4958 = inline10246
            var t4959 int = t4958 + 1
            ref_set__Ref_3int(t4956, t4959)
            continue
        } else {
            break Loop_loop4953
        }
    }
    var t4950 *ref_int_x = value__225.index
    var t4951 int
    var inline10252 int = ref_get__Ref_3int(t4950)
    t4951 = inline10252
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
        var inline10258 int = ref_get__Ref_3int(t5100)
        t5101 = inline10258
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
        var inline10260 string = "incomplete number"
        var inline10261 string = "" + inline10260
        var inline10262 string = inline10261 + " at byte "
        var inline10263 *ref_int_x = value__227.index
        var inline10264 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10263)
        var inline10265 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10264)
        var inline10266 string = inline10262 + inline10265
        t5062 = inline10266
        var t5063 Result__string__string = Result__string__string_Err{
            _0: t5062,
        }
        return t5063
    } else {
        var t5065 string = value__227.input
        var t5066 *ref_int_x = value__227.index
        var t5067 int
        var inline10301 int = ref_get__Ref_3int(t5066)
        t5067 = inline10301
        var t5068 uint8
        var inline10299 uint8 = _goml_runtime_core_string_byte_get(t5065, t5067)
        t5068 = inline10299
        var t5069 bool = t5068 == 48
        if t5069 {
            var t5070 *ref_int_x = value__227.index
            var t5071 *ref_int_x = value__227.index
            var t5072 int
            var inline10289 int = ref_get__Ref_3int(t5071)
            t5072 = inline10289
            var t5073 int = t5072 + 1
            ref_set__Ref_3int(t5070, t5073)
            var t5079 *ref_int_x = value__227.index
            var t5080 int
            var inline10285 int = ref_get__Ref_3int(t5079)
            t5080 = inline10285
            var t5081 string = value__227.input
            var t5082 int
            var inline10283 int = _goml_runtime_core_string_len(t5081)
            t5082 = inline10283
            var t5083 bool = t5080 < t5082
            var jp5076 bool
            if t5083 {
                var t5084 string = value__227.input
                var t5085 *ref_int_x = value__227.index
                var t5086 int
                var inline10273 int = ref_get__Ref_3int(t5085)
                t5086 = inline10273
                var t5087 uint8
                var inline10271 uint8 = _goml_runtime_core_string_byte_get(t5084, t5086)
                t5087 = inline10271
                var inline10268 bool = t5087 >= 48
                if inline10268 {
                    var inline10269 bool = t5087 <= 57
                    jp5076 = inline10269
                } else {
                    jp5076 = false
                }
            } else {
                jp5076 = false
            }
            if jp5076 {
                var t5077 string
                var inline10275 string = "invalid leading zero"
                var inline10276 string = "" + inline10275
                var inline10277 string = inline10276 + " at byte "
                var inline10278 *ref_int_x = value__227.index
                var inline10279 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10278)
                var inline10280 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10279)
                var inline10281 string = inline10277 + inline10280
                t5077 = inline10281
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
                    var inline10303 uint8 = _goml_runtime_core_string_byte_get(t5052, t5054)
                    t5055 = inline10303
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
                var inline10291 string = "expected number"
                var inline10292 string = "" + inline10291
                var inline10293 string = inline10292 + " at byte "
                var inline10294 *ref_int_x = value__227.index
                var inline10295 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10294)
                var inline10296 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10295)
                var inline10297 string = inline10293 + inline10296
                t5092 = inline10297
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
                    var inline10303 uint8 = _goml_runtime_core_string_byte_get(t5052, t5054)
                    t5055 = inline10303
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
    var inline10331 int = ref_get__Ref_3int(t5125)
    t5126 = inline10331
    var t5127 int
    var inline10329 int = _goml_runtime_core_string_len(expected__231)
    t5127 = inline10329
    var t5128 int = t5126 + t5127
    var t5129 string = value__230.input
    var t5130 int
    var inline10327 int = _goml_runtime_core_string_len(t5129)
    t5130 = inline10327
    var t5131 bool = t5128 <= t5130
    var jp5116 bool
    if t5131 {
        var t5132 string = value__230.input
        var t5133 *ref_int_x = value__230.index
        var t5134 int
        var inline10311 int = ref_get__Ref_3int(t5133)
        t5134 = inline10311
        var t5135 *ref_int_x = value__230.index
        var t5136 int
        var inline10309 int = ref_get__Ref_3int(t5135)
        t5136 = inline10309
        var t5137 int
        var inline10307 int = _goml_runtime_core_string_len(expected__231)
        t5137 = inline10307
        var t5138 int = t5136 + t5137
        var t5139 string
        var inline10305 string = string_byte_slice(t5132, t5134, t5138)
        t5139 = inline10305
        var t5140 bool = t5139 == expected__231
        jp5116 = t5140
    } else {
        jp5116 = false
    }
    if jp5116 {
        var t5117 *ref_int_x = value__230.index
        var t5118 *ref_int_x = value__230.index
        var t5119 int
        var inline10317 int = ref_get__Ref_3int(t5118)
        t5119 = inline10317
        var t5120 int
        var inline10315 int = _goml_runtime_core_string_len(expected__231)
        t5120 = inline10315
        var t5121 int = t5119 + t5120
        ref_set__Ref_3int(t5117, t5121)
        var t5122 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t5122
    } else {
        var t5123 string
        var inline10319 string = "invalid literal"
        var inline10320 string = "" + inline10319
        var inline10321 string = inline10320 + " at byte "
        var inline10322 *ref_int_x = value__230.index
        var inline10323 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10322)
        var inline10324 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10323)
        var inline10325 string = inline10321 + inline10324
        t5123 = inline10325
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
        var inline10335 int = ref_get__Ref_3int(t5208)
        t5209 = inline10335
        var t5210 uint8
        var inline10333 uint8 = _goml_runtime_core_string_byte_get(t5207, t5209)
        t5210 = inline10333
        var t5211 bool = t5210 == 93
        jp5195 = t5211
    } else {
        jp5195 = false
    }
    if jp5195 {
        var t5196 *ref_int_x = value__233.index
        var t5197 *ref_int_x = value__233.index
        var t5198 int
        var inline10339 int = ref_get__Ref_3int(t5197)
        t5198 = inline10339
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
            var inline10381 int = ref_get__Ref_3int(t5153)
            t5154 = inline10381
            var t5155 string = value__233.input
            var t5156 int
            var inline10379 int = _goml_runtime_core_string_len(t5155)
            t5156 = inline10379
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
                    var inline10375 int = ref_get__Ref_3int(t5161)
                    t5162 = inline10375
                    var t5163 string = value__233.input
                    var t5164 int
                    var inline10373 int = _goml_runtime_core_string_len(t5163)
                    t5164 = inline10373
                    var t5165 bool = t5162 >= t5164
                    if t5165 {
                        var t5166 string
                        var inline10341 string = "unterminated array"
                        var inline10342 string = "" + inline10341
                        var inline10343 string = inline10342 + " at byte "
                        var inline10344 *ref_int_x = value__233.index
                        var inline10345 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10344)
                        var inline10346 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10345)
                        var inline10347 string = inline10343 + inline10346
                        t5166 = inline10347
                        var t5167 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5166,
                        }
                        return t5167
                    } else {
                        var t5169 string = value__233.input
                        var t5170 *ref_int_x = value__233.index
                        var t5171 int
                        var inline10371 int = ref_get__Ref_3int(t5170)
                        t5171 = inline10371
                        var t5172 uint8
                        var inline10369 uint8 = _goml_runtime_core_string_byte_get(t5169, t5171)
                        t5172 = inline10369
                        var t5173 bool = t5172 == 93
                        if t5173 {
                            var t5174 *ref_int_x = value__233.index
                            var t5175 *ref_int_x = value__233.index
                            var t5176 int
                            var inline10351 int = ref_get__Ref_3int(t5175)
                            t5176 = inline10351
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
                            var inline10367 int = ref_get__Ref_3int(t5182)
                            t5183 = inline10367
                            var t5184 uint8
                            var inline10365 uint8 = _goml_runtime_core_string_byte_get(t5181, t5183)
                            t5184 = inline10365
                            var t5185 bool = t5184 == 44
                            if t5185 {
                                var t5186 *ref_int_x = value__233.index
                                var t5187 *ref_int_x = value__233.index
                                var t5188 int
                                var inline10355 int = ref_get__Ref_3int(t5187)
                                t5188 = inline10355
                                var t5189 int = t5188 + 1
                                ref_set__Ref_3int(t5186, t5189)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5191 string
                                var inline10357 string = "expected array separator"
                                var inline10358 string = "" + inline10357
                                var inline10359 string = inline10358 + " at byte "
                                var inline10360 *ref_int_x = value__233.index
                                var inline10361 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10360)
                                var inline10362 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10361)
                                var inline10363 string = inline10359 + inline10362
                                t5191 = inline10363
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
        var inline10385 int = ref_get__Ref_3int(t5303)
        t5304 = inline10385
        var t5305 uint8
        var inline10383 uint8 = _goml_runtime_core_string_byte_get(t5302, t5304)
        t5305 = inline10383
        var t5306 bool = t5305 == 125
        jp5290 = t5306
    } else {
        jp5290 = false
    }
    if jp5290 {
        var t5291 *ref_int_x = value__236.index
        var t5292 *ref_int_x = value__236.index
        var t5293 int
        var inline10389 int = ref_get__Ref_3int(t5292)
        t5293 = inline10389
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
                        var inline10393 int = ref_get__Ref_3int(t5284)
                        t5285 = inline10393
                        var t5286 uint8
                        var inline10391 uint8 = _goml_runtime_core_string_byte_get(t5283, t5285)
                        t5286 = inline10391
                        var t5287 bool = t5286 != 58
                        jp5270 = t5287
                    }
                    if jp5270 {
                        var t5271 string
                        var inline10395 string = "expected object colon"
                        var inline10396 string = "" + inline10395
                        var inline10397 string = inline10396 + " at byte "
                        var inline10398 *ref_int_x = value__236.index
                        var inline10399 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10398)
                        var inline10400 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10399)
                        var inline10401 string = inline10397 + inline10400
                        t5271 = inline10401
                        var t5272 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5271,
                        }
                        return t5272
                    } else {
                        var t5273 *ref_int_x = value__236.index
                        var t5274 *ref_int_x = value__236.index
                        var t5275 int
                        var inline10405 int = ref_get__Ref_3int(t5274)
                        t5275 = inline10405
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
                                var inline10407 string = "unterminated object"
                                var inline10408 string = "" + inline10407
                                var inline10409 string = inline10408 + " at byte "
                                var inline10410 *ref_int_x = value__236.index
                                var inline10411 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10410)
                                var inline10412 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10411)
                                var inline10413 string = inline10409 + inline10412
                                t5241 = inline10413
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
                                    var inline10417 int = ref_get__Ref_3int(t5250)
                                    t5251 = inline10417
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
                                    var inline10431 uint8 = _goml_runtime_core_string_byte_get(t5256, t5258)
                                    t5259 = inline10431
                                    var t5260 bool = t5259 == 44
                                    if t5260 {
                                        var t5261 *ref_int_x = value__236.index
                                        var t5262 *ref_int_x = value__236.index
                                        var t5263 int
                                        var inline10421 int = ref_get__Ref_3int(t5262)
                                        t5263 = inline10421
                                        var t5264 int = t5263 + 1
                                        ref_set__Ref_3int(t5261, t5264)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5266 string
                                        var inline10423 string = "expected object separator"
                                        var inline10424 string = "" + inline10423
                                        var inline10425 string = inline10424 + " at byte "
                                        var inline10426 *ref_int_x = value__236.index
                                        var inline10427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10426)
                                        var inline10428 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10427)
                                        var inline10429 string = inline10425 + inline10428
                                        t5266 = inline10429
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
    var inline10469 int = ref_get__Ref_3int(t5311)
    t5312 = inline10469
    var t5313 string = value__240.input
    var t5314 int
    var inline10467 int = _goml_runtime_core_string_len(t5313)
    t5314 = inline10467
    var t5315 bool = t5312 >= t5314
    if t5315 {
        var t5316 string
        var inline10433 string = "expected JSON value"
        var inline10434 string = "" + inline10433
        var inline10435 string = inline10434 + " at byte "
        var inline10436 *ref_int_x = value__240.index
        var inline10437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10436)
        var inline10438 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10437)
        var inline10439 string = inline10435 + inline10438
        t5316 = inline10439
        var t5317 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5316,
        }
        return t5317
    } else {
        var t5318 string = value__240.input
        var t5319 *ref_int_x = value__240.index
        var t5320 int
        var inline10465 int = ref_get__Ref_3int(t5319)
        t5320 = inline10465
        var mtmp824 uint8
        var inline10463 uint8 = _goml_runtime_core_string_byte_get(t5318, t5320)
        mtmp824 = inline10463
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
                var inline10441 bool = mtmp824 >= 48
                if inline10441 {
                    var inline10442 bool = mtmp824 <= 57
                    jp5338 = inline10442
                } else {
                    jp5338 = false
                }
            }
            if jp5338 {
                var inline10444 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10446 string
                switch inline10444.(type) {
                case Result__string__string_Ok:
                    var inline10449 string = inline10444.(Result__string__string_Ok)._0
                    inline10446 = inline10449
                    var inline10447 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10446,
                    }
                    var inline10448 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10447,
                    }
                    return inline10448
                case Result__string__string_Err:
                    var inline10451 string = inline10444.(Result__string__string_Err)._0
                    var inline10453 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10451,
                    }
                    return inline10453
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5340 string
                var inline10455 string = "unexpected JSON token"
                var inline10456 string = "" + inline10455
                var inline10457 string = inline10456 + " at byte "
                var inline10458 *ref_int_x = value__240.index
                var inline10459 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10458)
                var inline10460 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10459)
                var inline10461 string = inline10457 + inline10460
                t5340 = inline10461
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
    var inline10483 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10484 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10483,
    }
    parser__245 = inline10484
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5347 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5347 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5350 *ref_int_x = parser__245.index
        var t5351 int
        var inline10481 int = ref_get__Ref_3int(t5350)
        t5351 = inline10481
        var t5352 int
        var inline10479 int = _goml_runtime_core_string_len(input__244)
        t5352 = inline10479
        var t5353 bool = t5351 == t5352
        if t5353 {
            var t5354 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5347,
            }
            return t5354
        } else {
            var t5355 string
            var inline10471 string = "trailing JSON data"
            var inline10472 string = "" + inline10471
            var inline10473 string = inline10472 + " at byte "
            var inline10474 *ref_int_x = parser__245.index
            var inline10475 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10474)
            var inline10476 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10475)
            var inline10477 string = inline10473 + inline10476
            t5355 = inline10477
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
    var inline10517 rune = 34
    var inline10518 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10517)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10518)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10515 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10515
    Loop_loop5371:
    for {
        var t5372 bool = for_index833 < for_limit834
        if t5372 {
            var for_item835 int = for_index833
            var t5373 int = for_index833 + 1
            for_index833 = t5373
            var byte__252 uint8
            var inline10503 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10503
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
                    var inline10489 string = string_byte_slice(value__249, start__250, for_item835)
                    t5406 = inline10489
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
                                            var inline10500 int = int(uint8(t5400))
                                            var inline10501 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10500)
                                            t5401 = inline10501
                                            var inline10497 string = _goml_m_inherent_i_char_i_char_i_to__string(t5401)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10497)
                                            var t5402_rhs uint8 = 16
                                            var t5402 uint8 = byte__252 % t5402_rhs
                                            var t5403 rune
                                            var inline10494 int = int(uint8(t5402))
                                            var inline10495 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10494)
                                            t5403 = inline10495
                                            var inline10491 string = _goml_m_inherent_i_char_i_char_i_to__string(t5403)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10491)
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
    var inline10513 int = _goml_runtime_core_string_len(value__249)
    t5366 = inline10513
    var t5367 bool = start__250 < t5366
    if t5367 {
        var t5368 int
        var inline10507 int = _goml_runtime_core_string_len(value__249)
        t5368 = inline10507
        var t5369 string
        var inline10505 string = string_byte_slice(value__249, start__250, t5368)
        t5369 = inline10505
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5369)
    } else {}
    var inline10509 rune = 34
    var inline10510 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10509)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10510)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10533 rune = 123
        var inline10534 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10533)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10534)
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
                    var inline10521 rune = 44
                    var inline10522 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10521)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10522)
                } else {}
                var t5436 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5436)
                var inline10525 rune = 58
                var inline10526 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10525)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10526)
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
        var inline10529 rune = 125
        var inline10530 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10529)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10530)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10545 rune = 91
        var inline10546 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10545)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10546)
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
                    var inline10537 rune = 44
                    var inline10538 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10537)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10538)
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
        var inline10541 rune = 93
        var inline10542 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10541)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10542)
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
    var inline10554 [0]uint8 = [0]uint8{}
    var inline10555 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(inline10554)
    var inline10556 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10555,
    }
    builder__265 = inline10556
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10549 *_goml_vec_uint8 = builder__265.values
    var inline10550 Tuple2_4bool_6string = string_from_utf8(inline10549)
    var inline10551 string = inline10550._1
    return inline10551
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
                    var t5473 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5472,
                    }
                    return t5473
                } else {
                    continue
                }
            } else {
                break Loop_loop5466
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5483 int
    var inline10567 int = _goml_runtime_core_string_len(value__272)
    t5483 = inline10567
    var t5484 bool = t5483 == 0
    if t5484 {
        return Option__int_None{}
    } else {
        var t5485 uint8
        var inline10564 int = 0
        var inline10565 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10564)
        t5485 = inline10565
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
        var inline10562 int = _goml_runtime_core_string_len(value__272)
        t5508 = inline10562
        var t5509 bool = index__274 == t5508
        if t5509 {
            return Option__int_None{}
        } else {
            Loop_loop5494:
            for {
                var t5495 int
                var inline10560 int = _goml_runtime_core_string_len(value__272)
                t5495 = inline10560
                var t5496 bool = index__274 < t5495
                if t5496 {
                    var byte__276 uint8
                    var inline10558 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10558
                    var t5506 bool = byte__276 < 48
                    var jp5501 bool
                    if t5506 {
                        jp5501 = true
                    } else {
                        var t5507 bool = byte__276 > 57
                        jp5501 = t5507
                    }
                    if jp5501 {
                        return Option__int_None{}
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
            var t5492 Option__int = Option__int_Some{
                _0: jp5491,
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
        switch mtmp412.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline11005 string = "missing name"
            var inline11006 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11005)
            _goml_runtime_core_string_println(inline11006)
            var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "version")
            switch mtmp417.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline11020 string = "missing version"
                var inline11021 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11020)
                _goml_runtime_core_string_println(inline11021)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x418 _goml_m_std_p_json_p_Value = mtmp417.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp419 Option__int
                switch x418.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline11031 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline11033 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11031)
                    mtmp419 = inline11033
                default:
                    mtmp419 = Option__int_None{}
                }
                switch mtmp419.(type) {
                case Option__int_None:
                    var inline11024 string = "invalid version"
                    var inline11025 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11024)
                    _goml_runtime_core_string_println(inline11025)
                case Option__int_Some:
                    var x420 int = mtmp419.(Option__int_Some)._0
                    var inline11028 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                    _goml_runtime_core_string_println(inline11028)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "stable")
            switch mtmp422.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline11035 string = "missing stable"
                var inline11036 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11035)
                _goml_runtime_core_string_println(inline11036)
                var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                println__T_string(t6290)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x423 _goml_m_std_p_json_p_Value = mtmp422.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11581 bool
                switch x423.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline11046 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11581 = inline11046
                    var inline11043 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11581)
                    _goml_runtime_core_string_println(inline11043)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                default:
                    var inline11039 string = "invalid stable"
                    var inline11040 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11039)
                    _goml_runtime_core_string_println(inline11040)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x413 _goml_m_std_p_json_p_Value = mtmp412.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11587 string
            switch x413.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline11016 string = x413.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11587 = inline11016
                var inline11013 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11587)
                _goml_runtime_core_string_println(inline11013)
                var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "version")
                switch mtmp417.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11020 string = "missing version"
                    var inline11021 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11020)
                    _goml_runtime_core_string_println(inline11021)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x418 _goml_m_std_p_json_p_Value = mtmp417.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp419 Option__int
                    switch x418.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11031 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11033 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11031)
                        mtmp419 = inline11033
                    default:
                        mtmp419 = Option__int_None{}
                    }
                    switch mtmp419.(type) {
                    case Option__int_None:
                        var inline11024 string = "invalid version"
                        var inline11025 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11024)
                        _goml_runtime_core_string_println(inline11025)
                    case Option__int_Some:
                        var x420 int = mtmp419.(Option__int_Some)._0
                        var inline11028 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                        _goml_runtime_core_string_println(inline11028)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "stable")
                switch mtmp422.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11035 string = "missing stable"
                    var inline11036 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11035)
                    _goml_runtime_core_string_println(inline11036)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x423 _goml_m_std_p_json_p_Value = mtmp422.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11581 bool
                    switch x423.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11046 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11581 = inline11046
                        var inline11043 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11581)
                        _goml_runtime_core_string_println(inline11043)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    default:
                        var inline11039 string = "invalid stable"
                        var inline11040 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11039)
                        _goml_runtime_core_string_println(inline11040)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline11009 string = "invalid name"
                var inline11010 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11009)
                _goml_runtime_core_string_println(inline11010)
                var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "version")
                switch mtmp417.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11020 string = "missing version"
                    var inline11021 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11020)
                    _goml_runtime_core_string_println(inline11021)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x418 _goml_m_std_p_json_p_Value = mtmp417.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp419 Option__int
                    switch x418.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11031 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11033 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11031)
                        mtmp419 = inline11033
                    default:
                        mtmp419 = Option__int_None{}
                    }
                    switch mtmp419.(type) {
                    case Option__int_None:
                        var inline11024 string = "invalid version"
                        var inline11025 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11024)
                        _goml_runtime_core_string_println(inline11025)
                    case Option__int_Some:
                        var x420 int = mtmp419.(Option__int_Some)._0
                        var inline11028 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                        _goml_runtime_core_string_println(inline11028)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6286, "stable")
                switch mtmp422.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11035 string = "missing stable"
                    var inline11036 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11035)
                    _goml_runtime_core_string_println(inline11036)
                    var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                    println__T_string(t6290)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x423 _goml_m_std_p_json_p_Value = mtmp422.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11581 bool
                    switch x423.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11046 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11581 = inline11046
                        var inline11043 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11581)
                        _goml_runtime_core_string_println(inline11043)
                        var t6290 string = _goml_m_std_p_json_p_encode(jp6286)
                        println__T_string(t6290)
                        return struct{}{}
                    default:
                        var inline11039 string = "invalid stable"
                        var inline11040 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11039)
                        _goml_runtime_core_string_println(inline11040)
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
        var inline11002 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x410)
        _goml_runtime_core_string_println(inline11002)
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
        var inline11057 int = _goml_runtime_core_string_len(x12)
        t6344 = inline11057
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
    var inline11067 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline11068 bool = inline11067._0
    var inline11069 rune = inline11067._1
    if inline11068 {
        return inline11069
    } else {
        var inline11072 rune = _goml_runtime_core_string_get("", -1)
        return inline11072
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
    var inline11079 uint32 = uint32(rune(self__34))
    var inline11080 bool = utf8_valid_scalar(inline11079)
    if inline11080 {
        var inline11081 string = _goml_runtime_core_char_to_string(self__34)
        return inline11081
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
    var inline11375 bool = string_is_char_boundary(self__41, start__42)
    var inline11377 bool
    if inline11375 {
        var inline11380 bool = string_is_char_boundary(self__41, end__43)
        inline11377 = inline11380
    } else {
        inline11377 = false
    }
    if inline11377 {
        var inline11378 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline11378
    } else {
        var inline11379 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline11379
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline11386 bool = utf8_valid_scalar(value__2)
    if inline11386 {
        var inline11387 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11388 rune = inline11387._1
        var inline11390 Option__char = Option__char_Some{
            _0: inline11388,
        }
        return inline11390
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__258 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__259 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__258, elem__259)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6892 string
    t6892 = value__1
    _goml_runtime_core_string_println(t6892)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t7018 bool = index__6 < 0
    var jp7016 bool
    if t7018 {
        jp7016 = true
    } else {
        var t7019 bool = index__6 >= length__7
        jp7016 = t7019
    }
    if jp7016 {
        var inline11401 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11401
    } else {
        var t6903 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6903))
        var t6906 bool = first__8 < 128
        if t6906 {
            var inline11403 int = 1
            var inline11404 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11404.(type) {
            case Option__char_None:
                var inline11405 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11405
            case Option__char_Some:
                var inline11406 rune = inline11404.(Option__char_Some)._0
                var inline11408 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11406,
                    _2: inline11403,
                }
                return inline11408
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6910 bool = first__8 < 194
            if t6910 {
                var inline11410 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11410
            } else {
                var t6914 bool = first__8 < 224
                if t6914 {
                    var t6927 int = length__7 - index__6
                    var t6928 bool = t6927 < 2
                    if t6928 {
                        var inline11412 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11412
                    } else {
                        var t6916 int = index__6 + 1
                        var t6917 uint8
                        var inline11426 uint8 = _goml_runtime_core_string_byte_get(value__5, t6916)
                        t6917 = inline11426
                        var second__9 uint32 = uint32(uint8(t6917))
                        var t6920 bool
                        var inline11423 bool = second__9 < 128
                        if inline11423 {
                            t6920 = true
                        } else {
                            var inline11424 bool = second__9 > 191
                            t6920 = inline11424
                        }
                        if t6920 {
                            var inline11414 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11414
                        } else {
                            var t6922_rhs uint32 = 31
                            var t6922 uint32 = first__8 & t6922_rhs
                            var t6923_rhs int = 6
                            var t6923 uint32 = t6922 << t6923_rhs
                            var t6924_rhs uint32 = 63
                            var t6924 uint32 = second__9 & t6924_rhs
                            var t6925 uint32 = t6923 | t6924
                            var inline11416 int = 2
                            var inline11417 Option__char = __goml_builtin_char_from_uint32(t6925)
                            switch inline11417.(type) {
                            case Option__char_None:
                                var inline11418 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11418
                            case Option__char_Some:
                                var inline11419 rune = inline11417.(Option__char_Some)._0
                                var inline11421 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11419,
                                    _2: inline11416,
                                }
                                return inline11421
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6932 bool = first__8 < 240
                    if t6932 {
                        var t6965 int = length__7 - index__6
                        var t6966 bool = t6965 < 3
                        if t6966 {
                            var inline11428 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11428
                        } else {
                            var t6934 int = index__6 + 1
                            var t6935 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6934)
                            var second__10 uint32 = uint32(uint8(t6935))
                            var t6936 int = index__6 + 2
                            var t6937 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6936)
                            var third__11 uint32 = uint32(uint8(t6937))
                            var t6963 bool = utf8_invalid_continuation(second__10)
                            var jp6958 bool
                            if t6963 {
                                jp6958 = true
                            } else {
                                var inline11430 bool = third__11 < 128
                                if inline11430 {
                                    jp6958 = true
                                } else {
                                    var inline11431 bool = third__11 > 191
                                    jp6958 = inline11431
                                }
                            }
                            var jp6952 bool
                            if jp6958 {
                                jp6952 = true
                            } else {
                                var t6961 bool = first__8 == 224
                                if t6961 {
                                    var t6962 bool = second__10 < 160
                                    jp6952 = t6962
                                } else {
                                    jp6952 = false
                                }
                            }
                            var jp6941 bool
                            if jp6952 {
                                jp6941 = true
                            } else {
                                var t6955 bool = first__8 == 237
                                if t6955 {
                                    var t6956 bool = second__10 >= 160
                                    jp6941 = t6956
                                } else {
                                    jp6941 = false
                                }
                            }
                            if jp6941 {
                                var inline11433 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11433
                            } else {
                                var t6943_rhs uint32 = 15
                                var t6943 uint32 = first__8 & t6943_rhs
                                var t6944_rhs int = 12
                                var t6944 uint32 = t6943 << t6944_rhs
                                var t6945_rhs uint32 = 63
                                var t6945 uint32 = second__10 & t6945_rhs
                                var t6946_rhs int = 6
                                var t6946 uint32 = t6945 << t6946_rhs
                                var t6947 uint32 = t6944 | t6946
                                var t6948_rhs uint32 = 63
                                var t6948 uint32 = third__11 & t6948_rhs
                                var t6949 uint32 = t6947 | t6948
                                var inline11435 int = 3
                                var inline11436 Option__char = __goml_builtin_char_from_uint32(t6949)
                                switch inline11436.(type) {
                                case Option__char_None:
                                    var inline11437 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11437
                                case Option__char_Some:
                                    var inline11438 rune = inline11436.(Option__char_Some)._0
                                    var inline11440 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11438,
                                        _2: inline11435,
                                    }
                                    return inline11440
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6970 bool = first__8 < 245
                        if t6970 {
                            var t7011 int = length__7 - index__6
                            var t7012 bool = t7011 < 4
                            if t7012 {
                                var t7013 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t7013
                            } else {
                                var t6972 int = index__6 + 1
                                var t6973 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6972)
                                var second__12 uint32 = uint32(uint8(t6973))
                                var t6974 int = index__6 + 2
                                var t6975 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6974)
                                var third__13 uint32 = uint32(uint8(t6975))
                                var t6976 int = index__6 + 3
                                var t6977 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6976)
                                var fourth__14 uint32 = uint32(uint8(t6977))
                                var t7009 bool = utf8_invalid_continuation(second__12)
                                var jp7007 bool
                                if t7009 {
                                    jp7007 = true
                                } else {
                                    var t7010 bool = utf8_invalid_continuation(third__13)
                                    jp7007 = t7010
                                }
                                var jp7001 bool
                                if jp7007 {
                                    jp7001 = true
                                } else {
                                    var t7008 bool = utf8_invalid_continuation(fourth__14)
                                    jp7001 = t7008
                                }
                                var jp6995 bool
                                if jp7001 {
                                    jp6995 = true
                                } else {
                                    var t7004 bool = first__8 == 240
                                    if t7004 {
                                        var t7005 bool = second__12 < 144
                                        jp6995 = t7005
                                    } else {
                                        jp6995 = false
                                    }
                                }
                                var jp6981 bool
                                if jp6995 {
                                    jp6981 = true
                                } else {
                                    var t6998 bool = first__8 == 244
                                    if t6998 {
                                        var t6999 bool = second__12 > 143
                                        jp6981 = t6999
                                    } else {
                                        jp6981 = false
                                    }
                                }
                                if jp6981 {
                                    var t6982 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6982
                                } else {
                                    var t6983_rhs uint32 = 7
                                    var t6983 uint32 = first__8 & t6983_rhs
                                    var t6984_rhs int = 18
                                    var t6984 uint32 = t6983 << t6984_rhs
                                    var t6985_rhs uint32 = 63
                                    var t6985 uint32 = second__12 & t6985_rhs
                                    var t6986_rhs int = 12
                                    var t6986 uint32 = t6985 << t6986_rhs
                                    var t6987 uint32 = t6984 | t6986
                                    var t6988_rhs uint32 = 63
                                    var t6988 uint32 = third__13 & t6988_rhs
                                    var t6989_rhs int = 6
                                    var t6989 uint32 = t6988 << t6989_rhs
                                    var t6990 uint32 = t6987 | t6989
                                    var t6991_rhs uint32 = 63
                                    var t6991 uint32 = fourth__14 & t6991_rhs
                                    var t6992 uint32 = t6990 | t6991
                                    var t6993 Tuple3_4bool_4char_3int = utf8_valid_decode(t6992, 4)
                                    return t6993
                                }
                            }
                        } else {
                            var t7014 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t7014
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t7039 uint32 = uint32(rune(value__29))
    var t7040 bool
    var inline11442 bool = t7039 <= 1114111
    if inline11442 {
        var inline11443 bool = t7039 >= 55296
        var inline11445 bool
        if inline11443 {
            var inline11447 bool = t7039 <= 57343
            inline11445 = inline11447
        } else {
            inline11445 = false
        }
        var inline11446 bool = !inline11445
        t7040 = inline11446
    } else {
        t7040 = false
    }
    if t7040 {
        var t7041 string = _goml_runtime_core_char_to_string(value__29)
        return t7041
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t7167 bool = string_is_char_boundary(value__21, start__22)
    var jp7164 bool
    if t7167 {
        var t7168 bool = string_is_char_boundary(value__21, end__23)
        jp7164 = t7168
    } else {
        jp7164 = false
    }
    if jp7164 {
        var t7165 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t7165
    } else {
        var t7166 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t7166
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t7175 bool
    var inline11475 bool = value__30 <= 1114111
    if inline11475 {
        var inline11476 bool = value__30 >= 55296
        var inline11478 bool
        if inline11476 {
            var inline11480 bool = value__30 <= 57343
            inline11478 = inline11480
        } else {
            inline11478 = false
        }
        var inline11479 bool = !inline11478
        t7175 = inline11479
    } else {
        t7175 = false
    }
    if t7175 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t7176 Option__char = Option__char_Some{
            _0: x24,
        }
        return t7176
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t7181 string = _goml_runtime_core_int_to_string(self__151)
    return t7181
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t7184 string = _goml_runtime_core_bool_to_string(self__148)
    return t7184
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t7187 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t7187
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11624 rune
    var inline11484 bool = utf8_valid_scalar(value__0)
    if inline11484 {
        var inline11485 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11486 rune = inline11485._1
        commute_field11624 = inline11486
        var t7193 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11624,
            _2: width__1,
        }
        return t7193
    } else {
        var inline11482 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11482
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t7198 bool = value__3 < 128
    if t7198 {
        return true
    } else {
        var t7199 bool = value__3 > 191
        return t7199
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t7204 bool = value__4 <= 1114111
    if t7204 {
        var t7208 bool = value__4 >= 55296
        var jp7206 bool
        if t7208 {
            var t7209 bool = value__4 <= 57343
            jp7206 = t7209
        } else {
            jp7206 = false
        }
        var t7207 bool = !jp7206
        return t7207
    } else {
        return false
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t7223 bool = index__16 < 0
    var jp7215 bool
    if t7223 {
        jp7215 = true
    } else {
        var t7224 int
        var inline11490 int = _goml_runtime_core_string_len(value__15)
        t7224 = inline11490
        var t7225 bool = index__16 > t7224
        jp7215 = t7225
    }
    if jp7215 {
        return false
    } else {
        var t7218 int
        var inline11494 int = _goml_runtime_core_string_len(value__15)
        t7218 = inline11494
        var t7219 bool = index__16 == t7218
        if t7219 {
            return true
        } else {
            var t7220 uint8
            var inline11492 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t7220 = inline11492
            var t7221_rhs uint8 = 192
            var t7221 uint8 = t7220 & t7221_rhs
            var t7222 bool = t7221 != 128
            return t7222
        }
    }
}

func main() {
    main0()
}
