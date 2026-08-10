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

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
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

func vec_new___goml_m_Vec__16std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    return &_goml_vec__goml_m_std_p_json_p_Value{
        items: nil,
    }
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

func vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    return &_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value{
        items: nil,
    }
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

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
}

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
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

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

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
    var vec_literal__178 *_goml_vec_uint8
    var inline8401 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline8401
    var t2661 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2661
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8416 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8416
    var t2675 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2675, length__5)
    var for_index1 int = 0
    Loop_loop2677:
    for {
        var t2678 bool = for_index1 < length__5
        if t2678 {
            var for_item3 int = for_index1
            var t2679 int = for_index1 + 1
            for_index1 = t2679
            var t2680 *_goml_vec_uint8 = self__3.values
            var t2681 uint8
            var inline8412 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2681 = inline8412
            vec_push__Vec_5uint8(t2680, t2681)
            continue
        } else {
            break Loop_loop2677
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2684 string
    var inline8418 string = char_to_string(value__8)
    t2684 = inline8418
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2684)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4625 string = "" + message__201
    var t4626 string = t4625 + " at byte "
    var t4627 *ref_int_x = value__200.index
    var t4628 int
    var inline9937 int = ref_get__Ref_3int(t4627)
    t4628 = inline9937
    var t4629 string
    var inline9935 string = _goml_runtime_core_int_to_string(t4628)
    t4629 = inline9935
    var t4630 string = t4626 + t4629
    return t4630
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4645:
    for {
        var t4653 *ref_int_x = value__203.index
        var t4654 int
        var inline9958 int = ref_get__Ref_3int(t4653)
        t4654 = inline9958
        var t4655 string = value__203.input
        var t4656 int
        var inline9956 int = _goml_runtime_core_string_len(t4655)
        t4656 = inline9956
        var t4657 bool = t4654 < t4656
        var jp4647 bool
        if t4657 {
            var t4658 string = value__203.input
            var t4659 *ref_int_x = value__203.index
            var t4660 int
            var inline9950 int = ref_get__Ref_3int(t4659)
            t4660 = inline9950
            var t4661 uint8
            var inline9948 uint8 = _goml_runtime_core_string_byte_get(t4658, t4660)
            t4661 = inline9948
            var inline9939 bool = t4661 == 9
            var inline9941 bool
            if inline9939 {
                inline9941 = true
            } else {
                var inline9946 bool = t4661 == 10
                inline9941 = inline9946
            }
            var inline9943 bool
            if inline9941 {
                inline9943 = true
            } else {
                var inline9945 bool = t4661 == 13
                inline9943 = inline9945
            }
            if inline9943 {
                jp4647 = true
            } else {
                var inline9944 bool = t4661 == 32
                jp4647 = inline9944
            }
        } else {
            jp4647 = false
        }
        if jp4647 {
            var t4648 *ref_int_x = value__203.index
            var t4649 *ref_int_x = value__203.index
            var t4650 int
            var inline9954 int = ref_get__Ref_3int(t4649)
            t4650 = inline9954
            var t4651 int = t4650 + 1
            ref_set__Ref_3int(t4648, t4651)
            continue
        } else {
            break Loop_loop4645
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4692 bool = value__204 >= 48
    var jp4668 bool
    if t4692 {
        var t4693 bool = value__204 <= 57
        jp4668 = t4693
    } else {
        jp4668 = false
    }
    if jp4668 {
        var t4669 uint8 = value__204 - 48
        var t4670 uint32 = uint32(uint8(t4669))
        var t4671 Option__uint32 = Option__uint32_Some{
            _0: t4670,
        }
        return t4671
    } else {
        var t4690 bool = value__204 >= 65
        var jp4675 bool
        if t4690 {
            var t4691 bool = value__204 <= 70
            jp4675 = t4691
        } else {
            jp4675 = false
        }
        if jp4675 {
            var t4676 uint8 = value__204 - 65
            var t4677 uint8 = t4676 + 10
            var t4678 uint32 = uint32(uint8(t4677))
            var t4679 Option__uint32 = Option__uint32_Some{
                _0: t4678,
            }
            return t4679
        } else {
            var t4688 bool = value__204 >= 97
            var jp4683 bool
            if t4688 {
                var t4689 bool = value__204 <= 102
                jp4683 = t4689
            } else {
                jp4683 = false
            }
            if jp4683 {
                var t4684 uint8 = value__204 - 97
                var t4685 uint8 = t4684 + 10
                var t4686 uint32 = uint32(uint8(t4685))
                var t4687 Option__uint32 = Option__uint32_Some{
                    _0: t4686,
                }
                return t4687
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4698 *ref_int_x = value__205.index
    var t4699 int
    var inline9986 int = ref_get__Ref_3int(t4698)
    t4699 = inline9986
    var t4700 int = t4699 + 4
    var t4701 string = value__205.input
    var t4702 int
    var inline9984 int = _goml_runtime_core_string_len(t4701)
    t4702 = inline9984
    var t4703 bool = t4700 > t4702
    if t4703 {
        var t4704 string
        var inline9960 string = "incomplete unicode escape"
        var inline9961 string = "" + inline9960
        var inline9962 string = inline9961 + " at byte "
        var inline9963 *ref_int_x = value__205.index
        var inline9964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9963)
        var inline9965 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9964)
        var inline9966 string = inline9962 + inline9965
        t4704 = inline9966
        var t4705 Result__uint32__string = Result__uint32__string_Err{
            _0: t4704,
        }
        return t4705
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4712:
        for {
            var t4713 bool = for_index744 < for_limit745
            if t4713 {
                var for_item746 int = for_index744
                var t4714 int = for_index744 + 1
                for_index744 = t4714
                var t4715 string = value__205.input
                var t4716 *ref_int_x = value__205.index
                var t4717 int
                var inline9978 int = ref_get__Ref_3int(t4716)
                t4717 = inline9978
                var t4718 int = t4717 + for_item746
                var t4719 uint8
                var inline9976 uint8 = _goml_runtime_core_string_byte_get(t4715, t4718)
                t4719 = inline9976
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4719)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4721 string
                    var inline9968 string = "invalid unicode escape"
                    var inline9969 string = "" + inline9968
                    var inline9970 string = inline9969 + " at byte "
                    var inline9971 *ref_int_x = value__205.index
                    var inline9972 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9971)
                    var inline9973 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9972)
                    var inline9974 string = inline9970 + inline9973
                    t4721 = inline9974
                    var t4722 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4721,
                    }
                    return t4722
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4723 uint32 = result__206 * 16
                    var t4724 uint32 = t4723 + x749
                    result__206 = t4724
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4712
            }
        }
        var t4707 *ref_int_x = value__205.index
        var t4708 *ref_int_x = value__205.index
        var t4709 int
        var inline9982 int = ref_get__Ref_3int(t4708)
        t4709 = inline9982
        var t4710 int = t4709 + 4
        ref_set__Ref_3int(t4707, t4710)
        var t4711 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4711
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline9999 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline9999
    switch mtmp753.(type) {
    case Option__char_None:
        var t4729 string
        var inline9988 string = "invalid unicode codepoint"
        var inline9989 string = "" + inline9988
        var inline9990 string = inline9989 + " at byte "
        var inline9991 *ref_int_x = value__209.index
        var inline9992 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9991)
        var inline9993 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9992)
        var inline9994 string = inline9990 + inline9993
        t4729 = inline9994
        var t4730 Result__unit__string = Result__unit__string_Err{
            _0: t4729,
        }
        return t4730
    case Option__char_Some:
        var x754 rune = mtmp753.(Option__char_Some)._0
        var inline9996 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline9996)
        var t4731 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4731
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4735 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4735 = x757
        var t4795 bool = jp4735 >= 55296
        var jp4739 bool
        if t4795 {
            var t4796 bool = jp4735 <= 56319
            jp4739 = t4796
        } else {
            jp4739 = false
        }
        if jp4739 {
            var t4775 *ref_int_x = value__213.index
            var t4776 int
            var inline10039 int = ref_get__Ref_3int(t4775)
            t4776 = inline10039
            var t4777 int = t4776 + 2
            var t4778 string = value__213.input
            var t4779 int
            var inline10037 int = _goml_runtime_core_string_len(t4778)
            t4779 = inline10037
            var t4780 bool = t4777 > t4779
            var jp4768 bool
            if t4780 {
                jp4768 = true
            } else {
                var t4781 string = value__213.input
                var t4782 *ref_int_x = value__213.index
                var t4783 int
                var inline10003 int = ref_get__Ref_3int(t4782)
                t4783 = inline10003
                var t4784 uint8
                var inline10001 uint8 = _goml_runtime_core_string_byte_get(t4781, t4783)
                t4784 = inline10001
                var t4785 bool = t4784 != 92
                jp4768 = t4785
            }
            var jp4743 bool
            if jp4768 {
                jp4743 = true
            } else {
                var t4769 string = value__213.input
                var t4770 *ref_int_x = value__213.index
                var t4771 int
                var inline10007 int = ref_get__Ref_3int(t4770)
                t4771 = inline10007
                var t4772 int = t4771 + 1
                var t4773 uint8
                var inline10005 uint8 = _goml_runtime_core_string_byte_get(t4769, t4772)
                t4773 = inline10005
                var t4774 bool = t4773 != 117
                jp4743 = t4774
            }
            if jp4743 {
                var t4744 string
                var inline10009 string = "missing low surrogate"
                var inline10010 string = "" + inline10009
                var inline10011 string = inline10010 + " at byte "
                var inline10012 *ref_int_x = value__213.index
                var inline10013 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10012)
                var inline10014 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10013)
                var inline10015 string = inline10011 + inline10014
                t4744 = inline10015
                var t4745 Result__unit__string = Result__unit__string_Err{
                    _0: t4744,
                }
                return t4745
            } else {
                var t4746 *ref_int_x = value__213.index
                var t4747 *ref_int_x = value__213.index
                var t4748 int
                var inline10035 int = ref_get__Ref_3int(t4747)
                t4748 = inline10035
                var t4749 int = t4748 + 2
                ref_set__Ref_3int(t4746, t4749)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4751 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4751 = x761
                    var t4764 bool = jp4751 < 56320
                    var jp4755 bool
                    if t4764 {
                        jp4755 = true
                    } else {
                        var t4765 bool = jp4751 > 57343
                        jp4755 = t4765
                    }
                    if jp4755 {
                        var t4756 string
                        var inline10017 string = "invalid low surrogate"
                        var inline10018 string = "" + inline10017
                        var inline10019 string = inline10018 + " at byte "
                        var inline10020 *ref_int_x = value__213.index
                        var inline10021 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10020)
                        var inline10022 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10021)
                        var inline10023 string = inline10019 + inline10022
                        t4756 = inline10023
                        var t4757 Result__unit__string = Result__unit__string_Err{
                            _0: t4756,
                        }
                        return t4757
                    } else {
                        var t4758 uint32 = jp4735 - 55296
                        var t4759 uint32 = t4758 * 1024
                        var t4760 uint32 = 65536 + t4759
                        var t4761 uint32 = t4760 + jp4751
                        var t4762 uint32 = t4761 - 56320
                        var inline10025 Option__char = char_from_uint32(t4762)
                        switch inline10025.(type) {
                        case Option__char_None:
                            var inline10026 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10027 Result__unit__string = Result__unit__string_Err{
                                _0: inline10026,
                            }
                            return inline10027
                        case Option__char_Some:
                            var inline10028 rune = inline10025.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10028)
                            var inline10031 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline10031
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4766 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4766
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4793 bool = jp4735 >= 56320
            var jp4789 bool
            if t4793 {
                var t4794 bool = jp4735 <= 57343
                jp4789 = t4794
            } else {
                jp4789 = false
            }
            if jp4789 {
                var t4790 string
                var inline10041 string = "unexpected low surrogate"
                var inline10042 string = "" + inline10041
                var inline10043 string = inline10042 + " at byte "
                var inline10044 *ref_int_x = value__213.index
                var inline10045 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10044)
                var inline10046 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10045)
                var inline10047 string = inline10043 + inline10046
                t4790 = inline10047
                var t4791 Result__unit__string = Result__unit__string_Err{
                    _0: t4790,
                }
                return t4791
            } else {
                var t4792 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4735)
                return t4792
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4797 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4797
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4913 *ref_int_x = value__217.index
    var t4914 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4913)
    var t4915 string = value__217.input
    var t4916 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4915)
    var t4917 bool = t4914 >= t4916
    var jp4905 bool
    if t4917 {
        jp4905 = true
    } else {
        var t4918 string = value__217.input
        var t4919 *ref_int_x = value__217.index
        var t4920 int
        var inline10051 int = ref_get__Ref_3int(t4919)
        t4920 = inline10051
        var t4921 uint8
        var inline10049 uint8 = _goml_runtime_core_string_byte_get(t4918, t4920)
        t4921 = inline10049
        var t4922 bool = t4921 != 34
        jp4905 = t4922
    }
    if jp4905 {
        var t4906 string
        var inline10053 string = "expected string"
        var inline10054 string = "" + inline10053
        var inline10055 string = inline10054 + " at byte "
        var inline10056 *ref_int_x = value__217.index
        var inline10057 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10056)
        var inline10058 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10057)
        var inline10059 string = inline10055 + inline10058
        t4906 = inline10059
        var t4907 Result__string__string = Result__string__string_Err{
            _0: t4906,
        }
        return t4907
    } else {
        var t4908 *ref_int_x = value__217.index
        var t4909 *ref_int_x = value__217.index
        var t4910 int
        var inline10063 int = ref_get__Ref_3int(t4909)
        t4910 = inline10063
        var t4911 int = t4910 + 1
        ref_set__Ref_3int(t4908, t4911)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4801 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4801)
        Loop_loop4805:
        for {
            var t4806 *ref_int_x = value__217.index
            var t4807 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4806)
            var t4808 string = value__217.input
            var t4809 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4808)
            var t4810 bool = t4807 < t4809
            if t4810 {
                var t4811 string = value__217.input
                var t4812 *ref_int_x = value__217.index
                var t4813 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4812)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4811, t4813)
                var t4815 bool = byte__220 == 34
                if t4815 {
                    var t4823 *ref_int_x = value__217.index
                    var t4824 int
                    var inline10078 int = ref_get__Ref_3int(t4823)
                    t4824 = inline10078
                    var t4825 bool = segment__219 < t4824
                    if t4825 {
                        var t4826 string = value__217.input
                        var t4827 *ref_int_x = value__217.index
                        var t4828 int
                        var inline10067 int = ref_get__Ref_3int(t4827)
                        t4828 = inline10067
                        var t4829 string
                        var inline10065 string = string_byte_slice(t4826, segment__219, t4828)
                        t4829 = inline10065
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4829)
                    } else {}
                    var t4817 *ref_int_x = value__217.index
                    var t4818 *ref_int_x = value__217.index
                    var t4819 int
                    var inline10076 int = ref_get__Ref_3int(t4818)
                    t4819 = inline10076
                    var t4820 int = t4819 + 1
                    ref_set__Ref_3int(t4817, t4820)
                    var t4821 string
                    var inline10069 *_goml_vec_uint8 = builder__218.values
                    var inline10070 Tuple2_4bool_6string = string_from_utf8(inline10069)
                    var inline10071 string = inline10070._1
                    t4821 = inline10071
                    var t4822 Result__string__string = Result__string__string_Ok{
                        _0: t4821,
                    }
                    return t4822
                } else {
                    var t4832 bool = byte__220 == 92
                    if t4832 {
                        var t4887 *ref_int_x = value__217.index
                        var t4888 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4887)
                        var t4889 bool = segment__219 < t4888
                        if t4889 {
                            var t4890 string = value__217.input
                            var t4891 *ref_int_x = value__217.index
                            var t4892 int
                            var inline10082 int = ref_get__Ref_3int(t4891)
                            t4892 = inline10082
                            var t4893 string
                            var inline10080 string = string_byte_slice(t4890, segment__219, t4892)
                            t4893 = inline10080
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4893)
                        } else {}
                        var t4834 *ref_int_x = value__217.index
                        var t4835 *ref_int_x = value__217.index
                        var t4836 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4835)
                        var t4837 int = t4836 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4834, t4837)
                        var t4880 *ref_int_x = value__217.index
                        var t4881 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4880)
                        var t4882 string = value__217.input
                        var t4883 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4882)
                        var t4884 bool = t4881 >= t4883
                        if t4884 {
                            var t4885 string
                            var inline10084 string = "incomplete escape"
                            var inline10085 string = "" + inline10084
                            var inline10086 string = inline10085 + " at byte "
                            var inline10087 *ref_int_x = value__217.index
                            var inline10088 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10087)
                            var inline10089 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10088)
                            var inline10090 string = inline10086 + inline10089
                            t4885 = inline10090
                            var t4886 Result__string__string = Result__string__string_Err{
                                _0: t4885,
                            }
                            return t4886
                        } else {
                            var t4839 string = value__217.input
                            var t4840 *ref_int_x = value__217.index
                            var t4841 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4840)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4839, t4841)
                            var t4842 *ref_int_x = value__217.index
                            var t4843 *ref_int_x = value__217.index
                            var t4844 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4843)
                            var t4845 int = t4844 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4842, t4845)
                            var t4849 bool = escape__221 == 34
                            if t4849 {
                                var inline10092 rune = 34
                                var inline10093 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10092)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10093)
                                var t4847 *ref_int_x = value__217.index
                                var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                segment__219 = t4848
                                continue
                            } else {
                                var t4852 bool = escape__221 == 92
                                if t4852 {
                                    var inline10096 rune = 92
                                    var inline10097 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10096)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10097)
                                    var t4847 *ref_int_x = value__217.index
                                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                    segment__219 = t4848
                                    continue
                                } else {
                                    var t4855 bool = escape__221 == 47
                                    if t4855 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4847 *ref_int_x = value__217.index
                                        var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                        segment__219 = t4848
                                        continue
                                    } else {
                                        var t4858 bool = escape__221 == 98
                                        if t4858 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4847 *ref_int_x = value__217.index
                                                var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                segment__219 = t4848
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4847 *ref_int_x = value__217.index
                                                var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                segment__219 = t4848
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4862 bool = escape__221 == 102
                                            if t4862 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4847 *ref_int_x = value__217.index
                                                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                    segment__219 = t4848
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4847 *ref_int_x = value__217.index
                                                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                    segment__219 = t4848
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4866 bool = escape__221 == 110
                                                if t4866 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4847 *ref_int_x = value__217.index
                                                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                    segment__219 = t4848
                                                    continue
                                                } else {
                                                    var t4869 bool = escape__221 == 114
                                                    if t4869 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4847 *ref_int_x = value__217.index
                                                        var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                        segment__219 = t4848
                                                        continue
                                                    } else {
                                                        var t4872 bool = escape__221 == 116
                                                        if t4872 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4847 *ref_int_x = value__217.index
                                                            var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                            segment__219 = t4848
                                                            continue
                                                        } else {
                                                            var t4875 bool = escape__221 == 117
                                                            if t4875 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4847 *ref_int_x = value__217.index
                                                                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                                                                    segment__219 = t4848
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4877 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4877
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4878 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4879 Result__string__string = Result__string__string_Err{
                                                                    _0: t4878,
                                                                }
                                                                return t4879
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
                        var t4896 bool = byte__220 < 32
                        if t4896 {
                            var t4897 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4898 Result__string__string = Result__string__string_Err{
                                _0: t4897,
                            }
                            return t4898
                        } else {
                            var t4899 *ref_int_x = value__217.index
                            var t4900 *ref_int_x = value__217.index
                            var t4901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4900)
                            var t4902 int = t4901 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4899, t4902)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4805
            }
        }
        var t4803 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4804 Result__string__string = Result__string__string_Err{
            _0: t4803,
        }
        return t4804
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4931 *ref_int_x = value__225.index
    var start__226 int
    var inline10117 int = ref_get__Ref_3int(t4931)
    start__226 = inline10117
    Loop_loop4936:
    for {
        var t4944 *ref_int_x = value__225.index
        var t4945 int
        var inline10113 int = ref_get__Ref_3int(t4944)
        t4945 = inline10113
        var t4946 string = value__225.input
        var t4947 int
        var inline10111 int = _goml_runtime_core_string_len(t4946)
        t4947 = inline10111
        var t4948 bool = t4945 < t4947
        var jp4938 bool
        if t4948 {
            var t4949 string = value__225.input
            var t4950 *ref_int_x = value__225.index
            var t4951 int
            var inline10105 int = ref_get__Ref_3int(t4950)
            t4951 = inline10105
            var t4952 uint8
            var inline10103 uint8 = _goml_runtime_core_string_byte_get(t4949, t4951)
            t4952 = inline10103
            var inline10100 bool = t4952 >= 48
            if inline10100 {
                var inline10101 bool = t4952 <= 57
                jp4938 = inline10101
            } else {
                jp4938 = false
            }
        } else {
            jp4938 = false
        }
        if jp4938 {
            var t4939 *ref_int_x = value__225.index
            var t4940 *ref_int_x = value__225.index
            var t4941 int
            var inline10109 int = ref_get__Ref_3int(t4940)
            t4941 = inline10109
            var t4942 int = t4941 + 1
            ref_set__Ref_3int(t4939, t4942)
            continue
        } else {
            break Loop_loop4936
        }
    }
    var t4933 *ref_int_x = value__225.index
    var t4934 int
    var inline10115 int = ref_get__Ref_3int(t4933)
    t4934 = inline10115
    var t4935 bool = t4934 > start__226
    return t4935
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4956 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4956)
    var t5077 string = value__227.input
    var t5078 *ref_int_x = value__227.index
    var t5079 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5078)
    var t5080 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5077, t5079)
    var t5081 bool = t5080 == 45
    if t5081 {
        var t5082 *ref_int_x = value__227.index
        var t5083 *ref_int_x = value__227.index
        var t5084 int
        var inline10121 int = ref_get__Ref_3int(t5083)
        t5084 = inline10121
        var t5085 int = t5084 + 1
        ref_set__Ref_3int(t5082, t5085)
    } else {}
    var t5040 *ref_int_x = value__227.index
    var t5041 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5040)
    var t5042 string = value__227.input
    var t5043 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5042)
    var t5044 bool = t5041 >= t5043
    if t5044 {
        var t5045 string
        var inline10123 string = "incomplete number"
        var inline10124 string = "" + inline10123
        var inline10125 string = inline10124 + " at byte "
        var inline10126 *ref_int_x = value__227.index
        var inline10127 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10126)
        var inline10128 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10127)
        var inline10129 string = inline10125 + inline10128
        t5045 = inline10129
        var t5046 Result__string__string = Result__string__string_Err{
            _0: t5045,
        }
        return t5046
    } else {
        var t5048 string = value__227.input
        var t5049 *ref_int_x = value__227.index
        var t5050 int
        var inline10164 int = ref_get__Ref_3int(t5049)
        t5050 = inline10164
        var t5051 uint8
        var inline10162 uint8 = _goml_runtime_core_string_byte_get(t5048, t5050)
        t5051 = inline10162
        var t5052 bool = t5051 == 48
        if t5052 {
            var t5053 *ref_int_x = value__227.index
            var t5054 *ref_int_x = value__227.index
            var t5055 int
            var inline10152 int = ref_get__Ref_3int(t5054)
            t5055 = inline10152
            var t5056 int = t5055 + 1
            ref_set__Ref_3int(t5053, t5056)
            var t5062 *ref_int_x = value__227.index
            var t5063 int
            var inline10148 int = ref_get__Ref_3int(t5062)
            t5063 = inline10148
            var t5064 string = value__227.input
            var t5065 int
            var inline10146 int = _goml_runtime_core_string_len(t5064)
            t5065 = inline10146
            var t5066 bool = t5063 < t5065
            var jp5059 bool
            if t5066 {
                var t5067 string = value__227.input
                var t5068 *ref_int_x = value__227.index
                var t5069 int
                var inline10136 int = ref_get__Ref_3int(t5068)
                t5069 = inline10136
                var t5070 uint8
                var inline10134 uint8 = _goml_runtime_core_string_byte_get(t5067, t5069)
                t5070 = inline10134
                var inline10131 bool = t5070 >= 48
                if inline10131 {
                    var inline10132 bool = t5070 <= 57
                    jp5059 = inline10132
                } else {
                    jp5059 = false
                }
            } else {
                jp5059 = false
            }
            if jp5059 {
                var t5060 string
                var inline10138 string = "invalid leading zero"
                var inline10139 string = "" + inline10138
                var inline10140 string = inline10139 + " at byte "
                var inline10141 *ref_int_x = value__227.index
                var inline10142 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10141)
                var inline10143 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10142)
                var inline10144 string = inline10140 + inline10143
                t5060 = inline10144
                var t5061 Result__string__string = Result__string__string_Err{
                    _0: t5060,
                }
                return t5061
            } else {
                var t5030 *ref_int_x = value__227.index
                var t5031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5030)
                var t5032 string = value__227.input
                var t5033 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5032)
                var t5034 bool = t5031 < t5033
                var jp5020 bool
                if t5034 {
                    var t5035 string = value__227.input
                    var t5036 *ref_int_x = value__227.index
                    var t5037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5036)
                    var t5038 uint8
                    var inline10166 uint8 = _goml_runtime_core_string_byte_get(t5035, t5037)
                    t5038 = inline10166
                    var t5039 bool = t5038 == 46
                    jp5020 = t5039
                } else {
                    jp5020 = false
                }
                if jp5020 {
                    var t5021 *ref_int_x = value__227.index
                    var t5022 *ref_int_x = value__227.index
                    var t5023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5022)
                    var t5024 int = t5023 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5021, t5024)
                    var t5026 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5027 bool = !t5026
                    if t5027 {
                        var t5028 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5029 Result__string__string = Result__string__string_Err{
                            _0: t5028,
                        }
                        return t5029
                    } else {
                        var t5002 *ref_int_x = value__227.index
                        var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                        var t5004 string = value__227.input
                        var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                        var t5006 bool = t5003 < t5005
                        var jp4967 bool
                        if t5006 {
                            var t5009 string = value__227.input
                            var t5010 *ref_int_x = value__227.index
                            var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                            var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                            var t5013 bool = t5012 == 101
                            if t5013 {
                                jp4967 = true
                            } else {
                                var t5014 string = value__227.input
                                var t5015 *ref_int_x = value__227.index
                                var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                                var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                                var t5018 bool = t5017 == 69
                                jp4967 = t5018
                            }
                        } else {
                            jp4967 = false
                        }
                        if jp4967 {
                            var t4968 *ref_int_x = value__227.index
                            var t4969 *ref_int_x = value__227.index
                            var t4970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4969)
                            var t4971 int = t4970 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4968, t4971)
                            var t4985 *ref_int_x = value__227.index
                            var t4986 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4985)
                            var t4987 string = value__227.input
                            var t4988 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4987)
                            var t4989 bool = t4986 < t4988
                            var jp4979 bool
                            if t4989 {
                                var t4992 string = value__227.input
                                var t4993 *ref_int_x = value__227.index
                                var t4994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4993)
                                var t4995 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4992, t4994)
                                var t4996 bool = t4995 == 43
                                if t4996 {
                                    jp4979 = true
                                } else {
                                    var t4997 string = value__227.input
                                    var t4998 *ref_int_x = value__227.index
                                    var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                                    var t5000 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4997, t4999)
                                    var t5001 bool = t5000 == 45
                                    jp4979 = t5001
                                }
                            } else {
                                jp4979 = false
                            }
                            if jp4979 {
                                var t4980 *ref_int_x = value__227.index
                                var t4981 *ref_int_x = value__227.index
                                var t4982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4981)
                                var t4983 int = t4982 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4980, t4983)
                            } else {}
                            var t4974 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4975 bool = !t4974
                            if t4975 {
                                var t4976 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4977 Result__string__string = Result__string__string_Err{
                                    _0: t4976,
                                }
                                return t4977
                            } else {
                                var t4961 string = value__227.input
                                var t4962 *ref_int_x = value__227.index
                                var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                                var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                                var t4965 Result__string__string = Result__string__string_Ok{
                                    _0: t4964,
                                }
                                return t4965
                            }
                        } else {
                            var t4961 string = value__227.input
                            var t4962 *ref_int_x = value__227.index
                            var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                            var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                            var t4965 Result__string__string = Result__string__string_Ok{
                                _0: t4964,
                            }
                            return t4965
                        }
                    }
                } else {
                    var t5002 *ref_int_x = value__227.index
                    var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                    var t5004 string = value__227.input
                    var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                    var t5006 bool = t5003 < t5005
                    var jp4967 bool
                    if t5006 {
                        var t5009 string = value__227.input
                        var t5010 *ref_int_x = value__227.index
                        var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                        var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                        var t5013 bool = t5012 == 101
                        if t5013 {
                            jp4967 = true
                        } else {
                            var t5014 string = value__227.input
                            var t5015 *ref_int_x = value__227.index
                            var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                            var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                            var t5018 bool = t5017 == 69
                            jp4967 = t5018
                        }
                    } else {
                        jp4967 = false
                    }
                    if jp4967 {
                        var t4968 *ref_int_x = value__227.index
                        var t4969 *ref_int_x = value__227.index
                        var t4970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4969)
                        var t4971 int = t4970 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4968, t4971)
                        var t4985 *ref_int_x = value__227.index
                        var t4986 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4985)
                        var t4987 string = value__227.input
                        var t4988 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4987)
                        var t4989 bool = t4986 < t4988
                        var jp4979 bool
                        if t4989 {
                            var t4992 string = value__227.input
                            var t4993 *ref_int_x = value__227.index
                            var t4994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4993)
                            var t4995 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4992, t4994)
                            var t4996 bool = t4995 == 43
                            if t4996 {
                                jp4979 = true
                            } else {
                                var t4997 string = value__227.input
                                var t4998 *ref_int_x = value__227.index
                                var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                                var t5000 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4997, t4999)
                                var t5001 bool = t5000 == 45
                                jp4979 = t5001
                            }
                        } else {
                            jp4979 = false
                        }
                        if jp4979 {
                            var t4980 *ref_int_x = value__227.index
                            var t4981 *ref_int_x = value__227.index
                            var t4982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4981)
                            var t4983 int = t4982 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4980, t4983)
                        } else {}
                        var t4974 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4975 bool = !t4974
                        if t4975 {
                            var t4976 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4977 Result__string__string = Result__string__string_Err{
                                _0: t4976,
                            }
                            return t4977
                        } else {
                            var t4961 string = value__227.input
                            var t4962 *ref_int_x = value__227.index
                            var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                            var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                            var t4965 Result__string__string = Result__string__string_Ok{
                                _0: t4964,
                            }
                            return t4965
                        }
                    } else {
                        var t4961 string = value__227.input
                        var t4962 *ref_int_x = value__227.index
                        var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                        var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                        var t4965 Result__string__string = Result__string__string_Ok{
                            _0: t4964,
                        }
                        return t4965
                    }
                }
            }
        } else {
            var t5073 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5074 bool = !t5073
            if t5074 {
                var t5075 string
                var inline10154 string = "expected number"
                var inline10155 string = "" + inline10154
                var inline10156 string = inline10155 + " at byte "
                var inline10157 *ref_int_x = value__227.index
                var inline10158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10157)
                var inline10159 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10158)
                var inline10160 string = inline10156 + inline10159
                t5075 = inline10160
                var t5076 Result__string__string = Result__string__string_Err{
                    _0: t5075,
                }
                return t5076
            } else {
                var t5030 *ref_int_x = value__227.index
                var t5031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5030)
                var t5032 string = value__227.input
                var t5033 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5032)
                var t5034 bool = t5031 < t5033
                var jp5020 bool
                if t5034 {
                    var t5035 string = value__227.input
                    var t5036 *ref_int_x = value__227.index
                    var t5037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5036)
                    var t5038 uint8
                    var inline10166 uint8 = _goml_runtime_core_string_byte_get(t5035, t5037)
                    t5038 = inline10166
                    var t5039 bool = t5038 == 46
                    jp5020 = t5039
                } else {
                    jp5020 = false
                }
                if jp5020 {
                    var t5021 *ref_int_x = value__227.index
                    var t5022 *ref_int_x = value__227.index
                    var t5023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5022)
                    var t5024 int = t5023 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5021, t5024)
                    var t5026 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5027 bool = !t5026
                    if t5027 {
                        var t5028 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5029 Result__string__string = Result__string__string_Err{
                            _0: t5028,
                        }
                        return t5029
                    } else {
                        var t5002 *ref_int_x = value__227.index
                        var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                        var t5004 string = value__227.input
                        var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                        var t5006 bool = t5003 < t5005
                        var jp4967 bool
                        if t5006 {
                            var t5009 string = value__227.input
                            var t5010 *ref_int_x = value__227.index
                            var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                            var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                            var t5013 bool = t5012 == 101
                            if t5013 {
                                jp4967 = true
                            } else {
                                var t5014 string = value__227.input
                                var t5015 *ref_int_x = value__227.index
                                var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                                var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                                var t5018 bool = t5017 == 69
                                jp4967 = t5018
                            }
                        } else {
                            jp4967 = false
                        }
                        if jp4967 {
                            var t4968 *ref_int_x = value__227.index
                            var t4969 *ref_int_x = value__227.index
                            var t4970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4969)
                            var t4971 int = t4970 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4968, t4971)
                            var t4985 *ref_int_x = value__227.index
                            var t4986 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4985)
                            var t4987 string = value__227.input
                            var t4988 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4987)
                            var t4989 bool = t4986 < t4988
                            var jp4979 bool
                            if t4989 {
                                var t4992 string = value__227.input
                                var t4993 *ref_int_x = value__227.index
                                var t4994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4993)
                                var t4995 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4992, t4994)
                                var t4996 bool = t4995 == 43
                                if t4996 {
                                    jp4979 = true
                                } else {
                                    var t4997 string = value__227.input
                                    var t4998 *ref_int_x = value__227.index
                                    var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                                    var t5000 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4997, t4999)
                                    var t5001 bool = t5000 == 45
                                    jp4979 = t5001
                                }
                            } else {
                                jp4979 = false
                            }
                            if jp4979 {
                                var t4980 *ref_int_x = value__227.index
                                var t4981 *ref_int_x = value__227.index
                                var t4982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4981)
                                var t4983 int = t4982 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4980, t4983)
                            } else {}
                            var t4974 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4975 bool = !t4974
                            if t4975 {
                                var t4976 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4977 Result__string__string = Result__string__string_Err{
                                    _0: t4976,
                                }
                                return t4977
                            } else {
                                var t4961 string = value__227.input
                                var t4962 *ref_int_x = value__227.index
                                var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                                var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                                var t4965 Result__string__string = Result__string__string_Ok{
                                    _0: t4964,
                                }
                                return t4965
                            }
                        } else {
                            var t4961 string = value__227.input
                            var t4962 *ref_int_x = value__227.index
                            var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                            var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                            var t4965 Result__string__string = Result__string__string_Ok{
                                _0: t4964,
                            }
                            return t4965
                        }
                    }
                } else {
                    var t5002 *ref_int_x = value__227.index
                    var t5003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5002)
                    var t5004 string = value__227.input
                    var t5005 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5004)
                    var t5006 bool = t5003 < t5005
                    var jp4967 bool
                    if t5006 {
                        var t5009 string = value__227.input
                        var t5010 *ref_int_x = value__227.index
                        var t5011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5010)
                        var t5012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5009, t5011)
                        var t5013 bool = t5012 == 101
                        if t5013 {
                            jp4967 = true
                        } else {
                            var t5014 string = value__227.input
                            var t5015 *ref_int_x = value__227.index
                            var t5016 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5015)
                            var t5017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5014, t5016)
                            var t5018 bool = t5017 == 69
                            jp4967 = t5018
                        }
                    } else {
                        jp4967 = false
                    }
                    if jp4967 {
                        var t4968 *ref_int_x = value__227.index
                        var t4969 *ref_int_x = value__227.index
                        var t4970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4969)
                        var t4971 int = t4970 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4968, t4971)
                        var t4985 *ref_int_x = value__227.index
                        var t4986 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4985)
                        var t4987 string = value__227.input
                        var t4988 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4987)
                        var t4989 bool = t4986 < t4988
                        var jp4979 bool
                        if t4989 {
                            var t4992 string = value__227.input
                            var t4993 *ref_int_x = value__227.index
                            var t4994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4993)
                            var t4995 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4992, t4994)
                            var t4996 bool = t4995 == 43
                            if t4996 {
                                jp4979 = true
                            } else {
                                var t4997 string = value__227.input
                                var t4998 *ref_int_x = value__227.index
                                var t4999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4998)
                                var t5000 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4997, t4999)
                                var t5001 bool = t5000 == 45
                                jp4979 = t5001
                            }
                        } else {
                            jp4979 = false
                        }
                        if jp4979 {
                            var t4980 *ref_int_x = value__227.index
                            var t4981 *ref_int_x = value__227.index
                            var t4982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4981)
                            var t4983 int = t4982 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4980, t4983)
                        } else {}
                        var t4974 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4975 bool = !t4974
                        if t4975 {
                            var t4976 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4977 Result__string__string = Result__string__string_Err{
                                _0: t4976,
                            }
                            return t4977
                        } else {
                            var t4961 string = value__227.input
                            var t4962 *ref_int_x = value__227.index
                            var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                            var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                            var t4965 Result__string__string = Result__string__string_Ok{
                                _0: t4964,
                            }
                            return t4965
                        }
                    } else {
                        var t4961 string = value__227.input
                        var t4962 *ref_int_x = value__227.index
                        var t4963 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4962)
                        var t4964 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4961, start__228, t4963)
                        var t4965 Result__string__string = Result__string__string_Ok{
                            _0: t4964,
                        }
                        return t4965
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5108 *ref_int_x = value__230.index
    var t5109 int
    var inline10194 int = ref_get__Ref_3int(t5108)
    t5109 = inline10194
    var t5110 int
    var inline10192 int = _goml_runtime_core_string_len(expected__231)
    t5110 = inline10192
    var t5111 int = t5109 + t5110
    var t5112 string = value__230.input
    var t5113 int
    var inline10190 int = _goml_runtime_core_string_len(t5112)
    t5113 = inline10190
    var t5114 bool = t5111 <= t5113
    var jp5099 bool
    if t5114 {
        var t5115 string = value__230.input
        var t5116 *ref_int_x = value__230.index
        var t5117 int
        var inline10174 int = ref_get__Ref_3int(t5116)
        t5117 = inline10174
        var t5118 *ref_int_x = value__230.index
        var t5119 int
        var inline10172 int = ref_get__Ref_3int(t5118)
        t5119 = inline10172
        var t5120 int
        var inline10170 int = _goml_runtime_core_string_len(expected__231)
        t5120 = inline10170
        var t5121 int = t5119 + t5120
        var t5122 string
        var inline10168 string = string_byte_slice(t5115, t5117, t5121)
        t5122 = inline10168
        var t5123 bool = t5122 == expected__231
        jp5099 = t5123
    } else {
        jp5099 = false
    }
    if jp5099 {
        var t5100 *ref_int_x = value__230.index
        var t5101 *ref_int_x = value__230.index
        var t5102 int
        var inline10180 int = ref_get__Ref_3int(t5101)
        t5102 = inline10180
        var t5103 int
        var inline10178 int = _goml_runtime_core_string_len(expected__231)
        t5103 = inline10178
        var t5104 int = t5102 + t5103
        ref_set__Ref_3int(t5100, t5104)
        var t5105 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t5105
    } else {
        var t5106 string
        var inline10182 string = "invalid literal"
        var inline10183 string = "" + inline10182
        var inline10184 string = inline10183 + " at byte "
        var inline10185 *ref_int_x = value__230.index
        var inline10186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10185)
        var inline10187 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10186)
        var inline10188 string = inline10184 + inline10187
        t5106 = inline10188
        var t5107 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5106,
        }
        return t5107
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5126 *ref_int_x = value__233.index
    var t5127 *ref_int_x = value__233.index
    var t5128 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5127)
    var t5129 int = t5128 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5126, t5129)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8833 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t5184 *ref_int_x = value__233.index
    var t5185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5184)
    var t5186 string = value__233.input
    var t5187 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5186)
    var t5188 bool = t5185 < t5187
    var jp5177 bool
    if t5188 {
        var t5189 string = value__233.input
        var t5190 *ref_int_x = value__233.index
        var t5191 int
        var inline10198 int = ref_get__Ref_3int(t5190)
        t5191 = inline10198
        var t5192 uint8
        var inline10196 uint8 = _goml_runtime_core_string_byte_get(t5189, t5191)
        t5192 = inline10196
        var t5193 bool = t5192 == 93
        jp5177 = t5193
    } else {
        jp5177 = false
    }
    if jp5177 {
        var t5178 *ref_int_x = value__233.index
        var t5179 *ref_int_x = value__233.index
        var t5180 int
        var inline10202 int = ref_get__Ref_3int(t5179)
        t5180 = inline10202
        var t5181 int = t5180 + 1
        ref_set__Ref_3int(t5178, t5181)
        var t5182 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8833,
        }
        var t5183 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5182,
        }
        return t5183
    } else {
        Loop_loop5134:
        for {
            var t5135 *ref_int_x = value__233.index
            var t5136 int
            var inline10244 int = ref_get__Ref_3int(t5135)
            t5136 = inline10244
            var t5137 string = value__233.input
            var t5138 int
            var inline10242 int = _goml_runtime_core_string_len(t5137)
            t5138 = inline10242
            var t5139 bool = t5136 < t5138
            if t5139 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5141 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp5141 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(vec_literal__8833, jp5141)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5143 *ref_int_x = value__233.index
                    var t5144 int
                    var inline10238 int = ref_get__Ref_3int(t5143)
                    t5144 = inline10238
                    var t5145 string = value__233.input
                    var t5146 int
                    var inline10236 int = _goml_runtime_core_string_len(t5145)
                    t5146 = inline10236
                    var t5147 bool = t5144 >= t5146
                    if t5147 {
                        var t5148 string
                        var inline10204 string = "unterminated array"
                        var inline10205 string = "" + inline10204
                        var inline10206 string = inline10205 + " at byte "
                        var inline10207 *ref_int_x = value__233.index
                        var inline10208 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10207)
                        var inline10209 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10208)
                        var inline10210 string = inline10206 + inline10209
                        t5148 = inline10210
                        var t5149 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5148,
                        }
                        return t5149
                    } else {
                        var t5151 string = value__233.input
                        var t5152 *ref_int_x = value__233.index
                        var t5153 int
                        var inline10234 int = ref_get__Ref_3int(t5152)
                        t5153 = inline10234
                        var t5154 uint8
                        var inline10232 uint8 = _goml_runtime_core_string_byte_get(t5151, t5153)
                        t5154 = inline10232
                        var t5155 bool = t5154 == 93
                        if t5155 {
                            var t5156 *ref_int_x = value__233.index
                            var t5157 *ref_int_x = value__233.index
                            var t5158 int
                            var inline10214 int = ref_get__Ref_3int(t5157)
                            t5158 = inline10214
                            var t5159 int = t5158 + 1
                            ref_set__Ref_3int(t5156, t5159)
                            var t5160 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8833,
                            }
                            var t5161 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t5160,
                            }
                            return t5161
                        } else {
                            var t5163 string = value__233.input
                            var t5164 *ref_int_x = value__233.index
                            var t5165 int
                            var inline10230 int = ref_get__Ref_3int(t5164)
                            t5165 = inline10230
                            var t5166 uint8
                            var inline10228 uint8 = _goml_runtime_core_string_byte_get(t5163, t5165)
                            t5166 = inline10228
                            var t5167 bool = t5166 == 44
                            if t5167 {
                                var t5168 *ref_int_x = value__233.index
                                var t5169 *ref_int_x = value__233.index
                                var t5170 int
                                var inline10218 int = ref_get__Ref_3int(t5169)
                                t5170 = inline10218
                                var t5171 int = t5170 + 1
                                ref_set__Ref_3int(t5168, t5171)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5173 string
                                var inline10220 string = "expected array separator"
                                var inline10221 string = "" + inline10220
                                var inline10222 string = inline10221 + " at byte "
                                var inline10223 *ref_int_x = value__233.index
                                var inline10224 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10223)
                                var inline10225 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10224)
                                var inline10226 string = inline10222 + inline10225
                                t5173 = inline10226
                                var t5174 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5173,
                                }
                                return t5174
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t5175 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t5175
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5134
            }
        }
        var t5132 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5133 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5132,
        }
        return t5133
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5196 *ref_int_x = value__236.index
    var t5197 *ref_int_x = value__236.index
    var t5198 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5197)
    var t5199 int = t5198 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5196, t5199)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10035 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5278 *ref_int_x = value__236.index
    var t5279 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5278)
    var t5280 string = value__236.input
    var t5281 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5280)
    var t5282 bool = t5279 < t5281
    var jp5271 bool
    if t5282 {
        var t5283 string = value__236.input
        var t5284 *ref_int_x = value__236.index
        var t5285 int
        var inline10248 int = ref_get__Ref_3int(t5284)
        t5285 = inline10248
        var t5286 uint8
        var inline10246 uint8 = _goml_runtime_core_string_byte_get(t5283, t5285)
        t5286 = inline10246
        var t5287 bool = t5286 == 125
        jp5271 = t5287
    } else {
        jp5271 = false
    }
    if jp5271 {
        var t5272 *ref_int_x = value__236.index
        var t5273 *ref_int_x = value__236.index
        var t5274 int
        var inline10252 int = ref_get__Ref_3int(t5273)
        t5274 = inline10252
        var t5275 int = t5274 + 1
        ref_set__Ref_3int(t5272, t5275)
        var t5276 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10035,
        }
        var t5277 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5276,
        }
        return t5277
    } else {
        Loop_loop5204:
        for {
            var t5205 *ref_int_x = value__236.index
            var t5206 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5205)
            var t5207 string = value__236.input
            var t5208 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5207)
            var t5209 bool = t5206 < t5208
            if t5209 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5211 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5211 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5259 *ref_int_x = value__236.index
                    var t5260 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5259)
                    var t5261 string = value__236.input
                    var t5262 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5261)
                    var t5263 bool = t5260 >= t5262
                    var jp5251 bool
                    if t5263 {
                        jp5251 = true
                    } else {
                        var t5264 string = value__236.input
                        var t5265 *ref_int_x = value__236.index
                        var t5266 int
                        var inline10256 int = ref_get__Ref_3int(t5265)
                        t5266 = inline10256
                        var t5267 uint8
                        var inline10254 uint8 = _goml_runtime_core_string_byte_get(t5264, t5266)
                        t5267 = inline10254
                        var t5268 bool = t5267 != 58
                        jp5251 = t5268
                    }
                    if jp5251 {
                        var t5252 string
                        var inline10258 string = "expected object colon"
                        var inline10259 string = "" + inline10258
                        var inline10260 string = inline10259 + " at byte "
                        var inline10261 *ref_int_x = value__236.index
                        var inline10262 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10261)
                        var inline10263 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10262)
                        var inline10264 string = inline10260 + inline10263
                        t5252 = inline10264
                        var t5253 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5252,
                        }
                        return t5253
                    } else {
                        var t5254 *ref_int_x = value__236.index
                        var t5255 *ref_int_x = value__236.index
                        var t5256 int
                        var inline10268 int = ref_get__Ref_3int(t5255)
                        t5256 = inline10268
                        var t5257 int = t5256 + 1
                        ref_set__Ref_3int(t5254, t5257)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5214 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5214 = x816
                            var t5215 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5211,
                                _1: jp5214,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10035, t5215)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5217 *ref_int_x = value__236.index
                            var t5218 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5217)
                            var t5219 string = value__236.input
                            var t5220 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5219)
                            var t5221 bool = t5218 >= t5220
                            if t5221 {
                                var t5222 string
                                var inline10270 string = "unterminated object"
                                var inline10271 string = "" + inline10270
                                var inline10272 string = inline10271 + " at byte "
                                var inline10273 *ref_int_x = value__236.index
                                var inline10274 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10273)
                                var inline10275 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10274)
                                var inline10276 string = inline10272 + inline10275
                                t5222 = inline10276
                                var t5223 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5222,
                                }
                                return t5223
                            } else {
                                var t5225 string = value__236.input
                                var t5226 *ref_int_x = value__236.index
                                var t5227 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5226)
                                var t5228 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5225, t5227)
                                var t5229 bool = t5228 == 125
                                if t5229 {
                                    var t5230 *ref_int_x = value__236.index
                                    var t5231 *ref_int_x = value__236.index
                                    var t5232 int
                                    var inline10280 int = ref_get__Ref_3int(t5231)
                                    t5232 = inline10280
                                    var t5233 int = t5232 + 1
                                    ref_set__Ref_3int(t5230, t5233)
                                    var t5234 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10035,
                                    }
                                    var t5235 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5234,
                                    }
                                    return t5235
                                } else {
                                    var t5237 string = value__236.input
                                    var t5238 *ref_int_x = value__236.index
                                    var t5239 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5238)
                                    var t5240 uint8
                                    var inline10294 uint8 = _goml_runtime_core_string_byte_get(t5237, t5239)
                                    t5240 = inline10294
                                    var t5241 bool = t5240 == 44
                                    if t5241 {
                                        var t5242 *ref_int_x = value__236.index
                                        var t5243 *ref_int_x = value__236.index
                                        var t5244 int
                                        var inline10284 int = ref_get__Ref_3int(t5243)
                                        t5244 = inline10284
                                        var t5245 int = t5244 + 1
                                        ref_set__Ref_3int(t5242, t5245)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5247 string
                                        var inline10286 string = "expected object separator"
                                        var inline10287 string = "" + inline10286
                                        var inline10288 string = inline10287 + " at byte "
                                        var inline10289 *ref_int_x = value__236.index
                                        var inline10290 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10289)
                                        var inline10291 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10290)
                                        var inline10292 string = inline10288 + inline10291
                                        t5247 = inline10292
                                        var t5248 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5247,
                                        }
                                        return t5248
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5249 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5249
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5269 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5269
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5204
            }
        }
        var t5202 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5203 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5202,
        }
        return t5203
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5292 *ref_int_x = value__240.index
    var t5293 int
    var inline10332 int = ref_get__Ref_3int(t5292)
    t5293 = inline10332
    var t5294 string = value__240.input
    var t5295 int
    var inline10330 int = _goml_runtime_core_string_len(t5294)
    t5295 = inline10330
    var t5296 bool = t5293 >= t5295
    if t5296 {
        var t5297 string
        var inline10296 string = "expected JSON value"
        var inline10297 string = "" + inline10296
        var inline10298 string = inline10297 + " at byte "
        var inline10299 *ref_int_x = value__240.index
        var inline10300 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10299)
        var inline10301 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10300)
        var inline10302 string = inline10298 + inline10301
        t5297 = inline10302
        var t5298 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5297,
        }
        return t5298
    } else {
        var t5299 string = value__240.input
        var t5300 *ref_int_x = value__240.index
        var t5301 int
        var inline10328 int = ref_get__Ref_3int(t5300)
        t5301 = inline10328
        var mtmp824 uint8
        var inline10326 uint8 = _goml_runtime_core_string_byte_get(t5299, t5301)
        mtmp824 = inline10326
        switch mtmp824 {
        case 123:
            var t5304 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5304
        case 91:
            var t5305 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5305
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5308 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5309 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5308,
                }
                return t5309
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5310 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5310
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5311 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5312 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5311)
            return t5312
        case 102:
            var t5313 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5314 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5313)
            return t5314
        case 110:
            var t5315 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5315
        default:
            var t5323 bool = mtmp824 == 45
            var jp5319 bool
            if t5323 {
                jp5319 = true
            } else {
                var inline10304 bool = mtmp824 >= 48
                if inline10304 {
                    var inline10305 bool = mtmp824 <= 57
                    jp5319 = inline10305
                } else {
                    jp5319 = false
                }
            }
            if jp5319 {
                var inline10307 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10309 string
                switch inline10307.(type) {
                case Result__string__string_Ok:
                    var inline10312 string = inline10307.(Result__string__string_Ok)._0
                    inline10309 = inline10312
                    var inline10310 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10309,
                    }
                    var inline10311 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10310,
                    }
                    return inline10311
                case Result__string__string_Err:
                    var inline10314 string = inline10307.(Result__string__string_Err)._0
                    var inline10316 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10314,
                    }
                    return inline10316
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5321 string
                var inline10318 string = "unexpected JSON token"
                var inline10319 string = "" + inline10318
                var inline10320 string = inline10319 + " at byte "
                var inline10321 *ref_int_x = value__240.index
                var inline10322 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10321)
                var inline10323 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10322)
                var inline10324 string = inline10320 + inline10323
                t5321 = inline10324
                var t5322 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5321,
                }
                return t5322
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10346 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10347 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10346,
    }
    parser__245 = inline10347
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5328 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5328 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5331 *ref_int_x = parser__245.index
        var t5332 int
        var inline10344 int = ref_get__Ref_3int(t5331)
        t5332 = inline10344
        var t5333 int
        var inline10342 int = _goml_runtime_core_string_len(input__244)
        t5333 = inline10342
        var t5334 bool = t5332 == t5333
        if t5334 {
            var t5335 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5328,
            }
            return t5335
        } else {
            var t5336 string
            var inline10334 string = "trailing JSON data"
            var inline10335 string = "" + inline10334
            var inline10336 string = inline10335 + " at byte "
            var inline10337 *ref_int_x = parser__245.index
            var inline10338 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10337)
            var inline10339 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10338)
            var inline10340 string = inline10336 + inline10339
            t5336 = inline10340
            var t5337 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5336,
            }
            return t5337
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5338 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5338
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10380 rune = 34
    var inline10381 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10380)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10381)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10378 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10378
    Loop_loop5352:
    for {
        var t5353 bool = for_index833 < for_limit834
        if t5353 {
            var for_item835 int = for_index833
            var t5354 int = for_index833 + 1
            for_index833 = t5354
            var byte__252 uint8
            var inline10366 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10366
            var t5407 bool = byte__252 == 34
            var jp5405 bool
            if t5407 {
                jp5405 = true
            } else {
                var t5408 bool = byte__252 == 92
                jp5405 = t5408
            }
            var jp5402 bool
            if jp5405 {
                jp5402 = true
            } else {
                var t5406 bool = byte__252 == 8
                jp5402 = t5406
            }
            var jp5399 bool
            if jp5402 {
                jp5399 = true
            } else {
                var t5403 bool = byte__252 == 9
                jp5399 = t5403
            }
            var jp5396 bool
            if jp5399 {
                jp5396 = true
            } else {
                var t5400 bool = byte__252 == 10
                jp5396 = t5400
            }
            var jp5393 bool
            if jp5396 {
                jp5393 = true
            } else {
                var t5397 bool = byte__252 == 12
                jp5393 = t5397
            }
            var jp5390 bool
            if jp5393 {
                jp5390 = true
            } else {
                var t5394 bool = byte__252 == 13
                jp5390 = t5394
            }
            var jp5357 bool
            if jp5390 {
                jp5357 = true
            } else {
                var t5391 bool = byte__252 < 32
                jp5357 = t5391
            }
            if jp5357 {
                var t5386 bool = start__250 < for_item835
                if t5386 {
                    var t5387 string
                    var inline10352 string = string_byte_slice(value__249, start__250, for_item835)
                    t5387 = inline10352
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5387)
                } else {}
                var t5361 bool = byte__252 == 34
                if t5361 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5364 bool = byte__252 == 92
                    if t5364 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5367 bool = byte__252 == 8
                        if t5367 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5370 bool = byte__252 == 9
                            if t5370 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5373 bool = byte__252 == 10
                                if t5373 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5376 bool = byte__252 == 12
                                    if t5376 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5379 bool = byte__252 == 13
                                        if t5379 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5381 uint8 = byte__252 / 16
                                            var t5382 rune
                                            var inline10363 int = int(uint8(t5381))
                                            var inline10364 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10363)
                                            t5382 = inline10364
                                            var inline10360 string = _goml_m_inherent_i_char_i_char_i_to__string(t5382)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10360)
                                            var t5383_rhs uint8 = 16
                                            var t5383 uint8 = byte__252 % t5383_rhs
                                            var t5384 rune
                                            var inline10357 int = int(uint8(t5383))
                                            var inline10358 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10357)
                                            t5384 = inline10358
                                            var inline10354 string = _goml_m_inherent_i_char_i_char_i_to__string(t5384)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10354)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5360 int = for_item835 + 1
                start__250 = t5360
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5352
        }
    }
    var t5347 int
    var inline10376 int = _goml_runtime_core_string_len(value__249)
    t5347 = inline10376
    var t5348 bool = start__250 < t5347
    if t5348 {
        var t5349 int
        var inline10370 int = _goml_runtime_core_string_len(value__249)
        t5349 = inline10370
        var t5350 string
        var inline10368 string = string_byte_slice(value__249, start__250, t5349)
        t5350 = inline10368
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5350)
    } else {}
    var inline10372 rune = 34
    var inline10373 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10372)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10373)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10396 rune = 123
        var inline10397 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10396)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10397)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5413:
        for {
            var t5414 bool = for_index852 < for_limit851
            if t5414 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5415 int = for_index852 + 1
                for_index852 = t5415
                var t5421 bool = index__256 > 0
                if t5421 {
                    var inline10384 rune = 44
                    var inline10385 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10384)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10385)
                } else {}
                var t5417 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5417)
                var inline10388 rune = 58
                var inline10389 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10388)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10389)
                var t5418 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5418)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5419 int = compound_old859 + compound_value860
                index__256 = t5419
                continue
            } else {
                break Loop_loop5413
            }
        }
        var inline10392 rune = 125
        var inline10393 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10392)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10393)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10408 rune = 91
        var inline10409 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10408)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10409)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5425:
        for {
            var t5426 bool = for_index866 < for_limit865
            if t5426 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5427 int = for_index866 + 1
                for_index866 = t5427
                var t5431 bool = index__259 > 0
                if t5431 {
                    var inline10400 rune = 44
                    var inline10401 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10400)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10401)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5429 int = compound_old871 + compound_value872
                index__259 = t5429
                continue
            } else {
                break Loop_loop5425
            }
        }
        var inline10404 rune = 93
        var inline10405 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10404)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10405)
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
        var jp5436 string
        if x848 {
            jp5436 = "true"
        } else {
            jp5436 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5436)
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
    var inline10417 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10418 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10417,
    }
    builder__265 = inline10418
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10412 *_goml_vec_uint8 = builder__265.values
    var inline10413 Tuple2_4bool_6string = string_from_utf8(inline10412)
    var inline10414 string = inline10413._1
    return inline10414
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5447:
        for {
            var t5448 bool = for_index883 < for_limit882
            if t5448 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5449 int = for_index883 + 1
                for_index883 = t5449
                var t5451 string = for_item884._0
                var t5452 bool = t5451 == name__267
                if t5452 {
                    var t5453 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5454 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5453,
                    }
                    return t5454
                } else {
                    continue
                }
            } else {
                break Loop_loop5447
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5464 int
    var inline10429 int = _goml_runtime_core_string_len(value__272)
    t5464 = inline10429
    var t5465 bool = t5464 == 0
    if t5465 {
        return Option__int_None{}
    } else {
        var t5466 uint8
        var inline10426 int = 0
        var inline10427 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10426)
        t5466 = inline10427
        var negative__273 bool = t5466 == 45
        var jp5468 int
        if negative__273 {
            jp5468 = 1
        } else {
            jp5468 = 0
        }
        var index__274 int = jp5468
        var result__275 int = 0
        var t5489 int
        var inline10424 int = _goml_runtime_core_string_len(value__272)
        t5489 = inline10424
        var t5490 bool = index__274 == t5489
        if t5490 {
            return Option__int_None{}
        } else {
            Loop_loop5475:
            for {
                var t5476 int
                var inline10422 int = _goml_runtime_core_string_len(value__272)
                t5476 = inline10422
                var t5477 bool = index__274 < t5476
                if t5477 {
                    var byte__276 uint8
                    var inline10420 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10420
                    var t5487 bool = byte__276 < 48
                    var jp5482 bool
                    if t5487 {
                        jp5482 = true
                    } else {
                        var t5488 bool = byte__276 > 57
                        jp5482 = t5488
                    }
                    if jp5482 {
                        return Option__int_None{}
                    } else {
                        var t5483 int = result__275 * 10
                        var t5484 uint8 = byte__276 - 48
                        var t5485 int = int(uint8(t5484))
                        var t5486 int = t5483 + t5485
                        result__275 = t5486
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5479 int = compound_old895 + compound_value896
                        index__274 = t5479
                        continue
                    }
                } else {
                    break Loop_loop5475
                }
            }
            var jp5472 int
            if negative__273 {
                var t5474 int = 0 - result__275
                jp5472 = t5474
            } else {
                jp5472 = result__275
            }
            var t5473 Option__int = Option__int_Some{
                _0: jp5472,
            }
            return t5473
        }
    }
}

func main0() struct{} {
    var mtmp172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6262 _goml_m_std_p_json_p_Value
    switch mtmp172.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x173 _goml_m_std_p_json_p_Value = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6262 = x173
        var mtmp176 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "name")
        switch mtmp176.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline10876 string = "missing name"
            var inline10877 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10876)
            _goml_runtime_core_string_println(inline10877)
            var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "version")
            switch mtmp181.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10891 string = "missing version"
                var inline10892 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10891)
                _goml_runtime_core_string_println(inline10892)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp183 Option__int
                switch x182.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline10902 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline10904 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10902)
                    mtmp183 = inline10904
                default:
                    mtmp183 = Option__int_None{}
                }
                switch mtmp183.(type) {
                case Option__int_None:
                    var inline10895 string = "invalid version"
                    var inline10896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10895)
                    _goml_runtime_core_string_println(inline10896)
                case Option__int_Some:
                    var x184 int = mtmp183.(Option__int_Some)._0
                    var inline10899 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                    _goml_runtime_core_string_println(inline10899)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "stable")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10906 string = "missing stable"
                var inline10907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10906)
                _goml_runtime_core_string_println(inline10907)
                var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                println__T_string(t6266)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11178 bool
                switch x187.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline10917 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11178 = inline10917
                    var inline10914 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11178)
                    _goml_runtime_core_string_println(inline10914)
                    var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                    println__T_string(t6266)
                    return struct{}{}
                default:
                    var inline10910 string = "invalid stable"
                    var inline10911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10910)
                    _goml_runtime_core_string_println(inline10911)
                    var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                    println__T_string(t6266)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x177 _goml_m_std_p_json_p_Value = mtmp176.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11184 string
            switch x177.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline10887 string = x177.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11184 = inline10887
                var inline10884 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11184)
                _goml_runtime_core_string_println(inline10884)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10891 string = "missing version"
                    var inline10892 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10891)
                    _goml_runtime_core_string_println(inline10892)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10902 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10904 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10902)
                        mtmp183 = inline10904
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10895 string = "invalid version"
                        var inline10896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10895)
                        _goml_runtime_core_string_println(inline10896)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10899 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10899)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10906 string = "missing stable"
                    var inline10907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10906)
                    _goml_runtime_core_string_println(inline10907)
                    var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                    println__T_string(t6266)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11178 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10917 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11178 = inline10917
                        var inline10914 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11178)
                        _goml_runtime_core_string_println(inline10914)
                        var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                        println__T_string(t6266)
                        return struct{}{}
                    default:
                        var inline10910 string = "invalid stable"
                        var inline10911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10910)
                        _goml_runtime_core_string_println(inline10911)
                        var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                        println__T_string(t6266)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline10880 string = "invalid name"
                var inline10881 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10880)
                _goml_runtime_core_string_println(inline10881)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10891 string = "missing version"
                    var inline10892 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10891)
                    _goml_runtime_core_string_println(inline10892)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10902 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10904 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10902)
                        mtmp183 = inline10904
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10895 string = "invalid version"
                        var inline10896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10895)
                        _goml_runtime_core_string_println(inline10896)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10899 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10899)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6262, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10906 string = "missing stable"
                    var inline10907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10906)
                    _goml_runtime_core_string_println(inline10907)
                    var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                    println__T_string(t6266)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11178 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10917 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11178 = inline10917
                        var inline10914 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11178)
                        _goml_runtime_core_string_println(inline10914)
                        var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                        println__T_string(t6266)
                        return struct{}{}
                    default:
                        var inline10910 string = "invalid stable"
                        var inline10911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10910)
                        _goml_runtime_core_string_println(inline10911)
                        var t6266 string = _goml_m_std_p_json_p_encode(jp6262)
                        println__T_string(t6266)
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
        var x174 string = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline10873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
        _goml_runtime_core_string_println(inline10873)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6282 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6282
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6322:
    for {
        var t6323 int
        var inline10928 int = _goml_runtime_core_string_len(x12)
        t6323 = inline10928
        var t6324 bool = index__26 < t6323
        if t6324 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6326 int = compound_old17 + x16
                index__26 = t6326
                continue
            } else {
                var t6328 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6328
            }
        } else {
            break Loop_loop6322
        }
    }
    var t6321 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6321
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t6358 string = _goml_runtime_core_int_to_string(self__32)
    return t6358
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline10938 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline10939 bool = inline10938._0
    var inline10940 rune = inline10938._1
    if inline10939 {
        return inline10940
    } else {
        var inline10943 rune = _goml_runtime_core_string_get("", -1)
        return inline10943
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__255 int) *ref_int_x {
    var t6443 *ref_int_x = ref__Ref_3int(value__255)
    return t6443
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__256 *ref_int_x) int {
    var t6446 int = ref_get__Ref_3int(self__256)
    return t6446
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__257 *ref_int_x, value__258 int) struct{} {
    ref_set__Ref_3int(self__257, value__258)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__34 rune) string {
    var inline10950 uint32 = uint32(rune(self__34))
    var inline10951 bool = utf8_valid_scalar(inline10950)
    if inline10951 {
        var inline10952 string = _goml_runtime_core_char_to_string(self__34)
        return inline10952
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t6511 int = _goml_runtime_core_string_len(self__36)
    return t6511
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t6514 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t6514
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline10955 bool = string_is_char_boundary(self__41, start__42)
    var inline10957 bool
    if inline10955 {
        var inline10960 bool = string_is_char_boundary(self__41, end__43)
        inline10957 = inline10960
    } else {
        inline10957 = false
    }
    if inline10957 {
        var inline10958 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline10958
    } else {
        var inline10959 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline10959
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline11002 bool = utf8_valid_scalar(value__2)
    if inline11002 {
        var inline11003 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11004 rune = inline11003._1
        var inline11006 Option__char = Option__char_Some{
            _0: inline11004,
        }
        return inline11006
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6623 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6623
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6628 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6628
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__174 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__175 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__174, elem__175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6664 string
    t6664 = value__1
    _goml_runtime_core_string_println(t6664)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6790 bool = index__6 < 0
    var jp6788 bool
    if t6790 {
        jp6788 = true
    } else {
        var t6791 bool = index__6 >= length__7
        jp6788 = t6791
    }
    if jp6788 {
        var inline11017 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11017
    } else {
        var t6675 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6675))
        var t6678 bool = first__8 < 128
        if t6678 {
            var inline11019 int = 1
            var inline11020 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11020.(type) {
            case Option__char_None:
                var inline11021 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11021
            case Option__char_Some:
                var inline11022 rune = inline11020.(Option__char_Some)._0
                var inline11024 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11022,
                    _2: inline11019,
                }
                return inline11024
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6682 bool = first__8 < 194
            if t6682 {
                var inline11026 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11026
            } else {
                var t6686 bool = first__8 < 224
                if t6686 {
                    var t6699 int = length__7 - index__6
                    var t6700 bool = t6699 < 2
                    if t6700 {
                        var inline11028 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11028
                    } else {
                        var t6688 int = index__6 + 1
                        var t6689 uint8
                        var inline11042 uint8 = _goml_runtime_core_string_byte_get(value__5, t6688)
                        t6689 = inline11042
                        var second__9 uint32 = uint32(uint8(t6689))
                        var t6692 bool
                        var inline11039 bool = second__9 < 128
                        if inline11039 {
                            t6692 = true
                        } else {
                            var inline11040 bool = second__9 > 191
                            t6692 = inline11040
                        }
                        if t6692 {
                            var inline11030 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11030
                        } else {
                            var t6694_rhs uint32 = 31
                            var t6694 uint32 = first__8 & t6694_rhs
                            var t6695_rhs int = 6
                            var t6695 uint32 = t6694 << t6695_rhs
                            var t6696_rhs uint32 = 63
                            var t6696 uint32 = second__9 & t6696_rhs
                            var t6697 uint32 = t6695 | t6696
                            var inline11032 int = 2
                            var inline11033 Option__char = __goml_builtin_char_from_uint32(t6697)
                            switch inline11033.(type) {
                            case Option__char_None:
                                var inline11034 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11034
                            case Option__char_Some:
                                var inline11035 rune = inline11033.(Option__char_Some)._0
                                var inline11037 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11035,
                                    _2: inline11032,
                                }
                                return inline11037
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6704 bool = first__8 < 240
                    if t6704 {
                        var t6737 int = length__7 - index__6
                        var t6738 bool = t6737 < 3
                        if t6738 {
                            var inline11044 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11044
                        } else {
                            var t6706 int = index__6 + 1
                            var t6707 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6706)
                            var second__10 uint32 = uint32(uint8(t6707))
                            var t6708 int = index__6 + 2
                            var t6709 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6708)
                            var third__11 uint32 = uint32(uint8(t6709))
                            var t6735 bool = utf8_invalid_continuation(second__10)
                            var jp6730 bool
                            if t6735 {
                                jp6730 = true
                            } else {
                                var inline11046 bool = third__11 < 128
                                if inline11046 {
                                    jp6730 = true
                                } else {
                                    var inline11047 bool = third__11 > 191
                                    jp6730 = inline11047
                                }
                            }
                            var jp6724 bool
                            if jp6730 {
                                jp6724 = true
                            } else {
                                var t6733 bool = first__8 == 224
                                if t6733 {
                                    var t6734 bool = second__10 < 160
                                    jp6724 = t6734
                                } else {
                                    jp6724 = false
                                }
                            }
                            var jp6713 bool
                            if jp6724 {
                                jp6713 = true
                            } else {
                                var t6727 bool = first__8 == 237
                                if t6727 {
                                    var t6728 bool = second__10 >= 160
                                    jp6713 = t6728
                                } else {
                                    jp6713 = false
                                }
                            }
                            if jp6713 {
                                var inline11049 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11049
                            } else {
                                var t6715_rhs uint32 = 15
                                var t6715 uint32 = first__8 & t6715_rhs
                                var t6716_rhs int = 12
                                var t6716 uint32 = t6715 << t6716_rhs
                                var t6717_rhs uint32 = 63
                                var t6717 uint32 = second__10 & t6717_rhs
                                var t6718_rhs int = 6
                                var t6718 uint32 = t6717 << t6718_rhs
                                var t6719 uint32 = t6716 | t6718
                                var t6720_rhs uint32 = 63
                                var t6720 uint32 = third__11 & t6720_rhs
                                var t6721 uint32 = t6719 | t6720
                                var inline11051 int = 3
                                var inline11052 Option__char = __goml_builtin_char_from_uint32(t6721)
                                switch inline11052.(type) {
                                case Option__char_None:
                                    var inline11053 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11053
                                case Option__char_Some:
                                    var inline11054 rune = inline11052.(Option__char_Some)._0
                                    var inline11056 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11054,
                                        _2: inline11051,
                                    }
                                    return inline11056
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6742 bool = first__8 < 245
                        if t6742 {
                            var t6783 int = length__7 - index__6
                            var t6784 bool = t6783 < 4
                            if t6784 {
                                var t6785 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6785
                            } else {
                                var t6744 int = index__6 + 1
                                var t6745 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6744)
                                var second__12 uint32 = uint32(uint8(t6745))
                                var t6746 int = index__6 + 2
                                var t6747 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6746)
                                var third__13 uint32 = uint32(uint8(t6747))
                                var t6748 int = index__6 + 3
                                var t6749 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6748)
                                var fourth__14 uint32 = uint32(uint8(t6749))
                                var t6781 bool = utf8_invalid_continuation(second__12)
                                var jp6779 bool
                                if t6781 {
                                    jp6779 = true
                                } else {
                                    var t6782 bool = utf8_invalid_continuation(third__13)
                                    jp6779 = t6782
                                }
                                var jp6773 bool
                                if jp6779 {
                                    jp6773 = true
                                } else {
                                    var t6780 bool = utf8_invalid_continuation(fourth__14)
                                    jp6773 = t6780
                                }
                                var jp6767 bool
                                if jp6773 {
                                    jp6767 = true
                                } else {
                                    var t6776 bool = first__8 == 240
                                    if t6776 {
                                        var t6777 bool = second__12 < 144
                                        jp6767 = t6777
                                    } else {
                                        jp6767 = false
                                    }
                                }
                                var jp6753 bool
                                if jp6767 {
                                    jp6753 = true
                                } else {
                                    var t6770 bool = first__8 == 244
                                    if t6770 {
                                        var t6771 bool = second__12 > 143
                                        jp6753 = t6771
                                    } else {
                                        jp6753 = false
                                    }
                                }
                                if jp6753 {
                                    var t6754 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6754
                                } else {
                                    var t6755_rhs uint32 = 7
                                    var t6755 uint32 = first__8 & t6755_rhs
                                    var t6756_rhs int = 18
                                    var t6756 uint32 = t6755 << t6756_rhs
                                    var t6757_rhs uint32 = 63
                                    var t6757 uint32 = second__12 & t6757_rhs
                                    var t6758_rhs int = 12
                                    var t6758 uint32 = t6757 << t6758_rhs
                                    var t6759 uint32 = t6756 | t6758
                                    var t6760_rhs uint32 = 63
                                    var t6760 uint32 = third__13 & t6760_rhs
                                    var t6761_rhs int = 6
                                    var t6761 uint32 = t6760 << t6761_rhs
                                    var t6762 uint32 = t6759 | t6761
                                    var t6763_rhs uint32 = 63
                                    var t6763 uint32 = fourth__14 & t6763_rhs
                                    var t6764 uint32 = t6762 | t6763
                                    var t6765 Tuple3_4bool_4char_3int = utf8_valid_decode(t6764, 4)
                                    return t6765
                                }
                            }
                        } else {
                            var t6786 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6786
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6811 uint32 = uint32(rune(value__29))
    var t6812 bool
    var inline11058 bool = t6811 <= 1114111
    if inline11058 {
        var inline11059 bool = t6811 >= 55296
        var inline11061 bool
        if inline11059 {
            var inline11063 bool = t6811 <= 57343
            inline11061 = inline11063
        } else {
            inline11061 = false
        }
        var inline11062 bool = !inline11061
        t6812 = inline11062
    } else {
        t6812 = false
    }
    if t6812 {
        var t6813 string = _goml_runtime_core_char_to_string(value__29)
        return t6813
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6827 bool = index__16 < 0
    var jp6819 bool
    if t6827 {
        jp6819 = true
    } else {
        var t6828 int
        var inline11065 int = _goml_runtime_core_string_len(value__15)
        t6828 = inline11065
        var t6829 bool = index__16 > t6828
        jp6819 = t6829
    }
    if jp6819 {
        return false
    } else {
        var t6822 int
        var inline11069 int = _goml_runtime_core_string_len(value__15)
        t6822 = inline11069
        var t6823 bool = index__16 == t6822
        if t6823 {
            return true
        } else {
            var t6824 uint8
            var inline11067 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6824 = inline11067
            var t6825_rhs uint8 = 192
            var t6825 uint8 = t6824 & t6825_rhs
            var t6826 bool = t6825 != 128
            return t6826
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6838 bool = string_is_char_boundary(value__21, start__22)
    var jp6835 bool
    if t6838 {
        var t6839 bool = string_is_char_boundary(value__21, end__23)
        jp6835 = t6839
    } else {
        jp6835 = false
    }
    if jp6835 {
        var t6836 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6836
    } else {
        var t6837 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6837
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t6865 bool
    var inline11073 bool = value__30 <= 1114111
    if inline11073 {
        var inline11074 bool = value__30 >= 55296
        var inline11076 bool
        if inline11074 {
            var inline11078 bool = value__30 <= 57343
            inline11076 = inline11078
        } else {
            inline11076 = false
        }
        var inline11077 bool = !inline11076
        t6865 = inline11077
    } else {
        t6865 = false
    }
    if t6865 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t6866 Option__char = Option__char_Some{
            _0: x24,
        }
        return t6866
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t6873 string = _goml_runtime_core_int_to_string(self__67)
    return t6873
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t6876 string = _goml_runtime_core_bool_to_string(self__64)
    return t6876
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6879 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6879
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11190 rune
    var inline11082 bool = utf8_valid_scalar(value__0)
    if inline11082 {
        var inline11083 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11084 rune = inline11083._1
        commute_field11190 = inline11084
        var t6885 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11190,
            _2: width__1,
        }
        return t6885
    } else {
        var inline11080 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11080
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6890 bool = value__3 < 128
    if t6890 {
        return true
    } else {
        var t6891 bool = value__3 > 191
        return t6891
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6896 bool = value__4 <= 1114111
    if t6896 {
        var t6900 bool = value__4 >= 55296
        var jp6898 bool
        if t6900 {
            var t6901 bool = value__4 <= 57343
            jp6898 = t6901
        } else {
            jp6898 = false
        }
        var t6899 bool = !jp6898
        return t6899
    } else {
        return false
    }
}

func main() {
    main0()
}
