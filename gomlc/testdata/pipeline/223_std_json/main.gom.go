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
    var inline8416 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline8416
    var t2676 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2676
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8431 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8431
    var t2690 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2690, length__5)
    var for_index1 int = 0
    Loop_loop2692:
    for {
        var t2693 bool = for_index1 < length__5
        if t2693 {
            var for_item3 int = for_index1
            var t2694 int = for_index1 + 1
            for_index1 = t2694
            var t2695 *_goml_vec_uint8 = self__3.values
            var t2696 uint8
            var inline8427 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2696 = inline8427
            vec_push__Vec_5uint8(t2695, t2696)
            continue
        } else {
            break Loop_loop2692
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2699 string
    var inline8433 string = char_to_string(value__8)
    t2699 = inline8433
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2699)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4640 string = "" + message__201
    var t4641 string = t4640 + " at byte "
    var t4642 *ref_int_x = value__200.index
    var t4643 int
    var inline9952 int = ref_get__Ref_3int(t4642)
    t4643 = inline9952
    var t4644 string
    var inline9950 string = _goml_runtime_core_int_to_string(t4643)
    t4644 = inline9950
    var t4645 string = t4641 + t4644
    return t4645
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4660:
    for {
        var t4668 *ref_int_x = value__203.index
        var t4669 int
        var inline9973 int = ref_get__Ref_3int(t4668)
        t4669 = inline9973
        var t4670 string = value__203.input
        var t4671 int
        var inline9971 int = _goml_runtime_core_string_len(t4670)
        t4671 = inline9971
        var t4672 bool = t4669 < t4671
        var jp4662 bool
        if t4672 {
            var t4673 string = value__203.input
            var t4674 *ref_int_x = value__203.index
            var t4675 int
            var inline9965 int = ref_get__Ref_3int(t4674)
            t4675 = inline9965
            var t4676 uint8
            var inline9963 uint8 = _goml_runtime_core_string_byte_get(t4673, t4675)
            t4676 = inline9963
            var inline9954 bool = t4676 == 9
            var inline9956 bool
            if inline9954 {
                inline9956 = true
            } else {
                var inline9961 bool = t4676 == 10
                inline9956 = inline9961
            }
            var inline9958 bool
            if inline9956 {
                inline9958 = true
            } else {
                var inline9960 bool = t4676 == 13
                inline9958 = inline9960
            }
            if inline9958 {
                jp4662 = true
            } else {
                var inline9959 bool = t4676 == 32
                jp4662 = inline9959
            }
        } else {
            jp4662 = false
        }
        if jp4662 {
            var t4663 *ref_int_x = value__203.index
            var t4664 *ref_int_x = value__203.index
            var t4665 int
            var inline9969 int = ref_get__Ref_3int(t4664)
            t4665 = inline9969
            var t4666 int = t4665 + 1
            ref_set__Ref_3int(t4663, t4666)
            continue
        } else {
            break Loop_loop4660
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4707 bool = value__204 >= 48
    var jp4683 bool
    if t4707 {
        var t4708 bool = value__204 <= 57
        jp4683 = t4708
    } else {
        jp4683 = false
    }
    if jp4683 {
        var t4684 uint8 = value__204 - 48
        var t4685 uint32 = uint32(uint8(t4684))
        var t4686 Option__uint32 = Option__uint32_Some{
            _0: t4685,
        }
        return t4686
    } else {
        var t4705 bool = value__204 >= 65
        var jp4690 bool
        if t4705 {
            var t4706 bool = value__204 <= 70
            jp4690 = t4706
        } else {
            jp4690 = false
        }
        if jp4690 {
            var t4691 uint8 = value__204 - 65
            var t4692 uint8 = t4691 + 10
            var t4693 uint32 = uint32(uint8(t4692))
            var t4694 Option__uint32 = Option__uint32_Some{
                _0: t4693,
            }
            return t4694
        } else {
            var t4703 bool = value__204 >= 97
            var jp4698 bool
            if t4703 {
                var t4704 bool = value__204 <= 102
                jp4698 = t4704
            } else {
                jp4698 = false
            }
            if jp4698 {
                var t4699 uint8 = value__204 - 97
                var t4700 uint8 = t4699 + 10
                var t4701 uint32 = uint32(uint8(t4700))
                var t4702 Option__uint32 = Option__uint32_Some{
                    _0: t4701,
                }
                return t4702
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4713 *ref_int_x = value__205.index
    var t4714 int
    var inline10001 int = ref_get__Ref_3int(t4713)
    t4714 = inline10001
    var t4715 int = t4714 + 4
    var t4716 string = value__205.input
    var t4717 int
    var inline9999 int = _goml_runtime_core_string_len(t4716)
    t4717 = inline9999
    var t4718 bool = t4715 > t4717
    if t4718 {
        var t4719 string
        var inline9975 string = "incomplete unicode escape"
        var inline9976 string = "" + inline9975
        var inline9977 string = inline9976 + " at byte "
        var inline9978 *ref_int_x = value__205.index
        var inline9979 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9978)
        var inline9980 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9979)
        var inline9981 string = inline9977 + inline9980
        t4719 = inline9981
        var t4720 Result__uint32__string = Result__uint32__string_Err{
            _0: t4719,
        }
        return t4720
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4727:
        for {
            var t4728 bool = for_index744 < for_limit745
            if t4728 {
                var for_item746 int = for_index744
                var t4729 int = for_index744 + 1
                for_index744 = t4729
                var t4730 string = value__205.input
                var t4731 *ref_int_x = value__205.index
                var t4732 int
                var inline9993 int = ref_get__Ref_3int(t4731)
                t4732 = inline9993
                var t4733 int = t4732 + for_item746
                var t4734 uint8
                var inline9991 uint8 = _goml_runtime_core_string_byte_get(t4730, t4733)
                t4734 = inline9991
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4734)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4736 string
                    var inline9983 string = "invalid unicode escape"
                    var inline9984 string = "" + inline9983
                    var inline9985 string = inline9984 + " at byte "
                    var inline9986 *ref_int_x = value__205.index
                    var inline9987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9986)
                    var inline9988 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9987)
                    var inline9989 string = inline9985 + inline9988
                    t4736 = inline9989
                    var t4737 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4736,
                    }
                    return t4737
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4738 uint32 = result__206 * 16
                    var t4739 uint32 = t4738 + x749
                    result__206 = t4739
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4727
            }
        }
        var t4722 *ref_int_x = value__205.index
        var t4723 *ref_int_x = value__205.index
        var t4724 int
        var inline9997 int = ref_get__Ref_3int(t4723)
        t4724 = inline9997
        var t4725 int = t4724 + 4
        ref_set__Ref_3int(t4722, t4725)
        var t4726 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4726
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline10014 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline10014
    switch mtmp753.(type) {
    case Option__char_None:
        var t4744 string
        var inline10003 string = "invalid unicode codepoint"
        var inline10004 string = "" + inline10003
        var inline10005 string = inline10004 + " at byte "
        var inline10006 *ref_int_x = value__209.index
        var inline10007 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10006)
        var inline10008 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10007)
        var inline10009 string = inline10005 + inline10008
        t4744 = inline10009
        var t4745 Result__unit__string = Result__unit__string_Err{
            _0: t4744,
        }
        return t4745
    case Option__char_Some:
        var x754 rune = mtmp753.(Option__char_Some)._0
        var inline10011 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10011)
        var t4746 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4746
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4750 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4750 = x757
        var t4810 bool = jp4750 >= 55296
        var jp4754 bool
        if t4810 {
            var t4811 bool = jp4750 <= 56319
            jp4754 = t4811
        } else {
            jp4754 = false
        }
        if jp4754 {
            var t4790 *ref_int_x = value__213.index
            var t4791 int
            var inline10054 int = ref_get__Ref_3int(t4790)
            t4791 = inline10054
            var t4792 int = t4791 + 2
            var t4793 string = value__213.input
            var t4794 int
            var inline10052 int = _goml_runtime_core_string_len(t4793)
            t4794 = inline10052
            var t4795 bool = t4792 > t4794
            var jp4783 bool
            if t4795 {
                jp4783 = true
            } else {
                var t4796 string = value__213.input
                var t4797 *ref_int_x = value__213.index
                var t4798 int
                var inline10018 int = ref_get__Ref_3int(t4797)
                t4798 = inline10018
                var t4799 uint8
                var inline10016 uint8 = _goml_runtime_core_string_byte_get(t4796, t4798)
                t4799 = inline10016
                var t4800 bool = t4799 != 92
                jp4783 = t4800
            }
            var jp4758 bool
            if jp4783 {
                jp4758 = true
            } else {
                var t4784 string = value__213.input
                var t4785 *ref_int_x = value__213.index
                var t4786 int
                var inline10022 int = ref_get__Ref_3int(t4785)
                t4786 = inline10022
                var t4787 int = t4786 + 1
                var t4788 uint8
                var inline10020 uint8 = _goml_runtime_core_string_byte_get(t4784, t4787)
                t4788 = inline10020
                var t4789 bool = t4788 != 117
                jp4758 = t4789
            }
            if jp4758 {
                var t4759 string
                var inline10024 string = "missing low surrogate"
                var inline10025 string = "" + inline10024
                var inline10026 string = inline10025 + " at byte "
                var inline10027 *ref_int_x = value__213.index
                var inline10028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10027)
                var inline10029 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10028)
                var inline10030 string = inline10026 + inline10029
                t4759 = inline10030
                var t4760 Result__unit__string = Result__unit__string_Err{
                    _0: t4759,
                }
                return t4760
            } else {
                var t4761 *ref_int_x = value__213.index
                var t4762 *ref_int_x = value__213.index
                var t4763 int
                var inline10050 int = ref_get__Ref_3int(t4762)
                t4763 = inline10050
                var t4764 int = t4763 + 2
                ref_set__Ref_3int(t4761, t4764)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4766 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4766 = x761
                    var t4779 bool = jp4766 < 56320
                    var jp4770 bool
                    if t4779 {
                        jp4770 = true
                    } else {
                        var t4780 bool = jp4766 > 57343
                        jp4770 = t4780
                    }
                    if jp4770 {
                        var t4771 string
                        var inline10032 string = "invalid low surrogate"
                        var inline10033 string = "" + inline10032
                        var inline10034 string = inline10033 + " at byte "
                        var inline10035 *ref_int_x = value__213.index
                        var inline10036 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10035)
                        var inline10037 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10036)
                        var inline10038 string = inline10034 + inline10037
                        t4771 = inline10038
                        var t4772 Result__unit__string = Result__unit__string_Err{
                            _0: t4771,
                        }
                        return t4772
                    } else {
                        var t4773 uint32 = jp4750 - 55296
                        var t4774 uint32 = t4773 * 1024
                        var t4775 uint32 = 65536 + t4774
                        var t4776 uint32 = t4775 + jp4766
                        var t4777 uint32 = t4776 - 56320
                        var inline10040 Option__char = char_from_uint32(t4777)
                        switch inline10040.(type) {
                        case Option__char_None:
                            var inline10041 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10042 Result__unit__string = Result__unit__string_Err{
                                _0: inline10041,
                            }
                            return inline10042
                        case Option__char_Some:
                            var inline10043 rune = inline10040.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10043)
                            var inline10046 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline10046
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4781 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4781
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4808 bool = jp4750 >= 56320
            var jp4804 bool
            if t4808 {
                var t4809 bool = jp4750 <= 57343
                jp4804 = t4809
            } else {
                jp4804 = false
            }
            if jp4804 {
                var t4805 string
                var inline10056 string = "unexpected low surrogate"
                var inline10057 string = "" + inline10056
                var inline10058 string = inline10057 + " at byte "
                var inline10059 *ref_int_x = value__213.index
                var inline10060 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10059)
                var inline10061 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10060)
                var inline10062 string = inline10058 + inline10061
                t4805 = inline10062
                var t4806 Result__unit__string = Result__unit__string_Err{
                    _0: t4805,
                }
                return t4806
            } else {
                var t4807 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4750)
                return t4807
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4812 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4812
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4928 *ref_int_x = value__217.index
    var t4929 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4928)
    var t4930 string = value__217.input
    var t4931 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4930)
    var t4932 bool = t4929 >= t4931
    var jp4920 bool
    if t4932 {
        jp4920 = true
    } else {
        var t4933 string = value__217.input
        var t4934 *ref_int_x = value__217.index
        var t4935 int
        var inline10066 int = ref_get__Ref_3int(t4934)
        t4935 = inline10066
        var t4936 uint8
        var inline10064 uint8 = _goml_runtime_core_string_byte_get(t4933, t4935)
        t4936 = inline10064
        var t4937 bool = t4936 != 34
        jp4920 = t4937
    }
    if jp4920 {
        var t4921 string
        var inline10068 string = "expected string"
        var inline10069 string = "" + inline10068
        var inline10070 string = inline10069 + " at byte "
        var inline10071 *ref_int_x = value__217.index
        var inline10072 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10071)
        var inline10073 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10072)
        var inline10074 string = inline10070 + inline10073
        t4921 = inline10074
        var t4922 Result__string__string = Result__string__string_Err{
            _0: t4921,
        }
        return t4922
    } else {
        var t4923 *ref_int_x = value__217.index
        var t4924 *ref_int_x = value__217.index
        var t4925 int
        var inline10078 int = ref_get__Ref_3int(t4924)
        t4925 = inline10078
        var t4926 int = t4925 + 1
        ref_set__Ref_3int(t4923, t4926)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4816 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4816)
        Loop_loop4820:
        for {
            var t4821 *ref_int_x = value__217.index
            var t4822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4821)
            var t4823 string = value__217.input
            var t4824 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4823)
            var t4825 bool = t4822 < t4824
            if t4825 {
                var t4826 string = value__217.input
                var t4827 *ref_int_x = value__217.index
                var t4828 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4827)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4826, t4828)
                var t4830 bool = byte__220 == 34
                if t4830 {
                    var t4838 *ref_int_x = value__217.index
                    var t4839 int
                    var inline10093 int = ref_get__Ref_3int(t4838)
                    t4839 = inline10093
                    var t4840 bool = segment__219 < t4839
                    if t4840 {
                        var t4841 string = value__217.input
                        var t4842 *ref_int_x = value__217.index
                        var t4843 int
                        var inline10082 int = ref_get__Ref_3int(t4842)
                        t4843 = inline10082
                        var t4844 string
                        var inline10080 string = string_byte_slice(t4841, segment__219, t4843)
                        t4844 = inline10080
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4844)
                    } else {}
                    var t4832 *ref_int_x = value__217.index
                    var t4833 *ref_int_x = value__217.index
                    var t4834 int
                    var inline10091 int = ref_get__Ref_3int(t4833)
                    t4834 = inline10091
                    var t4835 int = t4834 + 1
                    ref_set__Ref_3int(t4832, t4835)
                    var t4836 string
                    var inline10084 *_goml_vec_uint8 = builder__218.values
                    var inline10085 Tuple2_4bool_6string = string_from_utf8(inline10084)
                    var inline10086 string = inline10085._1
                    t4836 = inline10086
                    var t4837 Result__string__string = Result__string__string_Ok{
                        _0: t4836,
                    }
                    return t4837
                } else {
                    var t4847 bool = byte__220 == 92
                    if t4847 {
                        var t4902 *ref_int_x = value__217.index
                        var t4903 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4902)
                        var t4904 bool = segment__219 < t4903
                        if t4904 {
                            var t4905 string = value__217.input
                            var t4906 *ref_int_x = value__217.index
                            var t4907 int
                            var inline10097 int = ref_get__Ref_3int(t4906)
                            t4907 = inline10097
                            var t4908 string
                            var inline10095 string = string_byte_slice(t4905, segment__219, t4907)
                            t4908 = inline10095
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4908)
                        } else {}
                        var t4849 *ref_int_x = value__217.index
                        var t4850 *ref_int_x = value__217.index
                        var t4851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4850)
                        var t4852 int = t4851 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4849, t4852)
                        var t4895 *ref_int_x = value__217.index
                        var t4896 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4895)
                        var t4897 string = value__217.input
                        var t4898 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4897)
                        var t4899 bool = t4896 >= t4898
                        if t4899 {
                            var t4900 string
                            var inline10099 string = "incomplete escape"
                            var inline10100 string = "" + inline10099
                            var inline10101 string = inline10100 + " at byte "
                            var inline10102 *ref_int_x = value__217.index
                            var inline10103 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10102)
                            var inline10104 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10103)
                            var inline10105 string = inline10101 + inline10104
                            t4900 = inline10105
                            var t4901 Result__string__string = Result__string__string_Err{
                                _0: t4900,
                            }
                            return t4901
                        } else {
                            var t4854 string = value__217.input
                            var t4855 *ref_int_x = value__217.index
                            var t4856 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4855)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4854, t4856)
                            var t4857 *ref_int_x = value__217.index
                            var t4858 *ref_int_x = value__217.index
                            var t4859 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4858)
                            var t4860 int = t4859 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4857, t4860)
                            var t4864 bool = escape__221 == 34
                            if t4864 {
                                var inline10107 rune = 34
                                var inline10108 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10107)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10108)
                                var t4862 *ref_int_x = value__217.index
                                var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                segment__219 = t4863
                                continue
                            } else {
                                var t4867 bool = escape__221 == 92
                                if t4867 {
                                    var inline10111 rune = 92
                                    var inline10112 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10111)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10112)
                                    var t4862 *ref_int_x = value__217.index
                                    var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                    segment__219 = t4863
                                    continue
                                } else {
                                    var t4870 bool = escape__221 == 47
                                    if t4870 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4862 *ref_int_x = value__217.index
                                        var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                        segment__219 = t4863
                                        continue
                                    } else {
                                        var t4873 bool = escape__221 == 98
                                        if t4873 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4862 *ref_int_x = value__217.index
                                                var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                segment__219 = t4863
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4862 *ref_int_x = value__217.index
                                                var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                segment__219 = t4863
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4877 bool = escape__221 == 102
                                            if t4877 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4862 *ref_int_x = value__217.index
                                                    var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                    segment__219 = t4863
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4862 *ref_int_x = value__217.index
                                                    var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                    segment__219 = t4863
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4881 bool = escape__221 == 110
                                                if t4881 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4862 *ref_int_x = value__217.index
                                                    var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                    segment__219 = t4863
                                                    continue
                                                } else {
                                                    var t4884 bool = escape__221 == 114
                                                    if t4884 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4862 *ref_int_x = value__217.index
                                                        var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                        segment__219 = t4863
                                                        continue
                                                    } else {
                                                        var t4887 bool = escape__221 == 116
                                                        if t4887 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4862 *ref_int_x = value__217.index
                                                            var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                            segment__219 = t4863
                                                            continue
                                                        } else {
                                                            var t4890 bool = escape__221 == 117
                                                            if t4890 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4862 *ref_int_x = value__217.index
                                                                    var t4863 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4862)
                                                                    segment__219 = t4863
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4892 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4892
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4893 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4894 Result__string__string = Result__string__string_Err{
                                                                    _0: t4893,
                                                                }
                                                                return t4894
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
                        var t4911 bool = byte__220 < 32
                        if t4911 {
                            var t4912 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4913 Result__string__string = Result__string__string_Err{
                                _0: t4912,
                            }
                            return t4913
                        } else {
                            var t4914 *ref_int_x = value__217.index
                            var t4915 *ref_int_x = value__217.index
                            var t4916 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4915)
                            var t4917 int = t4916 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4914, t4917)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4820
            }
        }
        var t4818 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4819 Result__string__string = Result__string__string_Err{
            _0: t4818,
        }
        return t4819
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4946 *ref_int_x = value__225.index
    var start__226 int
    var inline10132 int = ref_get__Ref_3int(t4946)
    start__226 = inline10132
    Loop_loop4951:
    for {
        var t4959 *ref_int_x = value__225.index
        var t4960 int
        var inline10128 int = ref_get__Ref_3int(t4959)
        t4960 = inline10128
        var t4961 string = value__225.input
        var t4962 int
        var inline10126 int = _goml_runtime_core_string_len(t4961)
        t4962 = inline10126
        var t4963 bool = t4960 < t4962
        var jp4953 bool
        if t4963 {
            var t4964 string = value__225.input
            var t4965 *ref_int_x = value__225.index
            var t4966 int
            var inline10120 int = ref_get__Ref_3int(t4965)
            t4966 = inline10120
            var t4967 uint8
            var inline10118 uint8 = _goml_runtime_core_string_byte_get(t4964, t4966)
            t4967 = inline10118
            var inline10115 bool = t4967 >= 48
            if inline10115 {
                var inline10116 bool = t4967 <= 57
                jp4953 = inline10116
            } else {
                jp4953 = false
            }
        } else {
            jp4953 = false
        }
        if jp4953 {
            var t4954 *ref_int_x = value__225.index
            var t4955 *ref_int_x = value__225.index
            var t4956 int
            var inline10124 int = ref_get__Ref_3int(t4955)
            t4956 = inline10124
            var t4957 int = t4956 + 1
            ref_set__Ref_3int(t4954, t4957)
            continue
        } else {
            break Loop_loop4951
        }
    }
    var t4948 *ref_int_x = value__225.index
    var t4949 int
    var inline10130 int = ref_get__Ref_3int(t4948)
    t4949 = inline10130
    var t4950 bool = t4949 > start__226
    return t4950
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4971 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4971)
    var t5092 string = value__227.input
    var t5093 *ref_int_x = value__227.index
    var t5094 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5093)
    var t5095 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5092, t5094)
    var t5096 bool = t5095 == 45
    if t5096 {
        var t5097 *ref_int_x = value__227.index
        var t5098 *ref_int_x = value__227.index
        var t5099 int
        var inline10136 int = ref_get__Ref_3int(t5098)
        t5099 = inline10136
        var t5100 int = t5099 + 1
        ref_set__Ref_3int(t5097, t5100)
    } else {}
    var t5055 *ref_int_x = value__227.index
    var t5056 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5055)
    var t5057 string = value__227.input
    var t5058 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5057)
    var t5059 bool = t5056 >= t5058
    if t5059 {
        var t5060 string
        var inline10138 string = "incomplete number"
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
        var t5063 string = value__227.input
        var t5064 *ref_int_x = value__227.index
        var t5065 int
        var inline10179 int = ref_get__Ref_3int(t5064)
        t5065 = inline10179
        var t5066 uint8
        var inline10177 uint8 = _goml_runtime_core_string_byte_get(t5063, t5065)
        t5066 = inline10177
        var t5067 bool = t5066 == 48
        if t5067 {
            var t5068 *ref_int_x = value__227.index
            var t5069 *ref_int_x = value__227.index
            var t5070 int
            var inline10167 int = ref_get__Ref_3int(t5069)
            t5070 = inline10167
            var t5071 int = t5070 + 1
            ref_set__Ref_3int(t5068, t5071)
            var t5077 *ref_int_x = value__227.index
            var t5078 int
            var inline10163 int = ref_get__Ref_3int(t5077)
            t5078 = inline10163
            var t5079 string = value__227.input
            var t5080 int
            var inline10161 int = _goml_runtime_core_string_len(t5079)
            t5080 = inline10161
            var t5081 bool = t5078 < t5080
            var jp5074 bool
            if t5081 {
                var t5082 string = value__227.input
                var t5083 *ref_int_x = value__227.index
                var t5084 int
                var inline10151 int = ref_get__Ref_3int(t5083)
                t5084 = inline10151
                var t5085 uint8
                var inline10149 uint8 = _goml_runtime_core_string_byte_get(t5082, t5084)
                t5085 = inline10149
                var inline10146 bool = t5085 >= 48
                if inline10146 {
                    var inline10147 bool = t5085 <= 57
                    jp5074 = inline10147
                } else {
                    jp5074 = false
                }
            } else {
                jp5074 = false
            }
            if jp5074 {
                var t5075 string
                var inline10153 string = "invalid leading zero"
                var inline10154 string = "" + inline10153
                var inline10155 string = inline10154 + " at byte "
                var inline10156 *ref_int_x = value__227.index
                var inline10157 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10156)
                var inline10158 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10157)
                var inline10159 string = inline10155 + inline10158
                t5075 = inline10159
                var t5076 Result__string__string = Result__string__string_Err{
                    _0: t5075,
                }
                return t5076
            } else {
                var t5045 *ref_int_x = value__227.index
                var t5046 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5045)
                var t5047 string = value__227.input
                var t5048 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5047)
                var t5049 bool = t5046 < t5048
                var jp5035 bool
                if t5049 {
                    var t5050 string = value__227.input
                    var t5051 *ref_int_x = value__227.index
                    var t5052 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5051)
                    var t5053 uint8
                    var inline10181 uint8 = _goml_runtime_core_string_byte_get(t5050, t5052)
                    t5053 = inline10181
                    var t5054 bool = t5053 == 46
                    jp5035 = t5054
                } else {
                    jp5035 = false
                }
                if jp5035 {
                    var t5036 *ref_int_x = value__227.index
                    var t5037 *ref_int_x = value__227.index
                    var t5038 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5037)
                    var t5039 int = t5038 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5036, t5039)
                    var t5041 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5042 bool = !t5041
                    if t5042 {
                        var t5043 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5044 Result__string__string = Result__string__string_Err{
                            _0: t5043,
                        }
                        return t5044
                    } else {
                        var t5017 *ref_int_x = value__227.index
                        var t5018 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5017)
                        var t5019 string = value__227.input
                        var t5020 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5019)
                        var t5021 bool = t5018 < t5020
                        var jp4982 bool
                        if t5021 {
                            var t5024 string = value__227.input
                            var t5025 *ref_int_x = value__227.index
                            var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                            var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                            var t5028 bool = t5027 == 101
                            if t5028 {
                                jp4982 = true
                            } else {
                                var t5029 string = value__227.input
                                var t5030 *ref_int_x = value__227.index
                                var t5031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5030)
                                var t5032 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5029, t5031)
                                var t5033 bool = t5032 == 69
                                jp4982 = t5033
                            }
                        } else {
                            jp4982 = false
                        }
                        if jp4982 {
                            var t4983 *ref_int_x = value__227.index
                            var t4984 *ref_int_x = value__227.index
                            var t4985 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4984)
                            var t4986 int = t4985 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4983, t4986)
                            var t5000 *ref_int_x = value__227.index
                            var t5001 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5000)
                            var t5002 string = value__227.input
                            var t5003 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5002)
                            var t5004 bool = t5001 < t5003
                            var jp4994 bool
                            if t5004 {
                                var t5007 string = value__227.input
                                var t5008 *ref_int_x = value__227.index
                                var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                                var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                                var t5011 bool = t5010 == 43
                                if t5011 {
                                    jp4994 = true
                                } else {
                                    var t5012 string = value__227.input
                                    var t5013 *ref_int_x = value__227.index
                                    var t5014 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5013)
                                    var t5015 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5012, t5014)
                                    var t5016 bool = t5015 == 45
                                    jp4994 = t5016
                                }
                            } else {
                                jp4994 = false
                            }
                            if jp4994 {
                                var t4995 *ref_int_x = value__227.index
                                var t4996 *ref_int_x = value__227.index
                                var t4997 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4996)
                                var t4998 int = t4997 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4995, t4998)
                            } else {}
                            var t4989 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4990 bool = !t4989
                            if t4990 {
                                var t4991 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4992 Result__string__string = Result__string__string_Err{
                                    _0: t4991,
                                }
                                return t4992
                            } else {
                                var t4976 string = value__227.input
                                var t4977 *ref_int_x = value__227.index
                                var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                                var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                                var t4980 Result__string__string = Result__string__string_Ok{
                                    _0: t4979,
                                }
                                return t4980
                            }
                        } else {
                            var t4976 string = value__227.input
                            var t4977 *ref_int_x = value__227.index
                            var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                            var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                            var t4980 Result__string__string = Result__string__string_Ok{
                                _0: t4979,
                            }
                            return t4980
                        }
                    }
                } else {
                    var t5017 *ref_int_x = value__227.index
                    var t5018 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5017)
                    var t5019 string = value__227.input
                    var t5020 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5019)
                    var t5021 bool = t5018 < t5020
                    var jp4982 bool
                    if t5021 {
                        var t5024 string = value__227.input
                        var t5025 *ref_int_x = value__227.index
                        var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                        var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                        var t5028 bool = t5027 == 101
                        if t5028 {
                            jp4982 = true
                        } else {
                            var t5029 string = value__227.input
                            var t5030 *ref_int_x = value__227.index
                            var t5031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5030)
                            var t5032 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5029, t5031)
                            var t5033 bool = t5032 == 69
                            jp4982 = t5033
                        }
                    } else {
                        jp4982 = false
                    }
                    if jp4982 {
                        var t4983 *ref_int_x = value__227.index
                        var t4984 *ref_int_x = value__227.index
                        var t4985 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4984)
                        var t4986 int = t4985 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4983, t4986)
                        var t5000 *ref_int_x = value__227.index
                        var t5001 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5000)
                        var t5002 string = value__227.input
                        var t5003 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5002)
                        var t5004 bool = t5001 < t5003
                        var jp4994 bool
                        if t5004 {
                            var t5007 string = value__227.input
                            var t5008 *ref_int_x = value__227.index
                            var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                            var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                            var t5011 bool = t5010 == 43
                            if t5011 {
                                jp4994 = true
                            } else {
                                var t5012 string = value__227.input
                                var t5013 *ref_int_x = value__227.index
                                var t5014 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5013)
                                var t5015 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5012, t5014)
                                var t5016 bool = t5015 == 45
                                jp4994 = t5016
                            }
                        } else {
                            jp4994 = false
                        }
                        if jp4994 {
                            var t4995 *ref_int_x = value__227.index
                            var t4996 *ref_int_x = value__227.index
                            var t4997 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4996)
                            var t4998 int = t4997 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4995, t4998)
                        } else {}
                        var t4989 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4990 bool = !t4989
                        if t4990 {
                            var t4991 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4992 Result__string__string = Result__string__string_Err{
                                _0: t4991,
                            }
                            return t4992
                        } else {
                            var t4976 string = value__227.input
                            var t4977 *ref_int_x = value__227.index
                            var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                            var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                            var t4980 Result__string__string = Result__string__string_Ok{
                                _0: t4979,
                            }
                            return t4980
                        }
                    } else {
                        var t4976 string = value__227.input
                        var t4977 *ref_int_x = value__227.index
                        var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                        var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                        var t4980 Result__string__string = Result__string__string_Ok{
                            _0: t4979,
                        }
                        return t4980
                    }
                }
            }
        } else {
            var t5088 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5089 bool = !t5088
            if t5089 {
                var t5090 string
                var inline10169 string = "expected number"
                var inline10170 string = "" + inline10169
                var inline10171 string = inline10170 + " at byte "
                var inline10172 *ref_int_x = value__227.index
                var inline10173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10172)
                var inline10174 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10173)
                var inline10175 string = inline10171 + inline10174
                t5090 = inline10175
                var t5091 Result__string__string = Result__string__string_Err{
                    _0: t5090,
                }
                return t5091
            } else {
                var t5045 *ref_int_x = value__227.index
                var t5046 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5045)
                var t5047 string = value__227.input
                var t5048 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5047)
                var t5049 bool = t5046 < t5048
                var jp5035 bool
                if t5049 {
                    var t5050 string = value__227.input
                    var t5051 *ref_int_x = value__227.index
                    var t5052 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5051)
                    var t5053 uint8
                    var inline10181 uint8 = _goml_runtime_core_string_byte_get(t5050, t5052)
                    t5053 = inline10181
                    var t5054 bool = t5053 == 46
                    jp5035 = t5054
                } else {
                    jp5035 = false
                }
                if jp5035 {
                    var t5036 *ref_int_x = value__227.index
                    var t5037 *ref_int_x = value__227.index
                    var t5038 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5037)
                    var t5039 int = t5038 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5036, t5039)
                    var t5041 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5042 bool = !t5041
                    if t5042 {
                        var t5043 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5044 Result__string__string = Result__string__string_Err{
                            _0: t5043,
                        }
                        return t5044
                    } else {
                        var t5017 *ref_int_x = value__227.index
                        var t5018 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5017)
                        var t5019 string = value__227.input
                        var t5020 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5019)
                        var t5021 bool = t5018 < t5020
                        var jp4982 bool
                        if t5021 {
                            var t5024 string = value__227.input
                            var t5025 *ref_int_x = value__227.index
                            var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                            var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                            var t5028 bool = t5027 == 101
                            if t5028 {
                                jp4982 = true
                            } else {
                                var t5029 string = value__227.input
                                var t5030 *ref_int_x = value__227.index
                                var t5031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5030)
                                var t5032 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5029, t5031)
                                var t5033 bool = t5032 == 69
                                jp4982 = t5033
                            }
                        } else {
                            jp4982 = false
                        }
                        if jp4982 {
                            var t4983 *ref_int_x = value__227.index
                            var t4984 *ref_int_x = value__227.index
                            var t4985 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4984)
                            var t4986 int = t4985 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4983, t4986)
                            var t5000 *ref_int_x = value__227.index
                            var t5001 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5000)
                            var t5002 string = value__227.input
                            var t5003 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5002)
                            var t5004 bool = t5001 < t5003
                            var jp4994 bool
                            if t5004 {
                                var t5007 string = value__227.input
                                var t5008 *ref_int_x = value__227.index
                                var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                                var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                                var t5011 bool = t5010 == 43
                                if t5011 {
                                    jp4994 = true
                                } else {
                                    var t5012 string = value__227.input
                                    var t5013 *ref_int_x = value__227.index
                                    var t5014 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5013)
                                    var t5015 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5012, t5014)
                                    var t5016 bool = t5015 == 45
                                    jp4994 = t5016
                                }
                            } else {
                                jp4994 = false
                            }
                            if jp4994 {
                                var t4995 *ref_int_x = value__227.index
                                var t4996 *ref_int_x = value__227.index
                                var t4997 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4996)
                                var t4998 int = t4997 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4995, t4998)
                            } else {}
                            var t4989 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4990 bool = !t4989
                            if t4990 {
                                var t4991 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4992 Result__string__string = Result__string__string_Err{
                                    _0: t4991,
                                }
                                return t4992
                            } else {
                                var t4976 string = value__227.input
                                var t4977 *ref_int_x = value__227.index
                                var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                                var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                                var t4980 Result__string__string = Result__string__string_Ok{
                                    _0: t4979,
                                }
                                return t4980
                            }
                        } else {
                            var t4976 string = value__227.input
                            var t4977 *ref_int_x = value__227.index
                            var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                            var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                            var t4980 Result__string__string = Result__string__string_Ok{
                                _0: t4979,
                            }
                            return t4980
                        }
                    }
                } else {
                    var t5017 *ref_int_x = value__227.index
                    var t5018 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5017)
                    var t5019 string = value__227.input
                    var t5020 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5019)
                    var t5021 bool = t5018 < t5020
                    var jp4982 bool
                    if t5021 {
                        var t5024 string = value__227.input
                        var t5025 *ref_int_x = value__227.index
                        var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                        var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                        var t5028 bool = t5027 == 101
                        if t5028 {
                            jp4982 = true
                        } else {
                            var t5029 string = value__227.input
                            var t5030 *ref_int_x = value__227.index
                            var t5031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5030)
                            var t5032 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5029, t5031)
                            var t5033 bool = t5032 == 69
                            jp4982 = t5033
                        }
                    } else {
                        jp4982 = false
                    }
                    if jp4982 {
                        var t4983 *ref_int_x = value__227.index
                        var t4984 *ref_int_x = value__227.index
                        var t4985 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4984)
                        var t4986 int = t4985 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4983, t4986)
                        var t5000 *ref_int_x = value__227.index
                        var t5001 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5000)
                        var t5002 string = value__227.input
                        var t5003 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5002)
                        var t5004 bool = t5001 < t5003
                        var jp4994 bool
                        if t5004 {
                            var t5007 string = value__227.input
                            var t5008 *ref_int_x = value__227.index
                            var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                            var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                            var t5011 bool = t5010 == 43
                            if t5011 {
                                jp4994 = true
                            } else {
                                var t5012 string = value__227.input
                                var t5013 *ref_int_x = value__227.index
                                var t5014 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5013)
                                var t5015 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5012, t5014)
                                var t5016 bool = t5015 == 45
                                jp4994 = t5016
                            }
                        } else {
                            jp4994 = false
                        }
                        if jp4994 {
                            var t4995 *ref_int_x = value__227.index
                            var t4996 *ref_int_x = value__227.index
                            var t4997 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4996)
                            var t4998 int = t4997 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4995, t4998)
                        } else {}
                        var t4989 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4990 bool = !t4989
                        if t4990 {
                            var t4991 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4992 Result__string__string = Result__string__string_Err{
                                _0: t4991,
                            }
                            return t4992
                        } else {
                            var t4976 string = value__227.input
                            var t4977 *ref_int_x = value__227.index
                            var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                            var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                            var t4980 Result__string__string = Result__string__string_Ok{
                                _0: t4979,
                            }
                            return t4980
                        }
                    } else {
                        var t4976 string = value__227.input
                        var t4977 *ref_int_x = value__227.index
                        var t4978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4977)
                        var t4979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4976, start__228, t4978)
                        var t4980 Result__string__string = Result__string__string_Ok{
                            _0: t4979,
                        }
                        return t4980
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5123 *ref_int_x = value__230.index
    var t5124 int
    var inline10209 int = ref_get__Ref_3int(t5123)
    t5124 = inline10209
    var t5125 int
    var inline10207 int = _goml_runtime_core_string_len(expected__231)
    t5125 = inline10207
    var t5126 int = t5124 + t5125
    var t5127 string = value__230.input
    var t5128 int
    var inline10205 int = _goml_runtime_core_string_len(t5127)
    t5128 = inline10205
    var t5129 bool = t5126 <= t5128
    var jp5114 bool
    if t5129 {
        var t5130 string = value__230.input
        var t5131 *ref_int_x = value__230.index
        var t5132 int
        var inline10189 int = ref_get__Ref_3int(t5131)
        t5132 = inline10189
        var t5133 *ref_int_x = value__230.index
        var t5134 int
        var inline10187 int = ref_get__Ref_3int(t5133)
        t5134 = inline10187
        var t5135 int
        var inline10185 int = _goml_runtime_core_string_len(expected__231)
        t5135 = inline10185
        var t5136 int = t5134 + t5135
        var t5137 string
        var inline10183 string = string_byte_slice(t5130, t5132, t5136)
        t5137 = inline10183
        var t5138 bool = t5137 == expected__231
        jp5114 = t5138
    } else {
        jp5114 = false
    }
    if jp5114 {
        var t5115 *ref_int_x = value__230.index
        var t5116 *ref_int_x = value__230.index
        var t5117 int
        var inline10195 int = ref_get__Ref_3int(t5116)
        t5117 = inline10195
        var t5118 int
        var inline10193 int = _goml_runtime_core_string_len(expected__231)
        t5118 = inline10193
        var t5119 int = t5117 + t5118
        ref_set__Ref_3int(t5115, t5119)
        var t5120 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t5120
    } else {
        var t5121 string
        var inline10197 string = "invalid literal"
        var inline10198 string = "" + inline10197
        var inline10199 string = inline10198 + " at byte "
        var inline10200 *ref_int_x = value__230.index
        var inline10201 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10200)
        var inline10202 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10201)
        var inline10203 string = inline10199 + inline10202
        t5121 = inline10203
        var t5122 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5121,
        }
        return t5122
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5141 *ref_int_x = value__233.index
    var t5142 *ref_int_x = value__233.index
    var t5143 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5142)
    var t5144 int = t5143 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5141, t5144)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8833 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t5199 *ref_int_x = value__233.index
    var t5200 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5199)
    var t5201 string = value__233.input
    var t5202 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5201)
    var t5203 bool = t5200 < t5202
    var jp5192 bool
    if t5203 {
        var t5204 string = value__233.input
        var t5205 *ref_int_x = value__233.index
        var t5206 int
        var inline10213 int = ref_get__Ref_3int(t5205)
        t5206 = inline10213
        var t5207 uint8
        var inline10211 uint8 = _goml_runtime_core_string_byte_get(t5204, t5206)
        t5207 = inline10211
        var t5208 bool = t5207 == 93
        jp5192 = t5208
    } else {
        jp5192 = false
    }
    if jp5192 {
        var t5193 *ref_int_x = value__233.index
        var t5194 *ref_int_x = value__233.index
        var t5195 int
        var inline10217 int = ref_get__Ref_3int(t5194)
        t5195 = inline10217
        var t5196 int = t5195 + 1
        ref_set__Ref_3int(t5193, t5196)
        var t5197 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8833,
        }
        var t5198 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5197,
        }
        return t5198
    } else {
        Loop_loop5149:
        for {
            var t5150 *ref_int_x = value__233.index
            var t5151 int
            var inline10259 int = ref_get__Ref_3int(t5150)
            t5151 = inline10259
            var t5152 string = value__233.input
            var t5153 int
            var inline10257 int = _goml_runtime_core_string_len(t5152)
            t5153 = inline10257
            var t5154 bool = t5151 < t5153
            if t5154 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5156 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp5156 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(vec_literal__8833, jp5156)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5158 *ref_int_x = value__233.index
                    var t5159 int
                    var inline10253 int = ref_get__Ref_3int(t5158)
                    t5159 = inline10253
                    var t5160 string = value__233.input
                    var t5161 int
                    var inline10251 int = _goml_runtime_core_string_len(t5160)
                    t5161 = inline10251
                    var t5162 bool = t5159 >= t5161
                    if t5162 {
                        var t5163 string
                        var inline10219 string = "unterminated array"
                        var inline10220 string = "" + inline10219
                        var inline10221 string = inline10220 + " at byte "
                        var inline10222 *ref_int_x = value__233.index
                        var inline10223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10222)
                        var inline10224 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10223)
                        var inline10225 string = inline10221 + inline10224
                        t5163 = inline10225
                        var t5164 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5163,
                        }
                        return t5164
                    } else {
                        var t5166 string = value__233.input
                        var t5167 *ref_int_x = value__233.index
                        var t5168 int
                        var inline10249 int = ref_get__Ref_3int(t5167)
                        t5168 = inline10249
                        var t5169 uint8
                        var inline10247 uint8 = _goml_runtime_core_string_byte_get(t5166, t5168)
                        t5169 = inline10247
                        var t5170 bool = t5169 == 93
                        if t5170 {
                            var t5171 *ref_int_x = value__233.index
                            var t5172 *ref_int_x = value__233.index
                            var t5173 int
                            var inline10229 int = ref_get__Ref_3int(t5172)
                            t5173 = inline10229
                            var t5174 int = t5173 + 1
                            ref_set__Ref_3int(t5171, t5174)
                            var t5175 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8833,
                            }
                            var t5176 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t5175,
                            }
                            return t5176
                        } else {
                            var t5178 string = value__233.input
                            var t5179 *ref_int_x = value__233.index
                            var t5180 int
                            var inline10245 int = ref_get__Ref_3int(t5179)
                            t5180 = inline10245
                            var t5181 uint8
                            var inline10243 uint8 = _goml_runtime_core_string_byte_get(t5178, t5180)
                            t5181 = inline10243
                            var t5182 bool = t5181 == 44
                            if t5182 {
                                var t5183 *ref_int_x = value__233.index
                                var t5184 *ref_int_x = value__233.index
                                var t5185 int
                                var inline10233 int = ref_get__Ref_3int(t5184)
                                t5185 = inline10233
                                var t5186 int = t5185 + 1
                                ref_set__Ref_3int(t5183, t5186)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5188 string
                                var inline10235 string = "expected array separator"
                                var inline10236 string = "" + inline10235
                                var inline10237 string = inline10236 + " at byte "
                                var inline10238 *ref_int_x = value__233.index
                                var inline10239 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10238)
                                var inline10240 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10239)
                                var inline10241 string = inline10237 + inline10240
                                t5188 = inline10241
                                var t5189 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5188,
                                }
                                return t5189
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t5190 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t5190
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5149
            }
        }
        var t5147 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5148 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5147,
        }
        return t5148
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5211 *ref_int_x = value__236.index
    var t5212 *ref_int_x = value__236.index
    var t5213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5212)
    var t5214 int = t5213 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5211, t5214)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10035 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5293 *ref_int_x = value__236.index
    var t5294 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5293)
    var t5295 string = value__236.input
    var t5296 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5295)
    var t5297 bool = t5294 < t5296
    var jp5286 bool
    if t5297 {
        var t5298 string = value__236.input
        var t5299 *ref_int_x = value__236.index
        var t5300 int
        var inline10263 int = ref_get__Ref_3int(t5299)
        t5300 = inline10263
        var t5301 uint8
        var inline10261 uint8 = _goml_runtime_core_string_byte_get(t5298, t5300)
        t5301 = inline10261
        var t5302 bool = t5301 == 125
        jp5286 = t5302
    } else {
        jp5286 = false
    }
    if jp5286 {
        var t5287 *ref_int_x = value__236.index
        var t5288 *ref_int_x = value__236.index
        var t5289 int
        var inline10267 int = ref_get__Ref_3int(t5288)
        t5289 = inline10267
        var t5290 int = t5289 + 1
        ref_set__Ref_3int(t5287, t5290)
        var t5291 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10035,
        }
        var t5292 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5291,
        }
        return t5292
    } else {
        Loop_loop5219:
        for {
            var t5220 *ref_int_x = value__236.index
            var t5221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5220)
            var t5222 string = value__236.input
            var t5223 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5222)
            var t5224 bool = t5221 < t5223
            if t5224 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5226 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5226 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5274 *ref_int_x = value__236.index
                    var t5275 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5274)
                    var t5276 string = value__236.input
                    var t5277 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5276)
                    var t5278 bool = t5275 >= t5277
                    var jp5266 bool
                    if t5278 {
                        jp5266 = true
                    } else {
                        var t5279 string = value__236.input
                        var t5280 *ref_int_x = value__236.index
                        var t5281 int
                        var inline10271 int = ref_get__Ref_3int(t5280)
                        t5281 = inline10271
                        var t5282 uint8
                        var inline10269 uint8 = _goml_runtime_core_string_byte_get(t5279, t5281)
                        t5282 = inline10269
                        var t5283 bool = t5282 != 58
                        jp5266 = t5283
                    }
                    if jp5266 {
                        var t5267 string
                        var inline10273 string = "expected object colon"
                        var inline10274 string = "" + inline10273
                        var inline10275 string = inline10274 + " at byte "
                        var inline10276 *ref_int_x = value__236.index
                        var inline10277 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10276)
                        var inline10278 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10277)
                        var inline10279 string = inline10275 + inline10278
                        t5267 = inline10279
                        var t5268 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5267,
                        }
                        return t5268
                    } else {
                        var t5269 *ref_int_x = value__236.index
                        var t5270 *ref_int_x = value__236.index
                        var t5271 int
                        var inline10283 int = ref_get__Ref_3int(t5270)
                        t5271 = inline10283
                        var t5272 int = t5271 + 1
                        ref_set__Ref_3int(t5269, t5272)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5229 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5229 = x816
                            var t5230 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5226,
                                _1: jp5229,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10035, t5230)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5232 *ref_int_x = value__236.index
                            var t5233 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5232)
                            var t5234 string = value__236.input
                            var t5235 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5234)
                            var t5236 bool = t5233 >= t5235
                            if t5236 {
                                var t5237 string
                                var inline10285 string = "unterminated object"
                                var inline10286 string = "" + inline10285
                                var inline10287 string = inline10286 + " at byte "
                                var inline10288 *ref_int_x = value__236.index
                                var inline10289 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10288)
                                var inline10290 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10289)
                                var inline10291 string = inline10287 + inline10290
                                t5237 = inline10291
                                var t5238 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5237,
                                }
                                return t5238
                            } else {
                                var t5240 string = value__236.input
                                var t5241 *ref_int_x = value__236.index
                                var t5242 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5241)
                                var t5243 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5240, t5242)
                                var t5244 bool = t5243 == 125
                                if t5244 {
                                    var t5245 *ref_int_x = value__236.index
                                    var t5246 *ref_int_x = value__236.index
                                    var t5247 int
                                    var inline10295 int = ref_get__Ref_3int(t5246)
                                    t5247 = inline10295
                                    var t5248 int = t5247 + 1
                                    ref_set__Ref_3int(t5245, t5248)
                                    var t5249 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10035,
                                    }
                                    var t5250 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5249,
                                    }
                                    return t5250
                                } else {
                                    var t5252 string = value__236.input
                                    var t5253 *ref_int_x = value__236.index
                                    var t5254 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5253)
                                    var t5255 uint8
                                    var inline10309 uint8 = _goml_runtime_core_string_byte_get(t5252, t5254)
                                    t5255 = inline10309
                                    var t5256 bool = t5255 == 44
                                    if t5256 {
                                        var t5257 *ref_int_x = value__236.index
                                        var t5258 *ref_int_x = value__236.index
                                        var t5259 int
                                        var inline10299 int = ref_get__Ref_3int(t5258)
                                        t5259 = inline10299
                                        var t5260 int = t5259 + 1
                                        ref_set__Ref_3int(t5257, t5260)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5262 string
                                        var inline10301 string = "expected object separator"
                                        var inline10302 string = "" + inline10301
                                        var inline10303 string = inline10302 + " at byte "
                                        var inline10304 *ref_int_x = value__236.index
                                        var inline10305 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10304)
                                        var inline10306 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10305)
                                        var inline10307 string = inline10303 + inline10306
                                        t5262 = inline10307
                                        var t5263 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5262,
                                        }
                                        return t5263
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5264 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5264
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5284 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5284
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5219
            }
        }
        var t5217 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5218 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5217,
        }
        return t5218
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5307 *ref_int_x = value__240.index
    var t5308 int
    var inline10347 int = ref_get__Ref_3int(t5307)
    t5308 = inline10347
    var t5309 string = value__240.input
    var t5310 int
    var inline10345 int = _goml_runtime_core_string_len(t5309)
    t5310 = inline10345
    var t5311 bool = t5308 >= t5310
    if t5311 {
        var t5312 string
        var inline10311 string = "expected JSON value"
        var inline10312 string = "" + inline10311
        var inline10313 string = inline10312 + " at byte "
        var inline10314 *ref_int_x = value__240.index
        var inline10315 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10314)
        var inline10316 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10315)
        var inline10317 string = inline10313 + inline10316
        t5312 = inline10317
        var t5313 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5312,
        }
        return t5313
    } else {
        var t5314 string = value__240.input
        var t5315 *ref_int_x = value__240.index
        var t5316 int
        var inline10343 int = ref_get__Ref_3int(t5315)
        t5316 = inline10343
        var mtmp824 uint8
        var inline10341 uint8 = _goml_runtime_core_string_byte_get(t5314, t5316)
        mtmp824 = inline10341
        switch mtmp824 {
        case 123:
            var t5319 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5319
        case 91:
            var t5320 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5320
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5323 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5324 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5323,
                }
                return t5324
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5325 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5325
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5326 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5327 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5326)
            return t5327
        case 102:
            var t5328 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5329 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5328)
            return t5329
        case 110:
            var t5330 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5330
        default:
            var t5338 bool = mtmp824 == 45
            var jp5334 bool
            if t5338 {
                jp5334 = true
            } else {
                var inline10319 bool = mtmp824 >= 48
                if inline10319 {
                    var inline10320 bool = mtmp824 <= 57
                    jp5334 = inline10320
                } else {
                    jp5334 = false
                }
            }
            if jp5334 {
                var inline10322 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10324 string
                switch inline10322.(type) {
                case Result__string__string_Ok:
                    var inline10327 string = inline10322.(Result__string__string_Ok)._0
                    inline10324 = inline10327
                    var inline10325 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10324,
                    }
                    var inline10326 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10325,
                    }
                    return inline10326
                case Result__string__string_Err:
                    var inline10329 string = inline10322.(Result__string__string_Err)._0
                    var inline10331 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10329,
                    }
                    return inline10331
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5336 string
                var inline10333 string = "unexpected JSON token"
                var inline10334 string = "" + inline10333
                var inline10335 string = inline10334 + " at byte "
                var inline10336 *ref_int_x = value__240.index
                var inline10337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10336)
                var inline10338 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10337)
                var inline10339 string = inline10335 + inline10338
                t5336 = inline10339
                var t5337 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5336,
                }
                return t5337
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10361 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10362 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10361,
    }
    parser__245 = inline10362
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5343 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5343 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5346 *ref_int_x = parser__245.index
        var t5347 int
        var inline10359 int = ref_get__Ref_3int(t5346)
        t5347 = inline10359
        var t5348 int
        var inline10357 int = _goml_runtime_core_string_len(input__244)
        t5348 = inline10357
        var t5349 bool = t5347 == t5348
        if t5349 {
            var t5350 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5343,
            }
            return t5350
        } else {
            var t5351 string
            var inline10349 string = "trailing JSON data"
            var inline10350 string = "" + inline10349
            var inline10351 string = inline10350 + " at byte "
            var inline10352 *ref_int_x = parser__245.index
            var inline10353 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10352)
            var inline10354 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10353)
            var inline10355 string = inline10351 + inline10354
            t5351 = inline10355
            var t5352 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5351,
            }
            return t5352
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5353 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5353
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10395 rune = 34
    var inline10396 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10395)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10396)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10393 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10393
    Loop_loop5367:
    for {
        var t5368 bool = for_index833 < for_limit834
        if t5368 {
            var for_item835 int = for_index833
            var t5369 int = for_index833 + 1
            for_index833 = t5369
            var byte__252 uint8
            var inline10381 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10381
            var t5422 bool = byte__252 == 34
            var jp5420 bool
            if t5422 {
                jp5420 = true
            } else {
                var t5423 bool = byte__252 == 92
                jp5420 = t5423
            }
            var jp5417 bool
            if jp5420 {
                jp5417 = true
            } else {
                var t5421 bool = byte__252 == 8
                jp5417 = t5421
            }
            var jp5414 bool
            if jp5417 {
                jp5414 = true
            } else {
                var t5418 bool = byte__252 == 9
                jp5414 = t5418
            }
            var jp5411 bool
            if jp5414 {
                jp5411 = true
            } else {
                var t5415 bool = byte__252 == 10
                jp5411 = t5415
            }
            var jp5408 bool
            if jp5411 {
                jp5408 = true
            } else {
                var t5412 bool = byte__252 == 12
                jp5408 = t5412
            }
            var jp5405 bool
            if jp5408 {
                jp5405 = true
            } else {
                var t5409 bool = byte__252 == 13
                jp5405 = t5409
            }
            var jp5372 bool
            if jp5405 {
                jp5372 = true
            } else {
                var t5406 bool = byte__252 < 32
                jp5372 = t5406
            }
            if jp5372 {
                var t5401 bool = start__250 < for_item835
                if t5401 {
                    var t5402 string
                    var inline10367 string = string_byte_slice(value__249, start__250, for_item835)
                    t5402 = inline10367
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5402)
                } else {}
                var t5376 bool = byte__252 == 34
                if t5376 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5379 bool = byte__252 == 92
                    if t5379 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5382 bool = byte__252 == 8
                        if t5382 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5385 bool = byte__252 == 9
                            if t5385 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5388 bool = byte__252 == 10
                                if t5388 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5391 bool = byte__252 == 12
                                    if t5391 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5394 bool = byte__252 == 13
                                        if t5394 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5396 uint8 = byte__252 / 16
                                            var t5397 rune
                                            var inline10378 int = int(uint8(t5396))
                                            var inline10379 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10378)
                                            t5397 = inline10379
                                            var inline10375 string = _goml_m_inherent_i_char_i_char_i_to__string(t5397)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10375)
                                            var t5398_rhs uint8 = 16
                                            var t5398 uint8 = byte__252 % t5398_rhs
                                            var t5399 rune
                                            var inline10372 int = int(uint8(t5398))
                                            var inline10373 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10372)
                                            t5399 = inline10373
                                            var inline10369 string = _goml_m_inherent_i_char_i_char_i_to__string(t5399)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10369)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5375 int = for_item835 + 1
                start__250 = t5375
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5367
        }
    }
    var t5362 int
    var inline10391 int = _goml_runtime_core_string_len(value__249)
    t5362 = inline10391
    var t5363 bool = start__250 < t5362
    if t5363 {
        var t5364 int
        var inline10385 int = _goml_runtime_core_string_len(value__249)
        t5364 = inline10385
        var t5365 string
        var inline10383 string = string_byte_slice(value__249, start__250, t5364)
        t5365 = inline10383
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5365)
    } else {}
    var inline10387 rune = 34
    var inline10388 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10387)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10388)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10411 rune = 123
        var inline10412 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10411)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10412)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5428:
        for {
            var t5429 bool = for_index852 < for_limit851
            if t5429 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5430 int = for_index852 + 1
                for_index852 = t5430
                var t5436 bool = index__256 > 0
                if t5436 {
                    var inline10399 rune = 44
                    var inline10400 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10399)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10400)
                } else {}
                var t5432 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5432)
                var inline10403 rune = 58
                var inline10404 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10403)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10404)
                var t5433 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5433)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5434 int = compound_old859 + compound_value860
                index__256 = t5434
                continue
            } else {
                break Loop_loop5428
            }
        }
        var inline10407 rune = 125
        var inline10408 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10407)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10408)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10423 rune = 91
        var inline10424 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10423)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10424)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5440:
        for {
            var t5441 bool = for_index866 < for_limit865
            if t5441 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5442 int = for_index866 + 1
                for_index866 = t5442
                var t5446 bool = index__259 > 0
                if t5446 {
                    var inline10415 rune = 44
                    var inline10416 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10415)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10416)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5444 int = compound_old871 + compound_value872
                index__259 = t5444
                continue
            } else {
                break Loop_loop5440
            }
        }
        var inline10419 rune = 93
        var inline10420 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10419)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10420)
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
        var jp5451 string
        if x848 {
            jp5451 = "true"
        } else {
            jp5451 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5451)
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
    var inline10432 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10433 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10432,
    }
    builder__265 = inline10433
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10427 *_goml_vec_uint8 = builder__265.values
    var inline10428 Tuple2_4bool_6string = string_from_utf8(inline10427)
    var inline10429 string = inline10428._1
    return inline10429
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5462:
        for {
            var t5463 bool = for_index883 < for_limit882
            if t5463 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5464 int = for_index883 + 1
                for_index883 = t5464
                var t5466 string = for_item884._0
                var t5467 bool = t5466 == name__267
                if t5467 {
                    var t5468 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5469 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5468,
                    }
                    return t5469
                } else {
                    continue
                }
            } else {
                break Loop_loop5462
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5479 int
    var inline10444 int = _goml_runtime_core_string_len(value__272)
    t5479 = inline10444
    var t5480 bool = t5479 == 0
    if t5480 {
        return Option__int_None{}
    } else {
        var t5481 uint8
        var inline10441 int = 0
        var inline10442 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10441)
        t5481 = inline10442
        var negative__273 bool = t5481 == 45
        var jp5483 int
        if negative__273 {
            jp5483 = 1
        } else {
            jp5483 = 0
        }
        var index__274 int = jp5483
        var result__275 int = 0
        var t5504 int
        var inline10439 int = _goml_runtime_core_string_len(value__272)
        t5504 = inline10439
        var t5505 bool = index__274 == t5504
        if t5505 {
            return Option__int_None{}
        } else {
            Loop_loop5490:
            for {
                var t5491 int
                var inline10437 int = _goml_runtime_core_string_len(value__272)
                t5491 = inline10437
                var t5492 bool = index__274 < t5491
                if t5492 {
                    var byte__276 uint8
                    var inline10435 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10435
                    var t5502 bool = byte__276 < 48
                    var jp5497 bool
                    if t5502 {
                        jp5497 = true
                    } else {
                        var t5503 bool = byte__276 > 57
                        jp5497 = t5503
                    }
                    if jp5497 {
                        return Option__int_None{}
                    } else {
                        var t5498 int = result__275 * 10
                        var t5499 uint8 = byte__276 - 48
                        var t5500 int = int(uint8(t5499))
                        var t5501 int = t5498 + t5500
                        result__275 = t5501
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5494 int = compound_old895 + compound_value896
                        index__274 = t5494
                        continue
                    }
                } else {
                    break Loop_loop5490
                }
            }
            var jp5487 int
            if negative__273 {
                var t5489 int = 0 - result__275
                jp5487 = t5489
            } else {
                jp5487 = result__275
            }
            var t5488 Option__int = Option__int_Some{
                _0: jp5487,
            }
            return t5488
        }
    }
}

func main0() struct{} {
    var mtmp187 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6277 _goml_m_std_p_json_p_Value
    switch mtmp187.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x188 _goml_m_std_p_json_p_Value = mtmp187.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6277 = x188
        var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "name")
        switch mtmp191.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline10891 string = "missing name"
            var inline10892 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10891)
            _goml_runtime_core_string_println(inline10892)
            var mtmp196 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "version")
            switch mtmp196.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10906 string = "missing version"
                var inline10907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10906)
                _goml_runtime_core_string_println(inline10907)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x197 _goml_m_std_p_json_p_Value = mtmp196.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp198 Option__int
                switch x197.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline10917 string = x197.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline10919 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10917)
                    mtmp198 = inline10919
                default:
                    mtmp198 = Option__int_None{}
                }
                switch mtmp198.(type) {
                case Option__int_None:
                    var inline10910 string = "invalid version"
                    var inline10911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10910)
                    _goml_runtime_core_string_println(inline10911)
                case Option__int_Some:
                    var x199 int = mtmp198.(Option__int_Some)._0
                    var inline10914 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x199)
                    _goml_runtime_core_string_println(inline10914)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp201 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "stable")
            switch mtmp201.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10921 string = "missing stable"
                var inline10922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10921)
                _goml_runtime_core_string_println(inline10922)
                var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                println__T_string(t6281)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x202 _goml_m_std_p_json_p_Value = mtmp201.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11193 bool
                switch x202.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline10932 bool = x202.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11193 = inline10932
                    var inline10929 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11193)
                    _goml_runtime_core_string_println(inline10929)
                    var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                    println__T_string(t6281)
                    return struct{}{}
                default:
                    var inline10925 string = "invalid stable"
                    var inline10926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10925)
                    _goml_runtime_core_string_println(inline10926)
                    var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                    println__T_string(t6281)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11199 string
            switch x192.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline10902 string = x192.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11199 = inline10902
                var inline10899 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11199)
                _goml_runtime_core_string_println(inline10899)
                var mtmp196 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "version")
                switch mtmp196.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10906 string = "missing version"
                    var inline10907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10906)
                    _goml_runtime_core_string_println(inline10907)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x197 _goml_m_std_p_json_p_Value = mtmp196.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp198 Option__int
                    switch x197.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10917 string = x197.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10919 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10917)
                        mtmp198 = inline10919
                    default:
                        mtmp198 = Option__int_None{}
                    }
                    switch mtmp198.(type) {
                    case Option__int_None:
                        var inline10910 string = "invalid version"
                        var inline10911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10910)
                        _goml_runtime_core_string_println(inline10911)
                    case Option__int_Some:
                        var x199 int = mtmp198.(Option__int_Some)._0
                        var inline10914 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x199)
                        _goml_runtime_core_string_println(inline10914)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp201 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "stable")
                switch mtmp201.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10921 string = "missing stable"
                    var inline10922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10921)
                    _goml_runtime_core_string_println(inline10922)
                    var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                    println__T_string(t6281)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x202 _goml_m_std_p_json_p_Value = mtmp201.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11193 bool
                    switch x202.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10932 bool = x202.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11193 = inline10932
                        var inline10929 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11193)
                        _goml_runtime_core_string_println(inline10929)
                        var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                        println__T_string(t6281)
                        return struct{}{}
                    default:
                        var inline10925 string = "invalid stable"
                        var inline10926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10925)
                        _goml_runtime_core_string_println(inline10926)
                        var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                        println__T_string(t6281)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline10895 string = "invalid name"
                var inline10896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10895)
                _goml_runtime_core_string_println(inline10896)
                var mtmp196 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "version")
                switch mtmp196.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10906 string = "missing version"
                    var inline10907 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10906)
                    _goml_runtime_core_string_println(inline10907)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x197 _goml_m_std_p_json_p_Value = mtmp196.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp198 Option__int
                    switch x197.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10917 string = x197.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10919 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10917)
                        mtmp198 = inline10919
                    default:
                        mtmp198 = Option__int_None{}
                    }
                    switch mtmp198.(type) {
                    case Option__int_None:
                        var inline10910 string = "invalid version"
                        var inline10911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10910)
                        _goml_runtime_core_string_println(inline10911)
                    case Option__int_Some:
                        var x199 int = mtmp198.(Option__int_Some)._0
                        var inline10914 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x199)
                        _goml_runtime_core_string_println(inline10914)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp201 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6277, "stable")
                switch mtmp201.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10921 string = "missing stable"
                    var inline10922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10921)
                    _goml_runtime_core_string_println(inline10922)
                    var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                    println__T_string(t6281)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x202 _goml_m_std_p_json_p_Value = mtmp201.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11193 bool
                    switch x202.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10932 bool = x202.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11193 = inline10932
                        var inline10929 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11193)
                        _goml_runtime_core_string_println(inline10929)
                        var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                        println__T_string(t6281)
                        return struct{}{}
                    default:
                        var inline10925 string = "invalid stable"
                        var inline10926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10925)
                        _goml_runtime_core_string_println(inline10926)
                        var t6281 string = _goml_m_std_p_json_p_encode(jp6277)
                        println__T_string(t6281)
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
        var x189 string = mtmp187.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline10888 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x189)
        _goml_runtime_core_string_println(inline10888)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6297 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6297
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6337:
    for {
        var t6338 int
        var inline10943 int = _goml_runtime_core_string_len(x12)
        t6338 = inline10943
        var t6339 bool = index__26 < t6338
        if t6339 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6341 int = compound_old17 + x16
                index__26 = t6341
                continue
            } else {
                var t6343 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6343
            }
        } else {
            break Loop_loop6337
        }
    }
    var t6336 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6336
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t6373 string = _goml_runtime_core_int_to_string(self__32)
    return t6373
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline10953 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline10954 bool = inline10953._0
    var inline10955 rune = inline10953._1
    if inline10954 {
        return inline10955
    } else {
        var inline10958 rune = _goml_runtime_core_string_get("", -1)
        return inline10958
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t6458 *ref_int_x = ref__Ref_3int(value__273)
    return t6458
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__274 *ref_int_x) int {
    var t6461 int = ref_get__Ref_3int(self__274)
    return t6461
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__275 *ref_int_x, value__276 int) struct{} {
    ref_set__Ref_3int(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__34 rune) string {
    var inline10965 uint32 = uint32(rune(self__34))
    var inline10966 bool = utf8_valid_scalar(inline10965)
    if inline10966 {
        var inline10967 string = _goml_runtime_core_char_to_string(self__34)
        return inline10967
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t6526 int = _goml_runtime_core_string_len(self__36)
    return t6526
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t6529 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t6529
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline10970 bool = string_is_char_boundary(self__41, start__42)
    var inline10972 bool
    if inline10970 {
        var inline10975 bool = string_is_char_boundary(self__41, end__43)
        inline10972 = inline10975
    } else {
        inline10972 = false
    }
    if inline10972 {
        var inline10973 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline10973
    } else {
        var inline10974 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline10974
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline11017 bool = utf8_valid_scalar(value__2)
    if inline11017 {
        var inline11018 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11019 rune = inline11018._1
        var inline11021 Option__char = Option__char_Some{
            _0: inline11019,
        }
        return inline11021
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6638 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6638
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6643 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6643
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__174 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__175 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__174, elem__175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6679 string
    t6679 = value__1
    _goml_runtime_core_string_println(t6679)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6805 bool = index__6 < 0
    var jp6803 bool
    if t6805 {
        jp6803 = true
    } else {
        var t6806 bool = index__6 >= length__7
        jp6803 = t6806
    }
    if jp6803 {
        var inline11032 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11032
    } else {
        var t6690 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6690))
        var t6693 bool = first__8 < 128
        if t6693 {
            var inline11034 int = 1
            var inline11035 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11035.(type) {
            case Option__char_None:
                var inline11036 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11036
            case Option__char_Some:
                var inline11037 rune = inline11035.(Option__char_Some)._0
                var inline11039 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11037,
                    _2: inline11034,
                }
                return inline11039
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6697 bool = first__8 < 194
            if t6697 {
                var inline11041 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11041
            } else {
                var t6701 bool = first__8 < 224
                if t6701 {
                    var t6714 int = length__7 - index__6
                    var t6715 bool = t6714 < 2
                    if t6715 {
                        var inline11043 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11043
                    } else {
                        var t6703 int = index__6 + 1
                        var t6704 uint8
                        var inline11057 uint8 = _goml_runtime_core_string_byte_get(value__5, t6703)
                        t6704 = inline11057
                        var second__9 uint32 = uint32(uint8(t6704))
                        var t6707 bool
                        var inline11054 bool = second__9 < 128
                        if inline11054 {
                            t6707 = true
                        } else {
                            var inline11055 bool = second__9 > 191
                            t6707 = inline11055
                        }
                        if t6707 {
                            var inline11045 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11045
                        } else {
                            var t6709_rhs uint32 = 31
                            var t6709 uint32 = first__8 & t6709_rhs
                            var t6710_rhs int = 6
                            var t6710 uint32 = t6709 << t6710_rhs
                            var t6711_rhs uint32 = 63
                            var t6711 uint32 = second__9 & t6711_rhs
                            var t6712 uint32 = t6710 | t6711
                            var inline11047 int = 2
                            var inline11048 Option__char = __goml_builtin_char_from_uint32(t6712)
                            switch inline11048.(type) {
                            case Option__char_None:
                                var inline11049 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11049
                            case Option__char_Some:
                                var inline11050 rune = inline11048.(Option__char_Some)._0
                                var inline11052 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11050,
                                    _2: inline11047,
                                }
                                return inline11052
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6719 bool = first__8 < 240
                    if t6719 {
                        var t6752 int = length__7 - index__6
                        var t6753 bool = t6752 < 3
                        if t6753 {
                            var inline11059 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11059
                        } else {
                            var t6721 int = index__6 + 1
                            var t6722 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6721)
                            var second__10 uint32 = uint32(uint8(t6722))
                            var t6723 int = index__6 + 2
                            var t6724 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6723)
                            var third__11 uint32 = uint32(uint8(t6724))
                            var t6750 bool = utf8_invalid_continuation(second__10)
                            var jp6745 bool
                            if t6750 {
                                jp6745 = true
                            } else {
                                var inline11061 bool = third__11 < 128
                                if inline11061 {
                                    jp6745 = true
                                } else {
                                    var inline11062 bool = third__11 > 191
                                    jp6745 = inline11062
                                }
                            }
                            var jp6739 bool
                            if jp6745 {
                                jp6739 = true
                            } else {
                                var t6748 bool = first__8 == 224
                                if t6748 {
                                    var t6749 bool = second__10 < 160
                                    jp6739 = t6749
                                } else {
                                    jp6739 = false
                                }
                            }
                            var jp6728 bool
                            if jp6739 {
                                jp6728 = true
                            } else {
                                var t6742 bool = first__8 == 237
                                if t6742 {
                                    var t6743 bool = second__10 >= 160
                                    jp6728 = t6743
                                } else {
                                    jp6728 = false
                                }
                            }
                            if jp6728 {
                                var inline11064 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11064
                            } else {
                                var t6730_rhs uint32 = 15
                                var t6730 uint32 = first__8 & t6730_rhs
                                var t6731_rhs int = 12
                                var t6731 uint32 = t6730 << t6731_rhs
                                var t6732_rhs uint32 = 63
                                var t6732 uint32 = second__10 & t6732_rhs
                                var t6733_rhs int = 6
                                var t6733 uint32 = t6732 << t6733_rhs
                                var t6734 uint32 = t6731 | t6733
                                var t6735_rhs uint32 = 63
                                var t6735 uint32 = third__11 & t6735_rhs
                                var t6736 uint32 = t6734 | t6735
                                var inline11066 int = 3
                                var inline11067 Option__char = __goml_builtin_char_from_uint32(t6736)
                                switch inline11067.(type) {
                                case Option__char_None:
                                    var inline11068 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11068
                                case Option__char_Some:
                                    var inline11069 rune = inline11067.(Option__char_Some)._0
                                    var inline11071 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11069,
                                        _2: inline11066,
                                    }
                                    return inline11071
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6757 bool = first__8 < 245
                        if t6757 {
                            var t6798 int = length__7 - index__6
                            var t6799 bool = t6798 < 4
                            if t6799 {
                                var t6800 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6800
                            } else {
                                var t6759 int = index__6 + 1
                                var t6760 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6759)
                                var second__12 uint32 = uint32(uint8(t6760))
                                var t6761 int = index__6 + 2
                                var t6762 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6761)
                                var third__13 uint32 = uint32(uint8(t6762))
                                var t6763 int = index__6 + 3
                                var t6764 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6763)
                                var fourth__14 uint32 = uint32(uint8(t6764))
                                var t6796 bool = utf8_invalid_continuation(second__12)
                                var jp6794 bool
                                if t6796 {
                                    jp6794 = true
                                } else {
                                    var t6797 bool = utf8_invalid_continuation(third__13)
                                    jp6794 = t6797
                                }
                                var jp6788 bool
                                if jp6794 {
                                    jp6788 = true
                                } else {
                                    var t6795 bool = utf8_invalid_continuation(fourth__14)
                                    jp6788 = t6795
                                }
                                var jp6782 bool
                                if jp6788 {
                                    jp6782 = true
                                } else {
                                    var t6791 bool = first__8 == 240
                                    if t6791 {
                                        var t6792 bool = second__12 < 144
                                        jp6782 = t6792
                                    } else {
                                        jp6782 = false
                                    }
                                }
                                var jp6768 bool
                                if jp6782 {
                                    jp6768 = true
                                } else {
                                    var t6785 bool = first__8 == 244
                                    if t6785 {
                                        var t6786 bool = second__12 > 143
                                        jp6768 = t6786
                                    } else {
                                        jp6768 = false
                                    }
                                }
                                if jp6768 {
                                    var t6769 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6769
                                } else {
                                    var t6770_rhs uint32 = 7
                                    var t6770 uint32 = first__8 & t6770_rhs
                                    var t6771_rhs int = 18
                                    var t6771 uint32 = t6770 << t6771_rhs
                                    var t6772_rhs uint32 = 63
                                    var t6772 uint32 = second__12 & t6772_rhs
                                    var t6773_rhs int = 12
                                    var t6773 uint32 = t6772 << t6773_rhs
                                    var t6774 uint32 = t6771 | t6773
                                    var t6775_rhs uint32 = 63
                                    var t6775 uint32 = third__13 & t6775_rhs
                                    var t6776_rhs int = 6
                                    var t6776 uint32 = t6775 << t6776_rhs
                                    var t6777 uint32 = t6774 | t6776
                                    var t6778_rhs uint32 = 63
                                    var t6778 uint32 = fourth__14 & t6778_rhs
                                    var t6779 uint32 = t6777 | t6778
                                    var t6780 Tuple3_4bool_4char_3int = utf8_valid_decode(t6779, 4)
                                    return t6780
                                }
                            }
                        } else {
                            var t6801 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6801
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6826 uint32 = uint32(rune(value__29))
    var t6827 bool
    var inline11073 bool = t6826 <= 1114111
    if inline11073 {
        var inline11074 bool = t6826 >= 55296
        var inline11076 bool
        if inline11074 {
            var inline11078 bool = t6826 <= 57343
            inline11076 = inline11078
        } else {
            inline11076 = false
        }
        var inline11077 bool = !inline11076
        t6827 = inline11077
    } else {
        t6827 = false
    }
    if t6827 {
        var t6828 string = _goml_runtime_core_char_to_string(value__29)
        return t6828
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6842 bool = index__16 < 0
    var jp6834 bool
    if t6842 {
        jp6834 = true
    } else {
        var t6843 int
        var inline11080 int = _goml_runtime_core_string_len(value__15)
        t6843 = inline11080
        var t6844 bool = index__16 > t6843
        jp6834 = t6844
    }
    if jp6834 {
        return false
    } else {
        var t6837 int
        var inline11084 int = _goml_runtime_core_string_len(value__15)
        t6837 = inline11084
        var t6838 bool = index__16 == t6837
        if t6838 {
            return true
        } else {
            var t6839 uint8
            var inline11082 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6839 = inline11082
            var t6840_rhs uint8 = 192
            var t6840 uint8 = t6839 & t6840_rhs
            var t6841 bool = t6840 != 128
            return t6841
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6853 bool = string_is_char_boundary(value__21, start__22)
    var jp6850 bool
    if t6853 {
        var t6854 bool = string_is_char_boundary(value__21, end__23)
        jp6850 = t6854
    } else {
        jp6850 = false
    }
    if jp6850 {
        var t6851 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6851
    } else {
        var t6852 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6852
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t6880 bool
    var inline11088 bool = value__30 <= 1114111
    if inline11088 {
        var inline11089 bool = value__30 >= 55296
        var inline11091 bool
        if inline11089 {
            var inline11093 bool = value__30 <= 57343
            inline11091 = inline11093
        } else {
            inline11091 = false
        }
        var inline11092 bool = !inline11091
        t6880 = inline11092
    } else {
        t6880 = false
    }
    if t6880 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t6881 Option__char = Option__char_Some{
            _0: x24,
        }
        return t6881
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t6888 string = _goml_runtime_core_int_to_string(self__67)
    return t6888
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t6891 string = _goml_runtime_core_bool_to_string(self__64)
    return t6891
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6894 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6894
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11205 rune
    var inline11097 bool = utf8_valid_scalar(value__0)
    if inline11097 {
        var inline11098 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11099 rune = inline11098._1
        commute_field11205 = inline11099
        var t6900 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11205,
            _2: width__1,
        }
        return t6900
    } else {
        var inline11095 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11095
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6905 bool = value__3 < 128
    if t6905 {
        return true
    } else {
        var t6906 bool = value__3 > 191
        return t6906
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6911 bool = value__4 <= 1114111
    if t6911 {
        var t6915 bool = value__4 >= 55296
        var jp6913 bool
        if t6915 {
            var t6916 bool = value__4 <= 57343
            jp6913 = t6916
        } else {
            jp6913 = false
        }
        var t6914 bool = !jp6913
        return t6914
    } else {
        return false
    }
}

func main() {
    main0()
}
