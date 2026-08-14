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
    var inline8411 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline8411
    var t2671 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2671
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8426 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8426
    var t2685 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2685, length__5)
    var for_index1 int = 0
    Loop_loop2687:
    for {
        var t2688 bool = for_index1 < length__5
        if t2688 {
            var for_item3 int = for_index1
            var t2689 int = for_index1 + 1
            for_index1 = t2689
            var t2690 *_goml_vec_uint8 = self__3.values
            var t2691 uint8
            var inline8422 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2691 = inline8422
            vec_push__Vec_5uint8(t2690, t2691)
            continue
        } else {
            break Loop_loop2687
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2694 string
    var inline8428 string = char_to_string(value__8)
    t2694 = inline8428
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2694)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4635 string = "" + message__201
    var t4636 string = t4635 + " at byte "
    var t4637 *ref_int_x = value__200.index
    var t4638 int
    var inline9947 int = ref_get__Ref_3int(t4637)
    t4638 = inline9947
    var t4639 string
    var inline9945 string = _goml_runtime_core_int_to_string(t4638)
    t4639 = inline9945
    var t4640 string = t4636 + t4639
    return t4640
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4655:
    for {
        var t4663 *ref_int_x = value__203.index
        var t4664 int
        var inline9968 int = ref_get__Ref_3int(t4663)
        t4664 = inline9968
        var t4665 string = value__203.input
        var t4666 int
        var inline9966 int = _goml_runtime_core_string_len(t4665)
        t4666 = inline9966
        var t4667 bool = t4664 < t4666
        var jp4657 bool
        if t4667 {
            var t4668 string = value__203.input
            var t4669 *ref_int_x = value__203.index
            var t4670 int
            var inline9960 int = ref_get__Ref_3int(t4669)
            t4670 = inline9960
            var t4671 uint8
            var inline9958 uint8 = _goml_runtime_core_string_byte_get(t4668, t4670)
            t4671 = inline9958
            var inline9949 bool = t4671 == 9
            var inline9951 bool
            if inline9949 {
                inline9951 = true
            } else {
                var inline9956 bool = t4671 == 10
                inline9951 = inline9956
            }
            var inline9953 bool
            if inline9951 {
                inline9953 = true
            } else {
                var inline9955 bool = t4671 == 13
                inline9953 = inline9955
            }
            if inline9953 {
                jp4657 = true
            } else {
                var inline9954 bool = t4671 == 32
                jp4657 = inline9954
            }
        } else {
            jp4657 = false
        }
        if jp4657 {
            var t4658 *ref_int_x = value__203.index
            var t4659 *ref_int_x = value__203.index
            var t4660 int
            var inline9964 int = ref_get__Ref_3int(t4659)
            t4660 = inline9964
            var t4661 int = t4660 + 1
            ref_set__Ref_3int(t4658, t4661)
            continue
        } else {
            break Loop_loop4655
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4702 bool = value__204 >= 48
    var jp4678 bool
    if t4702 {
        var t4703 bool = value__204 <= 57
        jp4678 = t4703
    } else {
        jp4678 = false
    }
    if jp4678 {
        var t4679 uint8 = value__204 - 48
        var t4680 uint32 = uint32(uint8(t4679))
        var t4681 Option__uint32 = Option__uint32_Some{
            _0: t4680,
        }
        return t4681
    } else {
        var t4700 bool = value__204 >= 65
        var jp4685 bool
        if t4700 {
            var t4701 bool = value__204 <= 70
            jp4685 = t4701
        } else {
            jp4685 = false
        }
        if jp4685 {
            var t4686 uint8 = value__204 - 65
            var t4687 uint8 = t4686 + 10
            var t4688 uint32 = uint32(uint8(t4687))
            var t4689 Option__uint32 = Option__uint32_Some{
                _0: t4688,
            }
            return t4689
        } else {
            var t4698 bool = value__204 >= 97
            var jp4693 bool
            if t4698 {
                var t4699 bool = value__204 <= 102
                jp4693 = t4699
            } else {
                jp4693 = false
            }
            if jp4693 {
                var t4694 uint8 = value__204 - 97
                var t4695 uint8 = t4694 + 10
                var t4696 uint32 = uint32(uint8(t4695))
                var t4697 Option__uint32 = Option__uint32_Some{
                    _0: t4696,
                }
                return t4697
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4708 *ref_int_x = value__205.index
    var t4709 int
    var inline9996 int = ref_get__Ref_3int(t4708)
    t4709 = inline9996
    var t4710 int = t4709 + 4
    var t4711 string = value__205.input
    var t4712 int
    var inline9994 int = _goml_runtime_core_string_len(t4711)
    t4712 = inline9994
    var t4713 bool = t4710 > t4712
    if t4713 {
        var t4714 string
        var inline9970 string = "incomplete unicode escape"
        var inline9971 string = "" + inline9970
        var inline9972 string = inline9971 + " at byte "
        var inline9973 *ref_int_x = value__205.index
        var inline9974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9973)
        var inline9975 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9974)
        var inline9976 string = inline9972 + inline9975
        t4714 = inline9976
        var t4715 Result__uint32__string = Result__uint32__string_Err{
            _0: t4714,
        }
        return t4715
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4722:
        for {
            var t4723 bool = for_index744 < for_limit745
            if t4723 {
                var for_item746 int = for_index744
                var t4724 int = for_index744 + 1
                for_index744 = t4724
                var t4725 string = value__205.input
                var t4726 *ref_int_x = value__205.index
                var t4727 int
                var inline9988 int = ref_get__Ref_3int(t4726)
                t4727 = inline9988
                var t4728 int = t4727 + for_item746
                var t4729 uint8
                var inline9986 uint8 = _goml_runtime_core_string_byte_get(t4725, t4728)
                t4729 = inline9986
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4729)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4731 string
                    var inline9978 string = "invalid unicode escape"
                    var inline9979 string = "" + inline9978
                    var inline9980 string = inline9979 + " at byte "
                    var inline9981 *ref_int_x = value__205.index
                    var inline9982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9981)
                    var inline9983 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9982)
                    var inline9984 string = inline9980 + inline9983
                    t4731 = inline9984
                    var t4732 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4731,
                    }
                    return t4732
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4733 uint32 = result__206 * 16
                    var t4734 uint32 = t4733 + x749
                    result__206 = t4734
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4722
            }
        }
        var t4717 *ref_int_x = value__205.index
        var t4718 *ref_int_x = value__205.index
        var t4719 int
        var inline9992 int = ref_get__Ref_3int(t4718)
        t4719 = inline9992
        var t4720 int = t4719 + 4
        ref_set__Ref_3int(t4717, t4720)
        var t4721 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4721
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline10009 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline10009
    switch mtmp753.(type) {
    case Option__char_None:
        var t4739 string
        var inline9998 string = "invalid unicode codepoint"
        var inline9999 string = "" + inline9998
        var inline10000 string = inline9999 + " at byte "
        var inline10001 *ref_int_x = value__209.index
        var inline10002 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10001)
        var inline10003 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10002)
        var inline10004 string = inline10000 + inline10003
        t4739 = inline10004
        var t4740 Result__unit__string = Result__unit__string_Err{
            _0: t4739,
        }
        return t4740
    case Option__char_Some:
        var x754 rune = mtmp753.(Option__char_Some)._0
        var inline10006 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10006)
        var t4741 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4741
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4745 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4745 = x757
        var t4805 bool = jp4745 >= 55296
        var jp4749 bool
        if t4805 {
            var t4806 bool = jp4745 <= 56319
            jp4749 = t4806
        } else {
            jp4749 = false
        }
        if jp4749 {
            var t4785 *ref_int_x = value__213.index
            var t4786 int
            var inline10049 int = ref_get__Ref_3int(t4785)
            t4786 = inline10049
            var t4787 int = t4786 + 2
            var t4788 string = value__213.input
            var t4789 int
            var inline10047 int = _goml_runtime_core_string_len(t4788)
            t4789 = inline10047
            var t4790 bool = t4787 > t4789
            var jp4778 bool
            if t4790 {
                jp4778 = true
            } else {
                var t4791 string = value__213.input
                var t4792 *ref_int_x = value__213.index
                var t4793 int
                var inline10013 int = ref_get__Ref_3int(t4792)
                t4793 = inline10013
                var t4794 uint8
                var inline10011 uint8 = _goml_runtime_core_string_byte_get(t4791, t4793)
                t4794 = inline10011
                var t4795 bool = t4794 != 92
                jp4778 = t4795
            }
            var jp4753 bool
            if jp4778 {
                jp4753 = true
            } else {
                var t4779 string = value__213.input
                var t4780 *ref_int_x = value__213.index
                var t4781 int
                var inline10017 int = ref_get__Ref_3int(t4780)
                t4781 = inline10017
                var t4782 int = t4781 + 1
                var t4783 uint8
                var inline10015 uint8 = _goml_runtime_core_string_byte_get(t4779, t4782)
                t4783 = inline10015
                var t4784 bool = t4783 != 117
                jp4753 = t4784
            }
            if jp4753 {
                var t4754 string
                var inline10019 string = "missing low surrogate"
                var inline10020 string = "" + inline10019
                var inline10021 string = inline10020 + " at byte "
                var inline10022 *ref_int_x = value__213.index
                var inline10023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10022)
                var inline10024 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10023)
                var inline10025 string = inline10021 + inline10024
                t4754 = inline10025
                var t4755 Result__unit__string = Result__unit__string_Err{
                    _0: t4754,
                }
                return t4755
            } else {
                var t4756 *ref_int_x = value__213.index
                var t4757 *ref_int_x = value__213.index
                var t4758 int
                var inline10045 int = ref_get__Ref_3int(t4757)
                t4758 = inline10045
                var t4759 int = t4758 + 2
                ref_set__Ref_3int(t4756, t4759)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4761 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4761 = x761
                    var t4774 bool = jp4761 < 56320
                    var jp4765 bool
                    if t4774 {
                        jp4765 = true
                    } else {
                        var t4775 bool = jp4761 > 57343
                        jp4765 = t4775
                    }
                    if jp4765 {
                        var t4766 string
                        var inline10027 string = "invalid low surrogate"
                        var inline10028 string = "" + inline10027
                        var inline10029 string = inline10028 + " at byte "
                        var inline10030 *ref_int_x = value__213.index
                        var inline10031 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10030)
                        var inline10032 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10031)
                        var inline10033 string = inline10029 + inline10032
                        t4766 = inline10033
                        var t4767 Result__unit__string = Result__unit__string_Err{
                            _0: t4766,
                        }
                        return t4767
                    } else {
                        var t4768 uint32 = jp4745 - 55296
                        var t4769 uint32 = t4768 * 1024
                        var t4770 uint32 = 65536 + t4769
                        var t4771 uint32 = t4770 + jp4761
                        var t4772 uint32 = t4771 - 56320
                        var inline10035 Option__char = char_from_uint32(t4772)
                        switch inline10035.(type) {
                        case Option__char_None:
                            var inline10036 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10037 Result__unit__string = Result__unit__string_Err{
                                _0: inline10036,
                            }
                            return inline10037
                        case Option__char_Some:
                            var inline10038 rune = inline10035.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10038)
                            var inline10041 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline10041
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4776 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4776
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4803 bool = jp4745 >= 56320
            var jp4799 bool
            if t4803 {
                var t4804 bool = jp4745 <= 57343
                jp4799 = t4804
            } else {
                jp4799 = false
            }
            if jp4799 {
                var t4800 string
                var inline10051 string = "unexpected low surrogate"
                var inline10052 string = "" + inline10051
                var inline10053 string = inline10052 + " at byte "
                var inline10054 *ref_int_x = value__213.index
                var inline10055 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10054)
                var inline10056 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10055)
                var inline10057 string = inline10053 + inline10056
                t4800 = inline10057
                var t4801 Result__unit__string = Result__unit__string_Err{
                    _0: t4800,
                }
                return t4801
            } else {
                var t4802 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4745)
                return t4802
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4807 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4807
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4923 *ref_int_x = value__217.index
    var t4924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4923)
    var t4925 string = value__217.input
    var t4926 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4925)
    var t4927 bool = t4924 >= t4926
    var jp4915 bool
    if t4927 {
        jp4915 = true
    } else {
        var t4928 string = value__217.input
        var t4929 *ref_int_x = value__217.index
        var t4930 int
        var inline10061 int = ref_get__Ref_3int(t4929)
        t4930 = inline10061
        var t4931 uint8
        var inline10059 uint8 = _goml_runtime_core_string_byte_get(t4928, t4930)
        t4931 = inline10059
        var t4932 bool = t4931 != 34
        jp4915 = t4932
    }
    if jp4915 {
        var t4916 string
        var inline10063 string = "expected string"
        var inline10064 string = "" + inline10063
        var inline10065 string = inline10064 + " at byte "
        var inline10066 *ref_int_x = value__217.index
        var inline10067 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10066)
        var inline10068 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10067)
        var inline10069 string = inline10065 + inline10068
        t4916 = inline10069
        var t4917 Result__string__string = Result__string__string_Err{
            _0: t4916,
        }
        return t4917
    } else {
        var t4918 *ref_int_x = value__217.index
        var t4919 *ref_int_x = value__217.index
        var t4920 int
        var inline10073 int = ref_get__Ref_3int(t4919)
        t4920 = inline10073
        var t4921 int = t4920 + 1
        ref_set__Ref_3int(t4918, t4921)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4811 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4811)
        Loop_loop4815:
        for {
            var t4816 *ref_int_x = value__217.index
            var t4817 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4816)
            var t4818 string = value__217.input
            var t4819 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4818)
            var t4820 bool = t4817 < t4819
            if t4820 {
                var t4821 string = value__217.input
                var t4822 *ref_int_x = value__217.index
                var t4823 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4822)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4821, t4823)
                var t4825 bool = byte__220 == 34
                if t4825 {
                    var t4833 *ref_int_x = value__217.index
                    var t4834 int
                    var inline10088 int = ref_get__Ref_3int(t4833)
                    t4834 = inline10088
                    var t4835 bool = segment__219 < t4834
                    if t4835 {
                        var t4836 string = value__217.input
                        var t4837 *ref_int_x = value__217.index
                        var t4838 int
                        var inline10077 int = ref_get__Ref_3int(t4837)
                        t4838 = inline10077
                        var t4839 string
                        var inline10075 string = string_byte_slice(t4836, segment__219, t4838)
                        t4839 = inline10075
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4839)
                    } else {}
                    var t4827 *ref_int_x = value__217.index
                    var t4828 *ref_int_x = value__217.index
                    var t4829 int
                    var inline10086 int = ref_get__Ref_3int(t4828)
                    t4829 = inline10086
                    var t4830 int = t4829 + 1
                    ref_set__Ref_3int(t4827, t4830)
                    var t4831 string
                    var inline10079 *_goml_vec_uint8 = builder__218.values
                    var inline10080 Tuple2_4bool_6string = string_from_utf8(inline10079)
                    var inline10081 string = inline10080._1
                    t4831 = inline10081
                    var t4832 Result__string__string = Result__string__string_Ok{
                        _0: t4831,
                    }
                    return t4832
                } else {
                    var t4842 bool = byte__220 == 92
                    if t4842 {
                        var t4897 *ref_int_x = value__217.index
                        var t4898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4897)
                        var t4899 bool = segment__219 < t4898
                        if t4899 {
                            var t4900 string = value__217.input
                            var t4901 *ref_int_x = value__217.index
                            var t4902 int
                            var inline10092 int = ref_get__Ref_3int(t4901)
                            t4902 = inline10092
                            var t4903 string
                            var inline10090 string = string_byte_slice(t4900, segment__219, t4902)
                            t4903 = inline10090
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4903)
                        } else {}
                        var t4844 *ref_int_x = value__217.index
                        var t4845 *ref_int_x = value__217.index
                        var t4846 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4845)
                        var t4847 int = t4846 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4844, t4847)
                        var t4890 *ref_int_x = value__217.index
                        var t4891 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4890)
                        var t4892 string = value__217.input
                        var t4893 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4892)
                        var t4894 bool = t4891 >= t4893
                        if t4894 {
                            var t4895 string
                            var inline10094 string = "incomplete escape"
                            var inline10095 string = "" + inline10094
                            var inline10096 string = inline10095 + " at byte "
                            var inline10097 *ref_int_x = value__217.index
                            var inline10098 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10097)
                            var inline10099 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10098)
                            var inline10100 string = inline10096 + inline10099
                            t4895 = inline10100
                            var t4896 Result__string__string = Result__string__string_Err{
                                _0: t4895,
                            }
                            return t4896
                        } else {
                            var t4849 string = value__217.input
                            var t4850 *ref_int_x = value__217.index
                            var t4851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4850)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4849, t4851)
                            var t4852 *ref_int_x = value__217.index
                            var t4853 *ref_int_x = value__217.index
                            var t4854 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4853)
                            var t4855 int = t4854 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4852, t4855)
                            var t4859 bool = escape__221 == 34
                            if t4859 {
                                var inline10102 rune = 34
                                var inline10103 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10102)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10103)
                                var t4857 *ref_int_x = value__217.index
                                var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                segment__219 = t4858
                                continue
                            } else {
                                var t4862 bool = escape__221 == 92
                                if t4862 {
                                    var inline10106 rune = 92
                                    var inline10107 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10106)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10107)
                                    var t4857 *ref_int_x = value__217.index
                                    var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                    segment__219 = t4858
                                    continue
                                } else {
                                    var t4865 bool = escape__221 == 47
                                    if t4865 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4857 *ref_int_x = value__217.index
                                        var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                        segment__219 = t4858
                                        continue
                                    } else {
                                        var t4868 bool = escape__221 == 98
                                        if t4868 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4857 *ref_int_x = value__217.index
                                                var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                segment__219 = t4858
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4857 *ref_int_x = value__217.index
                                                var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                segment__219 = t4858
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4872 bool = escape__221 == 102
                                            if t4872 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4857 *ref_int_x = value__217.index
                                                    var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                    segment__219 = t4858
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4857 *ref_int_x = value__217.index
                                                    var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                    segment__219 = t4858
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4876 bool = escape__221 == 110
                                                if t4876 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4857 *ref_int_x = value__217.index
                                                    var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                    segment__219 = t4858
                                                    continue
                                                } else {
                                                    var t4879 bool = escape__221 == 114
                                                    if t4879 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4857 *ref_int_x = value__217.index
                                                        var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                        segment__219 = t4858
                                                        continue
                                                    } else {
                                                        var t4882 bool = escape__221 == 116
                                                        if t4882 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4857 *ref_int_x = value__217.index
                                                            var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                            segment__219 = t4858
                                                            continue
                                                        } else {
                                                            var t4885 bool = escape__221 == 117
                                                            if t4885 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4857 *ref_int_x = value__217.index
                                                                    var t4858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4857)
                                                                    segment__219 = t4858
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4887 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4887
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4888 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4889 Result__string__string = Result__string__string_Err{
                                                                    _0: t4888,
                                                                }
                                                                return t4889
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
                        var t4906 bool = byte__220 < 32
                        if t4906 {
                            var t4907 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4908 Result__string__string = Result__string__string_Err{
                                _0: t4907,
                            }
                            return t4908
                        } else {
                            var t4909 *ref_int_x = value__217.index
                            var t4910 *ref_int_x = value__217.index
                            var t4911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4910)
                            var t4912 int = t4911 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4909, t4912)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4815
            }
        }
        var t4813 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4814 Result__string__string = Result__string__string_Err{
            _0: t4813,
        }
        return t4814
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4941 *ref_int_x = value__225.index
    var start__226 int
    var inline10127 int = ref_get__Ref_3int(t4941)
    start__226 = inline10127
    Loop_loop4946:
    for {
        var t4954 *ref_int_x = value__225.index
        var t4955 int
        var inline10123 int = ref_get__Ref_3int(t4954)
        t4955 = inline10123
        var t4956 string = value__225.input
        var t4957 int
        var inline10121 int = _goml_runtime_core_string_len(t4956)
        t4957 = inline10121
        var t4958 bool = t4955 < t4957
        var jp4948 bool
        if t4958 {
            var t4959 string = value__225.input
            var t4960 *ref_int_x = value__225.index
            var t4961 int
            var inline10115 int = ref_get__Ref_3int(t4960)
            t4961 = inline10115
            var t4962 uint8
            var inline10113 uint8 = _goml_runtime_core_string_byte_get(t4959, t4961)
            t4962 = inline10113
            var inline10110 bool = t4962 >= 48
            if inline10110 {
                var inline10111 bool = t4962 <= 57
                jp4948 = inline10111
            } else {
                jp4948 = false
            }
        } else {
            jp4948 = false
        }
        if jp4948 {
            var t4949 *ref_int_x = value__225.index
            var t4950 *ref_int_x = value__225.index
            var t4951 int
            var inline10119 int = ref_get__Ref_3int(t4950)
            t4951 = inline10119
            var t4952 int = t4951 + 1
            ref_set__Ref_3int(t4949, t4952)
            continue
        } else {
            break Loop_loop4946
        }
    }
    var t4943 *ref_int_x = value__225.index
    var t4944 int
    var inline10125 int = ref_get__Ref_3int(t4943)
    t4944 = inline10125
    var t4945 bool = t4944 > start__226
    return t4945
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4966 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4966)
    var t5087 string = value__227.input
    var t5088 *ref_int_x = value__227.index
    var t5089 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5088)
    var t5090 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5087, t5089)
    var t5091 bool = t5090 == 45
    if t5091 {
        var t5092 *ref_int_x = value__227.index
        var t5093 *ref_int_x = value__227.index
        var t5094 int
        var inline10131 int = ref_get__Ref_3int(t5093)
        t5094 = inline10131
        var t5095 int = t5094 + 1
        ref_set__Ref_3int(t5092, t5095)
    } else {}
    var t5050 *ref_int_x = value__227.index
    var t5051 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5050)
    var t5052 string = value__227.input
    var t5053 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5052)
    var t5054 bool = t5051 >= t5053
    if t5054 {
        var t5055 string
        var inline10133 string = "incomplete number"
        var inline10134 string = "" + inline10133
        var inline10135 string = inline10134 + " at byte "
        var inline10136 *ref_int_x = value__227.index
        var inline10137 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10136)
        var inline10138 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10137)
        var inline10139 string = inline10135 + inline10138
        t5055 = inline10139
        var t5056 Result__string__string = Result__string__string_Err{
            _0: t5055,
        }
        return t5056
    } else {
        var t5058 string = value__227.input
        var t5059 *ref_int_x = value__227.index
        var t5060 int
        var inline10174 int = ref_get__Ref_3int(t5059)
        t5060 = inline10174
        var t5061 uint8
        var inline10172 uint8 = _goml_runtime_core_string_byte_get(t5058, t5060)
        t5061 = inline10172
        var t5062 bool = t5061 == 48
        if t5062 {
            var t5063 *ref_int_x = value__227.index
            var t5064 *ref_int_x = value__227.index
            var t5065 int
            var inline10162 int = ref_get__Ref_3int(t5064)
            t5065 = inline10162
            var t5066 int = t5065 + 1
            ref_set__Ref_3int(t5063, t5066)
            var t5072 *ref_int_x = value__227.index
            var t5073 int
            var inline10158 int = ref_get__Ref_3int(t5072)
            t5073 = inline10158
            var t5074 string = value__227.input
            var t5075 int
            var inline10156 int = _goml_runtime_core_string_len(t5074)
            t5075 = inline10156
            var t5076 bool = t5073 < t5075
            var jp5069 bool
            if t5076 {
                var t5077 string = value__227.input
                var t5078 *ref_int_x = value__227.index
                var t5079 int
                var inline10146 int = ref_get__Ref_3int(t5078)
                t5079 = inline10146
                var t5080 uint8
                var inline10144 uint8 = _goml_runtime_core_string_byte_get(t5077, t5079)
                t5080 = inline10144
                var inline10141 bool = t5080 >= 48
                if inline10141 {
                    var inline10142 bool = t5080 <= 57
                    jp5069 = inline10142
                } else {
                    jp5069 = false
                }
            } else {
                jp5069 = false
            }
            if jp5069 {
                var t5070 string
                var inline10148 string = "invalid leading zero"
                var inline10149 string = "" + inline10148
                var inline10150 string = inline10149 + " at byte "
                var inline10151 *ref_int_x = value__227.index
                var inline10152 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10151)
                var inline10153 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10152)
                var inline10154 string = inline10150 + inline10153
                t5070 = inline10154
                var t5071 Result__string__string = Result__string__string_Err{
                    _0: t5070,
                }
                return t5071
            } else {
                var t5040 *ref_int_x = value__227.index
                var t5041 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5040)
                var t5042 string = value__227.input
                var t5043 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5042)
                var t5044 bool = t5041 < t5043
                var jp5030 bool
                if t5044 {
                    var t5045 string = value__227.input
                    var t5046 *ref_int_x = value__227.index
                    var t5047 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5046)
                    var t5048 uint8
                    var inline10176 uint8 = _goml_runtime_core_string_byte_get(t5045, t5047)
                    t5048 = inline10176
                    var t5049 bool = t5048 == 46
                    jp5030 = t5049
                } else {
                    jp5030 = false
                }
                if jp5030 {
                    var t5031 *ref_int_x = value__227.index
                    var t5032 *ref_int_x = value__227.index
                    var t5033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5032)
                    var t5034 int = t5033 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5031, t5034)
                    var t5036 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5037 bool = !t5036
                    if t5037 {
                        var t5038 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5039 Result__string__string = Result__string__string_Err{
                            _0: t5038,
                        }
                        return t5039
                    } else {
                        var t5012 *ref_int_x = value__227.index
                        var t5013 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5012)
                        var t5014 string = value__227.input
                        var t5015 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5014)
                        var t5016 bool = t5013 < t5015
                        var jp4977 bool
                        if t5016 {
                            var t5019 string = value__227.input
                            var t5020 *ref_int_x = value__227.index
                            var t5021 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5020)
                            var t5022 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5019, t5021)
                            var t5023 bool = t5022 == 101
                            if t5023 {
                                jp4977 = true
                            } else {
                                var t5024 string = value__227.input
                                var t5025 *ref_int_x = value__227.index
                                var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                                var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                                var t5028 bool = t5027 == 69
                                jp4977 = t5028
                            }
                        } else {
                            jp4977 = false
                        }
                        if jp4977 {
                            var t4978 *ref_int_x = value__227.index
                            var t4979 *ref_int_x = value__227.index
                            var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                            var t4981 int = t4980 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4978, t4981)
                            var t4995 *ref_int_x = value__227.index
                            var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4995)
                            var t4997 string = value__227.input
                            var t4998 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4997)
                            var t4999 bool = t4996 < t4998
                            var jp4989 bool
                            if t4999 {
                                var t5002 string = value__227.input
                                var t5003 *ref_int_x = value__227.index
                                var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                                var t5005 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5002, t5004)
                                var t5006 bool = t5005 == 43
                                if t5006 {
                                    jp4989 = true
                                } else {
                                    var t5007 string = value__227.input
                                    var t5008 *ref_int_x = value__227.index
                                    var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                                    var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                                    var t5011 bool = t5010 == 45
                                    jp4989 = t5011
                                }
                            } else {
                                jp4989 = false
                            }
                            if jp4989 {
                                var t4990 *ref_int_x = value__227.index
                                var t4991 *ref_int_x = value__227.index
                                var t4992 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4991)
                                var t4993 int = t4992 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4990, t4993)
                            } else {}
                            var t4984 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4985 bool = !t4984
                            if t4985 {
                                var t4986 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4987 Result__string__string = Result__string__string_Err{
                                    _0: t4986,
                                }
                                return t4987
                            } else {
                                var t4971 string = value__227.input
                                var t4972 *ref_int_x = value__227.index
                                var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                                var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                                var t4975 Result__string__string = Result__string__string_Ok{
                                    _0: t4974,
                                }
                                return t4975
                            }
                        } else {
                            var t4971 string = value__227.input
                            var t4972 *ref_int_x = value__227.index
                            var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                            var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                            var t4975 Result__string__string = Result__string__string_Ok{
                                _0: t4974,
                            }
                            return t4975
                        }
                    }
                } else {
                    var t5012 *ref_int_x = value__227.index
                    var t5013 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5012)
                    var t5014 string = value__227.input
                    var t5015 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5014)
                    var t5016 bool = t5013 < t5015
                    var jp4977 bool
                    if t5016 {
                        var t5019 string = value__227.input
                        var t5020 *ref_int_x = value__227.index
                        var t5021 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5020)
                        var t5022 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5019, t5021)
                        var t5023 bool = t5022 == 101
                        if t5023 {
                            jp4977 = true
                        } else {
                            var t5024 string = value__227.input
                            var t5025 *ref_int_x = value__227.index
                            var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                            var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                            var t5028 bool = t5027 == 69
                            jp4977 = t5028
                        }
                    } else {
                        jp4977 = false
                    }
                    if jp4977 {
                        var t4978 *ref_int_x = value__227.index
                        var t4979 *ref_int_x = value__227.index
                        var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                        var t4981 int = t4980 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4978, t4981)
                        var t4995 *ref_int_x = value__227.index
                        var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4995)
                        var t4997 string = value__227.input
                        var t4998 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4997)
                        var t4999 bool = t4996 < t4998
                        var jp4989 bool
                        if t4999 {
                            var t5002 string = value__227.input
                            var t5003 *ref_int_x = value__227.index
                            var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                            var t5005 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5002, t5004)
                            var t5006 bool = t5005 == 43
                            if t5006 {
                                jp4989 = true
                            } else {
                                var t5007 string = value__227.input
                                var t5008 *ref_int_x = value__227.index
                                var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                                var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                                var t5011 bool = t5010 == 45
                                jp4989 = t5011
                            }
                        } else {
                            jp4989 = false
                        }
                        if jp4989 {
                            var t4990 *ref_int_x = value__227.index
                            var t4991 *ref_int_x = value__227.index
                            var t4992 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4991)
                            var t4993 int = t4992 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4990, t4993)
                        } else {}
                        var t4984 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4985 bool = !t4984
                        if t4985 {
                            var t4986 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4987 Result__string__string = Result__string__string_Err{
                                _0: t4986,
                            }
                            return t4987
                        } else {
                            var t4971 string = value__227.input
                            var t4972 *ref_int_x = value__227.index
                            var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                            var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                            var t4975 Result__string__string = Result__string__string_Ok{
                                _0: t4974,
                            }
                            return t4975
                        }
                    } else {
                        var t4971 string = value__227.input
                        var t4972 *ref_int_x = value__227.index
                        var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                        var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                        var t4975 Result__string__string = Result__string__string_Ok{
                            _0: t4974,
                        }
                        return t4975
                    }
                }
            }
        } else {
            var t5083 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5084 bool = !t5083
            if t5084 {
                var t5085 string
                var inline10164 string = "expected number"
                var inline10165 string = "" + inline10164
                var inline10166 string = inline10165 + " at byte "
                var inline10167 *ref_int_x = value__227.index
                var inline10168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10167)
                var inline10169 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10168)
                var inline10170 string = inline10166 + inline10169
                t5085 = inline10170
                var t5086 Result__string__string = Result__string__string_Err{
                    _0: t5085,
                }
                return t5086
            } else {
                var t5040 *ref_int_x = value__227.index
                var t5041 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5040)
                var t5042 string = value__227.input
                var t5043 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5042)
                var t5044 bool = t5041 < t5043
                var jp5030 bool
                if t5044 {
                    var t5045 string = value__227.input
                    var t5046 *ref_int_x = value__227.index
                    var t5047 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5046)
                    var t5048 uint8
                    var inline10176 uint8 = _goml_runtime_core_string_byte_get(t5045, t5047)
                    t5048 = inline10176
                    var t5049 bool = t5048 == 46
                    jp5030 = t5049
                } else {
                    jp5030 = false
                }
                if jp5030 {
                    var t5031 *ref_int_x = value__227.index
                    var t5032 *ref_int_x = value__227.index
                    var t5033 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5032)
                    var t5034 int = t5033 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5031, t5034)
                    var t5036 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5037 bool = !t5036
                    if t5037 {
                        var t5038 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5039 Result__string__string = Result__string__string_Err{
                            _0: t5038,
                        }
                        return t5039
                    } else {
                        var t5012 *ref_int_x = value__227.index
                        var t5013 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5012)
                        var t5014 string = value__227.input
                        var t5015 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5014)
                        var t5016 bool = t5013 < t5015
                        var jp4977 bool
                        if t5016 {
                            var t5019 string = value__227.input
                            var t5020 *ref_int_x = value__227.index
                            var t5021 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5020)
                            var t5022 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5019, t5021)
                            var t5023 bool = t5022 == 101
                            if t5023 {
                                jp4977 = true
                            } else {
                                var t5024 string = value__227.input
                                var t5025 *ref_int_x = value__227.index
                                var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                                var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                                var t5028 bool = t5027 == 69
                                jp4977 = t5028
                            }
                        } else {
                            jp4977 = false
                        }
                        if jp4977 {
                            var t4978 *ref_int_x = value__227.index
                            var t4979 *ref_int_x = value__227.index
                            var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                            var t4981 int = t4980 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4978, t4981)
                            var t4995 *ref_int_x = value__227.index
                            var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4995)
                            var t4997 string = value__227.input
                            var t4998 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4997)
                            var t4999 bool = t4996 < t4998
                            var jp4989 bool
                            if t4999 {
                                var t5002 string = value__227.input
                                var t5003 *ref_int_x = value__227.index
                                var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                                var t5005 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5002, t5004)
                                var t5006 bool = t5005 == 43
                                if t5006 {
                                    jp4989 = true
                                } else {
                                    var t5007 string = value__227.input
                                    var t5008 *ref_int_x = value__227.index
                                    var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                                    var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                                    var t5011 bool = t5010 == 45
                                    jp4989 = t5011
                                }
                            } else {
                                jp4989 = false
                            }
                            if jp4989 {
                                var t4990 *ref_int_x = value__227.index
                                var t4991 *ref_int_x = value__227.index
                                var t4992 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4991)
                                var t4993 int = t4992 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4990, t4993)
                            } else {}
                            var t4984 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4985 bool = !t4984
                            if t4985 {
                                var t4986 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4987 Result__string__string = Result__string__string_Err{
                                    _0: t4986,
                                }
                                return t4987
                            } else {
                                var t4971 string = value__227.input
                                var t4972 *ref_int_x = value__227.index
                                var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                                var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                                var t4975 Result__string__string = Result__string__string_Ok{
                                    _0: t4974,
                                }
                                return t4975
                            }
                        } else {
                            var t4971 string = value__227.input
                            var t4972 *ref_int_x = value__227.index
                            var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                            var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                            var t4975 Result__string__string = Result__string__string_Ok{
                                _0: t4974,
                            }
                            return t4975
                        }
                    }
                } else {
                    var t5012 *ref_int_x = value__227.index
                    var t5013 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5012)
                    var t5014 string = value__227.input
                    var t5015 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5014)
                    var t5016 bool = t5013 < t5015
                    var jp4977 bool
                    if t5016 {
                        var t5019 string = value__227.input
                        var t5020 *ref_int_x = value__227.index
                        var t5021 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5020)
                        var t5022 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5019, t5021)
                        var t5023 bool = t5022 == 101
                        if t5023 {
                            jp4977 = true
                        } else {
                            var t5024 string = value__227.input
                            var t5025 *ref_int_x = value__227.index
                            var t5026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5025)
                            var t5027 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5024, t5026)
                            var t5028 bool = t5027 == 69
                            jp4977 = t5028
                        }
                    } else {
                        jp4977 = false
                    }
                    if jp4977 {
                        var t4978 *ref_int_x = value__227.index
                        var t4979 *ref_int_x = value__227.index
                        var t4980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4979)
                        var t4981 int = t4980 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4978, t4981)
                        var t4995 *ref_int_x = value__227.index
                        var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4995)
                        var t4997 string = value__227.input
                        var t4998 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4997)
                        var t4999 bool = t4996 < t4998
                        var jp4989 bool
                        if t4999 {
                            var t5002 string = value__227.input
                            var t5003 *ref_int_x = value__227.index
                            var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                            var t5005 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5002, t5004)
                            var t5006 bool = t5005 == 43
                            if t5006 {
                                jp4989 = true
                            } else {
                                var t5007 string = value__227.input
                                var t5008 *ref_int_x = value__227.index
                                var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
                                var t5010 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5007, t5009)
                                var t5011 bool = t5010 == 45
                                jp4989 = t5011
                            }
                        } else {
                            jp4989 = false
                        }
                        if jp4989 {
                            var t4990 *ref_int_x = value__227.index
                            var t4991 *ref_int_x = value__227.index
                            var t4992 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4991)
                            var t4993 int = t4992 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4990, t4993)
                        } else {}
                        var t4984 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4985 bool = !t4984
                        if t4985 {
                            var t4986 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4987 Result__string__string = Result__string__string_Err{
                                _0: t4986,
                            }
                            return t4987
                        } else {
                            var t4971 string = value__227.input
                            var t4972 *ref_int_x = value__227.index
                            var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                            var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                            var t4975 Result__string__string = Result__string__string_Ok{
                                _0: t4974,
                            }
                            return t4975
                        }
                    } else {
                        var t4971 string = value__227.input
                        var t4972 *ref_int_x = value__227.index
                        var t4973 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4972)
                        var t4974 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4971, start__228, t4973)
                        var t4975 Result__string__string = Result__string__string_Ok{
                            _0: t4974,
                        }
                        return t4975
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5118 *ref_int_x = value__230.index
    var t5119 int
    var inline10204 int = ref_get__Ref_3int(t5118)
    t5119 = inline10204
    var t5120 int
    var inline10202 int = _goml_runtime_core_string_len(expected__231)
    t5120 = inline10202
    var t5121 int = t5119 + t5120
    var t5122 string = value__230.input
    var t5123 int
    var inline10200 int = _goml_runtime_core_string_len(t5122)
    t5123 = inline10200
    var t5124 bool = t5121 <= t5123
    var jp5109 bool
    if t5124 {
        var t5125 string = value__230.input
        var t5126 *ref_int_x = value__230.index
        var t5127 int
        var inline10184 int = ref_get__Ref_3int(t5126)
        t5127 = inline10184
        var t5128 *ref_int_x = value__230.index
        var t5129 int
        var inline10182 int = ref_get__Ref_3int(t5128)
        t5129 = inline10182
        var t5130 int
        var inline10180 int = _goml_runtime_core_string_len(expected__231)
        t5130 = inline10180
        var t5131 int = t5129 + t5130
        var t5132 string
        var inline10178 string = string_byte_slice(t5125, t5127, t5131)
        t5132 = inline10178
        var t5133 bool = t5132 == expected__231
        jp5109 = t5133
    } else {
        jp5109 = false
    }
    if jp5109 {
        var t5110 *ref_int_x = value__230.index
        var t5111 *ref_int_x = value__230.index
        var t5112 int
        var inline10190 int = ref_get__Ref_3int(t5111)
        t5112 = inline10190
        var t5113 int
        var inline10188 int = _goml_runtime_core_string_len(expected__231)
        t5113 = inline10188
        var t5114 int = t5112 + t5113
        ref_set__Ref_3int(t5110, t5114)
        var t5115 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t5115
    } else {
        var t5116 string
        var inline10192 string = "invalid literal"
        var inline10193 string = "" + inline10192
        var inline10194 string = inline10193 + " at byte "
        var inline10195 *ref_int_x = value__230.index
        var inline10196 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10195)
        var inline10197 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10196)
        var inline10198 string = inline10194 + inline10197
        t5116 = inline10198
        var t5117 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5116,
        }
        return t5117
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5136 *ref_int_x = value__233.index
    var t5137 *ref_int_x = value__233.index
    var t5138 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5137)
    var t5139 int = t5138 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5136, t5139)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8833 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t5194 *ref_int_x = value__233.index
    var t5195 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5194)
    var t5196 string = value__233.input
    var t5197 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5196)
    var t5198 bool = t5195 < t5197
    var jp5187 bool
    if t5198 {
        var t5199 string = value__233.input
        var t5200 *ref_int_x = value__233.index
        var t5201 int
        var inline10208 int = ref_get__Ref_3int(t5200)
        t5201 = inline10208
        var t5202 uint8
        var inline10206 uint8 = _goml_runtime_core_string_byte_get(t5199, t5201)
        t5202 = inline10206
        var t5203 bool = t5202 == 93
        jp5187 = t5203
    } else {
        jp5187 = false
    }
    if jp5187 {
        var t5188 *ref_int_x = value__233.index
        var t5189 *ref_int_x = value__233.index
        var t5190 int
        var inline10212 int = ref_get__Ref_3int(t5189)
        t5190 = inline10212
        var t5191 int = t5190 + 1
        ref_set__Ref_3int(t5188, t5191)
        var t5192 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8833,
        }
        var t5193 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5192,
        }
        return t5193
    } else {
        Loop_loop5144:
        for {
            var t5145 *ref_int_x = value__233.index
            var t5146 int
            var inline10254 int = ref_get__Ref_3int(t5145)
            t5146 = inline10254
            var t5147 string = value__233.input
            var t5148 int
            var inline10252 int = _goml_runtime_core_string_len(t5147)
            t5148 = inline10252
            var t5149 bool = t5146 < t5148
            if t5149 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5151 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp5151 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(vec_literal__8833, jp5151)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5153 *ref_int_x = value__233.index
                    var t5154 int
                    var inline10248 int = ref_get__Ref_3int(t5153)
                    t5154 = inline10248
                    var t5155 string = value__233.input
                    var t5156 int
                    var inline10246 int = _goml_runtime_core_string_len(t5155)
                    t5156 = inline10246
                    var t5157 bool = t5154 >= t5156
                    if t5157 {
                        var t5158 string
                        var inline10214 string = "unterminated array"
                        var inline10215 string = "" + inline10214
                        var inline10216 string = inline10215 + " at byte "
                        var inline10217 *ref_int_x = value__233.index
                        var inline10218 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10217)
                        var inline10219 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10218)
                        var inline10220 string = inline10216 + inline10219
                        t5158 = inline10220
                        var t5159 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5158,
                        }
                        return t5159
                    } else {
                        var t5161 string = value__233.input
                        var t5162 *ref_int_x = value__233.index
                        var t5163 int
                        var inline10244 int = ref_get__Ref_3int(t5162)
                        t5163 = inline10244
                        var t5164 uint8
                        var inline10242 uint8 = _goml_runtime_core_string_byte_get(t5161, t5163)
                        t5164 = inline10242
                        var t5165 bool = t5164 == 93
                        if t5165 {
                            var t5166 *ref_int_x = value__233.index
                            var t5167 *ref_int_x = value__233.index
                            var t5168 int
                            var inline10224 int = ref_get__Ref_3int(t5167)
                            t5168 = inline10224
                            var t5169 int = t5168 + 1
                            ref_set__Ref_3int(t5166, t5169)
                            var t5170 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8833,
                            }
                            var t5171 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t5170,
                            }
                            return t5171
                        } else {
                            var t5173 string = value__233.input
                            var t5174 *ref_int_x = value__233.index
                            var t5175 int
                            var inline10240 int = ref_get__Ref_3int(t5174)
                            t5175 = inline10240
                            var t5176 uint8
                            var inline10238 uint8 = _goml_runtime_core_string_byte_get(t5173, t5175)
                            t5176 = inline10238
                            var t5177 bool = t5176 == 44
                            if t5177 {
                                var t5178 *ref_int_x = value__233.index
                                var t5179 *ref_int_x = value__233.index
                                var t5180 int
                                var inline10228 int = ref_get__Ref_3int(t5179)
                                t5180 = inline10228
                                var t5181 int = t5180 + 1
                                ref_set__Ref_3int(t5178, t5181)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5183 string
                                var inline10230 string = "expected array separator"
                                var inline10231 string = "" + inline10230
                                var inline10232 string = inline10231 + " at byte "
                                var inline10233 *ref_int_x = value__233.index
                                var inline10234 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10233)
                                var inline10235 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10234)
                                var inline10236 string = inline10232 + inline10235
                                t5183 = inline10236
                                var t5184 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5183,
                                }
                                return t5184
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t5185 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t5185
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5144
            }
        }
        var t5142 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5143 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5142,
        }
        return t5143
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5206 *ref_int_x = value__236.index
    var t5207 *ref_int_x = value__236.index
    var t5208 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5207)
    var t5209 int = t5208 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5206, t5209)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10035 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5288 *ref_int_x = value__236.index
    var t5289 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5288)
    var t5290 string = value__236.input
    var t5291 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5290)
    var t5292 bool = t5289 < t5291
    var jp5281 bool
    if t5292 {
        var t5293 string = value__236.input
        var t5294 *ref_int_x = value__236.index
        var t5295 int
        var inline10258 int = ref_get__Ref_3int(t5294)
        t5295 = inline10258
        var t5296 uint8
        var inline10256 uint8 = _goml_runtime_core_string_byte_get(t5293, t5295)
        t5296 = inline10256
        var t5297 bool = t5296 == 125
        jp5281 = t5297
    } else {
        jp5281 = false
    }
    if jp5281 {
        var t5282 *ref_int_x = value__236.index
        var t5283 *ref_int_x = value__236.index
        var t5284 int
        var inline10262 int = ref_get__Ref_3int(t5283)
        t5284 = inline10262
        var t5285 int = t5284 + 1
        ref_set__Ref_3int(t5282, t5285)
        var t5286 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10035,
        }
        var t5287 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5286,
        }
        return t5287
    } else {
        Loop_loop5214:
        for {
            var t5215 *ref_int_x = value__236.index
            var t5216 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5215)
            var t5217 string = value__236.input
            var t5218 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5217)
            var t5219 bool = t5216 < t5218
            if t5219 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5221 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5221 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5269 *ref_int_x = value__236.index
                    var t5270 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5269)
                    var t5271 string = value__236.input
                    var t5272 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5271)
                    var t5273 bool = t5270 >= t5272
                    var jp5261 bool
                    if t5273 {
                        jp5261 = true
                    } else {
                        var t5274 string = value__236.input
                        var t5275 *ref_int_x = value__236.index
                        var t5276 int
                        var inline10266 int = ref_get__Ref_3int(t5275)
                        t5276 = inline10266
                        var t5277 uint8
                        var inline10264 uint8 = _goml_runtime_core_string_byte_get(t5274, t5276)
                        t5277 = inline10264
                        var t5278 bool = t5277 != 58
                        jp5261 = t5278
                    }
                    if jp5261 {
                        var t5262 string
                        var inline10268 string = "expected object colon"
                        var inline10269 string = "" + inline10268
                        var inline10270 string = inline10269 + " at byte "
                        var inline10271 *ref_int_x = value__236.index
                        var inline10272 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10271)
                        var inline10273 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10272)
                        var inline10274 string = inline10270 + inline10273
                        t5262 = inline10274
                        var t5263 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5262,
                        }
                        return t5263
                    } else {
                        var t5264 *ref_int_x = value__236.index
                        var t5265 *ref_int_x = value__236.index
                        var t5266 int
                        var inline10278 int = ref_get__Ref_3int(t5265)
                        t5266 = inline10278
                        var t5267 int = t5266 + 1
                        ref_set__Ref_3int(t5264, t5267)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5224 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5224 = x816
                            var t5225 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5221,
                                _1: jp5224,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10035, t5225)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5227 *ref_int_x = value__236.index
                            var t5228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5227)
                            var t5229 string = value__236.input
                            var t5230 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5229)
                            var t5231 bool = t5228 >= t5230
                            if t5231 {
                                var t5232 string
                                var inline10280 string = "unterminated object"
                                var inline10281 string = "" + inline10280
                                var inline10282 string = inline10281 + " at byte "
                                var inline10283 *ref_int_x = value__236.index
                                var inline10284 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10283)
                                var inline10285 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10284)
                                var inline10286 string = inline10282 + inline10285
                                t5232 = inline10286
                                var t5233 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5232,
                                }
                                return t5233
                            } else {
                                var t5235 string = value__236.input
                                var t5236 *ref_int_x = value__236.index
                                var t5237 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5236)
                                var t5238 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5235, t5237)
                                var t5239 bool = t5238 == 125
                                if t5239 {
                                    var t5240 *ref_int_x = value__236.index
                                    var t5241 *ref_int_x = value__236.index
                                    var t5242 int
                                    var inline10290 int = ref_get__Ref_3int(t5241)
                                    t5242 = inline10290
                                    var t5243 int = t5242 + 1
                                    ref_set__Ref_3int(t5240, t5243)
                                    var t5244 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10035,
                                    }
                                    var t5245 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5244,
                                    }
                                    return t5245
                                } else {
                                    var t5247 string = value__236.input
                                    var t5248 *ref_int_x = value__236.index
                                    var t5249 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5248)
                                    var t5250 uint8
                                    var inline10304 uint8 = _goml_runtime_core_string_byte_get(t5247, t5249)
                                    t5250 = inline10304
                                    var t5251 bool = t5250 == 44
                                    if t5251 {
                                        var t5252 *ref_int_x = value__236.index
                                        var t5253 *ref_int_x = value__236.index
                                        var t5254 int
                                        var inline10294 int = ref_get__Ref_3int(t5253)
                                        t5254 = inline10294
                                        var t5255 int = t5254 + 1
                                        ref_set__Ref_3int(t5252, t5255)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5257 string
                                        var inline10296 string = "expected object separator"
                                        var inline10297 string = "" + inline10296
                                        var inline10298 string = inline10297 + " at byte "
                                        var inline10299 *ref_int_x = value__236.index
                                        var inline10300 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10299)
                                        var inline10301 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10300)
                                        var inline10302 string = inline10298 + inline10301
                                        t5257 = inline10302
                                        var t5258 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5257,
                                        }
                                        return t5258
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5259 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5259
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5279 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5279
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5214
            }
        }
        var t5212 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5213 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5212,
        }
        return t5213
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5302 *ref_int_x = value__240.index
    var t5303 int
    var inline10342 int = ref_get__Ref_3int(t5302)
    t5303 = inline10342
    var t5304 string = value__240.input
    var t5305 int
    var inline10340 int = _goml_runtime_core_string_len(t5304)
    t5305 = inline10340
    var t5306 bool = t5303 >= t5305
    if t5306 {
        var t5307 string
        var inline10306 string = "expected JSON value"
        var inline10307 string = "" + inline10306
        var inline10308 string = inline10307 + " at byte "
        var inline10309 *ref_int_x = value__240.index
        var inline10310 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10309)
        var inline10311 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10310)
        var inline10312 string = inline10308 + inline10311
        t5307 = inline10312
        var t5308 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5307,
        }
        return t5308
    } else {
        var t5309 string = value__240.input
        var t5310 *ref_int_x = value__240.index
        var t5311 int
        var inline10338 int = ref_get__Ref_3int(t5310)
        t5311 = inline10338
        var mtmp824 uint8
        var inline10336 uint8 = _goml_runtime_core_string_byte_get(t5309, t5311)
        mtmp824 = inline10336
        switch mtmp824 {
        case 123:
            var t5314 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5314
        case 91:
            var t5315 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5315
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5318 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5319 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5318,
                }
                return t5319
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5320 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5320
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5321 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5322 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5321)
            return t5322
        case 102:
            var t5323 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5324 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5323)
            return t5324
        case 110:
            var t5325 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5325
        default:
            var t5333 bool = mtmp824 == 45
            var jp5329 bool
            if t5333 {
                jp5329 = true
            } else {
                var inline10314 bool = mtmp824 >= 48
                if inline10314 {
                    var inline10315 bool = mtmp824 <= 57
                    jp5329 = inline10315
                } else {
                    jp5329 = false
                }
            }
            if jp5329 {
                var inline10317 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10319 string
                switch inline10317.(type) {
                case Result__string__string_Ok:
                    var inline10322 string = inline10317.(Result__string__string_Ok)._0
                    inline10319 = inline10322
                    var inline10320 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10319,
                    }
                    var inline10321 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10320,
                    }
                    return inline10321
                case Result__string__string_Err:
                    var inline10324 string = inline10317.(Result__string__string_Err)._0
                    var inline10326 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10324,
                    }
                    return inline10326
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5331 string
                var inline10328 string = "unexpected JSON token"
                var inline10329 string = "" + inline10328
                var inline10330 string = inline10329 + " at byte "
                var inline10331 *ref_int_x = value__240.index
                var inline10332 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10331)
                var inline10333 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10332)
                var inline10334 string = inline10330 + inline10333
                t5331 = inline10334
                var t5332 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5331,
                }
                return t5332
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10356 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10357 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10356,
    }
    parser__245 = inline10357
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5338 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5338 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5341 *ref_int_x = parser__245.index
        var t5342 int
        var inline10354 int = ref_get__Ref_3int(t5341)
        t5342 = inline10354
        var t5343 int
        var inline10352 int = _goml_runtime_core_string_len(input__244)
        t5343 = inline10352
        var t5344 bool = t5342 == t5343
        if t5344 {
            var t5345 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5338,
            }
            return t5345
        } else {
            var t5346 string
            var inline10344 string = "trailing JSON data"
            var inline10345 string = "" + inline10344
            var inline10346 string = inline10345 + " at byte "
            var inline10347 *ref_int_x = parser__245.index
            var inline10348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10347)
            var inline10349 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10348)
            var inline10350 string = inline10346 + inline10349
            t5346 = inline10350
            var t5347 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5346,
            }
            return t5347
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5348 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5348
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10390 rune = 34
    var inline10391 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10390)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10391)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10388 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10388
    Loop_loop5362:
    for {
        var t5363 bool = for_index833 < for_limit834
        if t5363 {
            var for_item835 int = for_index833
            var t5364 int = for_index833 + 1
            for_index833 = t5364
            var byte__252 uint8
            var inline10376 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10376
            var t5417 bool = byte__252 == 34
            var jp5415 bool
            if t5417 {
                jp5415 = true
            } else {
                var t5418 bool = byte__252 == 92
                jp5415 = t5418
            }
            var jp5412 bool
            if jp5415 {
                jp5412 = true
            } else {
                var t5416 bool = byte__252 == 8
                jp5412 = t5416
            }
            var jp5409 bool
            if jp5412 {
                jp5409 = true
            } else {
                var t5413 bool = byte__252 == 9
                jp5409 = t5413
            }
            var jp5406 bool
            if jp5409 {
                jp5406 = true
            } else {
                var t5410 bool = byte__252 == 10
                jp5406 = t5410
            }
            var jp5403 bool
            if jp5406 {
                jp5403 = true
            } else {
                var t5407 bool = byte__252 == 12
                jp5403 = t5407
            }
            var jp5400 bool
            if jp5403 {
                jp5400 = true
            } else {
                var t5404 bool = byte__252 == 13
                jp5400 = t5404
            }
            var jp5367 bool
            if jp5400 {
                jp5367 = true
            } else {
                var t5401 bool = byte__252 < 32
                jp5367 = t5401
            }
            if jp5367 {
                var t5396 bool = start__250 < for_item835
                if t5396 {
                    var t5397 string
                    var inline10362 string = string_byte_slice(value__249, start__250, for_item835)
                    t5397 = inline10362
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5397)
                } else {}
                var t5371 bool = byte__252 == 34
                if t5371 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5374 bool = byte__252 == 92
                    if t5374 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5377 bool = byte__252 == 8
                        if t5377 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5380 bool = byte__252 == 9
                            if t5380 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5383 bool = byte__252 == 10
                                if t5383 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5386 bool = byte__252 == 12
                                    if t5386 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5389 bool = byte__252 == 13
                                        if t5389 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5391 uint8 = byte__252 / 16
                                            var t5392 rune
                                            var inline10373 int = int(uint8(t5391))
                                            var inline10374 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10373)
                                            t5392 = inline10374
                                            var inline10370 string = _goml_m_inherent_i_char_i_char_i_to__string(t5392)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10370)
                                            var t5393_rhs uint8 = 16
                                            var t5393 uint8 = byte__252 % t5393_rhs
                                            var t5394 rune
                                            var inline10367 int = int(uint8(t5393))
                                            var inline10368 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10367)
                                            t5394 = inline10368
                                            var inline10364 string = _goml_m_inherent_i_char_i_char_i_to__string(t5394)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10364)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5370 int = for_item835 + 1
                start__250 = t5370
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5362
        }
    }
    var t5357 int
    var inline10386 int = _goml_runtime_core_string_len(value__249)
    t5357 = inline10386
    var t5358 bool = start__250 < t5357
    if t5358 {
        var t5359 int
        var inline10380 int = _goml_runtime_core_string_len(value__249)
        t5359 = inline10380
        var t5360 string
        var inline10378 string = string_byte_slice(value__249, start__250, t5359)
        t5360 = inline10378
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5360)
    } else {}
    var inline10382 rune = 34
    var inline10383 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10382)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10383)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10406 rune = 123
        var inline10407 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10406)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10407)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5423:
        for {
            var t5424 bool = for_index852 < for_limit851
            if t5424 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5425 int = for_index852 + 1
                for_index852 = t5425
                var t5431 bool = index__256 > 0
                if t5431 {
                    var inline10394 rune = 44
                    var inline10395 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10394)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10395)
                } else {}
                var t5427 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5427)
                var inline10398 rune = 58
                var inline10399 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10398)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10399)
                var t5428 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5428)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5429 int = compound_old859 + compound_value860
                index__256 = t5429
                continue
            } else {
                break Loop_loop5423
            }
        }
        var inline10402 rune = 125
        var inline10403 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10402)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10403)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10418 rune = 91
        var inline10419 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10418)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10419)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5435:
        for {
            var t5436 bool = for_index866 < for_limit865
            if t5436 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5437 int = for_index866 + 1
                for_index866 = t5437
                var t5441 bool = index__259 > 0
                if t5441 {
                    var inline10410 rune = 44
                    var inline10411 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10410)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10411)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5439 int = compound_old871 + compound_value872
                index__259 = t5439
                continue
            } else {
                break Loop_loop5435
            }
        }
        var inline10414 rune = 93
        var inline10415 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10414)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10415)
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
        var jp5446 string
        if x848 {
            jp5446 = "true"
        } else {
            jp5446 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5446)
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
    var inline10427 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10428 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10427,
    }
    builder__265 = inline10428
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10422 *_goml_vec_uint8 = builder__265.values
    var inline10423 Tuple2_4bool_6string = string_from_utf8(inline10422)
    var inline10424 string = inline10423._1
    return inline10424
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5457:
        for {
            var t5458 bool = for_index883 < for_limit882
            if t5458 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5459 int = for_index883 + 1
                for_index883 = t5459
                var t5461 string = for_item884._0
                var t5462 bool = t5461 == name__267
                if t5462 {
                    var t5463 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5464 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5463,
                    }
                    return t5464
                } else {
                    continue
                }
            } else {
                break Loop_loop5457
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5474 int
    var inline10439 int = _goml_runtime_core_string_len(value__272)
    t5474 = inline10439
    var t5475 bool = t5474 == 0
    if t5475 {
        return Option__int_None{}
    } else {
        var t5476 uint8
        var inline10436 int = 0
        var inline10437 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10436)
        t5476 = inline10437
        var negative__273 bool = t5476 == 45
        var jp5478 int
        if negative__273 {
            jp5478 = 1
        } else {
            jp5478 = 0
        }
        var index__274 int = jp5478
        var result__275 int = 0
        var t5499 int
        var inline10434 int = _goml_runtime_core_string_len(value__272)
        t5499 = inline10434
        var t5500 bool = index__274 == t5499
        if t5500 {
            return Option__int_None{}
        } else {
            Loop_loop5485:
            for {
                var t5486 int
                var inline10432 int = _goml_runtime_core_string_len(value__272)
                t5486 = inline10432
                var t5487 bool = index__274 < t5486
                if t5487 {
                    var byte__276 uint8
                    var inline10430 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10430
                    var t5497 bool = byte__276 < 48
                    var jp5492 bool
                    if t5497 {
                        jp5492 = true
                    } else {
                        var t5498 bool = byte__276 > 57
                        jp5492 = t5498
                    }
                    if jp5492 {
                        return Option__int_None{}
                    } else {
                        var t5493 int = result__275 * 10
                        var t5494 uint8 = byte__276 - 48
                        var t5495 int = int(uint8(t5494))
                        var t5496 int = t5493 + t5495
                        result__275 = t5496
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5489 int = compound_old895 + compound_value896
                        index__274 = t5489
                        continue
                    }
                } else {
                    break Loop_loop5485
                }
            }
            var jp5482 int
            if negative__273 {
                var t5484 int = 0 - result__275
                jp5482 = t5484
            } else {
                jp5482 = result__275
            }
            var t5483 Option__int = Option__int_Some{
                _0: jp5482,
            }
            return t5483
        }
    }
}

func main0() struct{} {
    var mtmp182 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6272 _goml_m_std_p_json_p_Value
    switch mtmp182.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x183 _goml_m_std_p_json_p_Value = mtmp182.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6272 = x183
        var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "name")
        switch mtmp186.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline10886 string = "missing name"
            var inline10887 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10886)
            _goml_runtime_core_string_println(inline10887)
            var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "version")
            switch mtmp191.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10901 string = "missing version"
                var inline10902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10901)
                _goml_runtime_core_string_println(inline10902)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp193 Option__int
                switch x192.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline10912 string = x192.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline10914 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10912)
                    mtmp193 = inline10914
                default:
                    mtmp193 = Option__int_None{}
                }
                switch mtmp193.(type) {
                case Option__int_None:
                    var inline10905 string = "invalid version"
                    var inline10906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10905)
                    _goml_runtime_core_string_println(inline10906)
                case Option__int_Some:
                    var x194 int = mtmp193.(Option__int_Some)._0
                    var inline10909 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x194)
                    _goml_runtime_core_string_println(inline10909)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp196 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "stable")
            switch mtmp196.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10916 string = "missing stable"
                var inline10917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10916)
                _goml_runtime_core_string_println(inline10917)
                var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                println__T_string(t6276)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x197 _goml_m_std_p_json_p_Value = mtmp196.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11188 bool
                switch x197.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline10927 bool = x197.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11188 = inline10927
                    var inline10924 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11188)
                    _goml_runtime_core_string_println(inline10924)
                    var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                    println__T_string(t6276)
                    return struct{}{}
                default:
                    var inline10920 string = "invalid stable"
                    var inline10921 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10920)
                    _goml_runtime_core_string_println(inline10921)
                    var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                    println__T_string(t6276)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11194 string
            switch x187.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline10897 string = x187.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11194 = inline10897
                var inline10894 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11194)
                _goml_runtime_core_string_println(inline10894)
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "version")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10901 string = "missing version"
                    var inline10902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10901)
                    _goml_runtime_core_string_println(inline10902)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp193 Option__int
                    switch x192.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10912 string = x192.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10914 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10912)
                        mtmp193 = inline10914
                    default:
                        mtmp193 = Option__int_None{}
                    }
                    switch mtmp193.(type) {
                    case Option__int_None:
                        var inline10905 string = "invalid version"
                        var inline10906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10905)
                        _goml_runtime_core_string_println(inline10906)
                    case Option__int_Some:
                        var x194 int = mtmp193.(Option__int_Some)._0
                        var inline10909 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x194)
                        _goml_runtime_core_string_println(inline10909)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp196 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "stable")
                switch mtmp196.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10916 string = "missing stable"
                    var inline10917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10916)
                    _goml_runtime_core_string_println(inline10917)
                    var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                    println__T_string(t6276)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x197 _goml_m_std_p_json_p_Value = mtmp196.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11188 bool
                    switch x197.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10927 bool = x197.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11188 = inline10927
                        var inline10924 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11188)
                        _goml_runtime_core_string_println(inline10924)
                        var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                        println__T_string(t6276)
                        return struct{}{}
                    default:
                        var inline10920 string = "invalid stable"
                        var inline10921 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10920)
                        _goml_runtime_core_string_println(inline10921)
                        var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                        println__T_string(t6276)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline10890 string = "invalid name"
                var inline10891 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10890)
                _goml_runtime_core_string_println(inline10891)
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "version")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10901 string = "missing version"
                    var inline10902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10901)
                    _goml_runtime_core_string_println(inline10902)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp193 Option__int
                    switch x192.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10912 string = x192.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10914 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10912)
                        mtmp193 = inline10914
                    default:
                        mtmp193 = Option__int_None{}
                    }
                    switch mtmp193.(type) {
                    case Option__int_None:
                        var inline10905 string = "invalid version"
                        var inline10906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10905)
                        _goml_runtime_core_string_println(inline10906)
                    case Option__int_Some:
                        var x194 int = mtmp193.(Option__int_Some)._0
                        var inline10909 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x194)
                        _goml_runtime_core_string_println(inline10909)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp196 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6272, "stable")
                switch mtmp196.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10916 string = "missing stable"
                    var inline10917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10916)
                    _goml_runtime_core_string_println(inline10917)
                    var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                    println__T_string(t6276)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x197 _goml_m_std_p_json_p_Value = mtmp196.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11188 bool
                    switch x197.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10927 bool = x197.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11188 = inline10927
                        var inline10924 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11188)
                        _goml_runtime_core_string_println(inline10924)
                        var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                        println__T_string(t6276)
                        return struct{}{}
                    default:
                        var inline10920 string = "invalid stable"
                        var inline10921 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10920)
                        _goml_runtime_core_string_println(inline10921)
                        var t6276 string = _goml_m_std_p_json_p_encode(jp6272)
                        println__T_string(t6276)
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
        var x184 string = mtmp182.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline10883 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x184)
        _goml_runtime_core_string_println(inline10883)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6292 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6292
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6332:
    for {
        var t6333 int
        var inline10938 int = _goml_runtime_core_string_len(x12)
        t6333 = inline10938
        var t6334 bool = index__26 < t6333
        if t6334 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6336 int = compound_old17 + x16
                index__26 = t6336
                continue
            } else {
                var t6338 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6338
            }
        } else {
            break Loop_loop6332
        }
    }
    var t6331 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6331
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t6368 string = _goml_runtime_core_int_to_string(self__32)
    return t6368
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline10948 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline10949 bool = inline10948._0
    var inline10950 rune = inline10948._1
    if inline10949 {
        return inline10950
    } else {
        var inline10953 rune = _goml_runtime_core_string_get("", -1)
        return inline10953
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t6453 *ref_int_x = ref__Ref_3int(value__270)
    return t6453
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__271 *ref_int_x) int {
    var t6456 int = ref_get__Ref_3int(self__271)
    return t6456
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__272 *ref_int_x, value__273 int) struct{} {
    ref_set__Ref_3int(self__272, value__273)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__34 rune) string {
    var inline10960 uint32 = uint32(rune(self__34))
    var inline10961 bool = utf8_valid_scalar(inline10960)
    if inline10961 {
        var inline10962 string = _goml_runtime_core_char_to_string(self__34)
        return inline10962
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t6521 int = _goml_runtime_core_string_len(self__36)
    return t6521
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t6524 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t6524
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline10965 bool = string_is_char_boundary(self__41, start__42)
    var inline10967 bool
    if inline10965 {
        var inline10970 bool = string_is_char_boundary(self__41, end__43)
        inline10967 = inline10970
    } else {
        inline10967 = false
    }
    if inline10967 {
        var inline10968 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline10968
    } else {
        var inline10969 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline10969
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline11012 bool = utf8_valid_scalar(value__2)
    if inline11012 {
        var inline11013 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11014 rune = inline11013._1
        var inline11016 Option__char = Option__char_Some{
            _0: inline11014,
        }
        return inline11016
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6633 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6633
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6638 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6638
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__174 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__175 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__174, elem__175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6674 string
    t6674 = value__1
    _goml_runtime_core_string_println(t6674)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6800 bool = index__6 < 0
    var jp6798 bool
    if t6800 {
        jp6798 = true
    } else {
        var t6801 bool = index__6 >= length__7
        jp6798 = t6801
    }
    if jp6798 {
        var inline11027 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11027
    } else {
        var t6685 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6685))
        var t6688 bool = first__8 < 128
        if t6688 {
            var inline11029 int = 1
            var inline11030 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11030.(type) {
            case Option__char_None:
                var inline11031 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11031
            case Option__char_Some:
                var inline11032 rune = inline11030.(Option__char_Some)._0
                var inline11034 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11032,
                    _2: inline11029,
                }
                return inline11034
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6692 bool = first__8 < 194
            if t6692 {
                var inline11036 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11036
            } else {
                var t6696 bool = first__8 < 224
                if t6696 {
                    var t6709 int = length__7 - index__6
                    var t6710 bool = t6709 < 2
                    if t6710 {
                        var inline11038 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11038
                    } else {
                        var t6698 int = index__6 + 1
                        var t6699 uint8
                        var inline11052 uint8 = _goml_runtime_core_string_byte_get(value__5, t6698)
                        t6699 = inline11052
                        var second__9 uint32 = uint32(uint8(t6699))
                        var t6702 bool
                        var inline11049 bool = second__9 < 128
                        if inline11049 {
                            t6702 = true
                        } else {
                            var inline11050 bool = second__9 > 191
                            t6702 = inline11050
                        }
                        if t6702 {
                            var inline11040 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11040
                        } else {
                            var t6704_rhs uint32 = 31
                            var t6704 uint32 = first__8 & t6704_rhs
                            var t6705_rhs int = 6
                            var t6705 uint32 = t6704 << t6705_rhs
                            var t6706_rhs uint32 = 63
                            var t6706 uint32 = second__9 & t6706_rhs
                            var t6707 uint32 = t6705 | t6706
                            var inline11042 int = 2
                            var inline11043 Option__char = __goml_builtin_char_from_uint32(t6707)
                            switch inline11043.(type) {
                            case Option__char_None:
                                var inline11044 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11044
                            case Option__char_Some:
                                var inline11045 rune = inline11043.(Option__char_Some)._0
                                var inline11047 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11045,
                                    _2: inline11042,
                                }
                                return inline11047
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6714 bool = first__8 < 240
                    if t6714 {
                        var t6747 int = length__7 - index__6
                        var t6748 bool = t6747 < 3
                        if t6748 {
                            var inline11054 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11054
                        } else {
                            var t6716 int = index__6 + 1
                            var t6717 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6716)
                            var second__10 uint32 = uint32(uint8(t6717))
                            var t6718 int = index__6 + 2
                            var t6719 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6718)
                            var third__11 uint32 = uint32(uint8(t6719))
                            var t6745 bool = utf8_invalid_continuation(second__10)
                            var jp6740 bool
                            if t6745 {
                                jp6740 = true
                            } else {
                                var inline11056 bool = third__11 < 128
                                if inline11056 {
                                    jp6740 = true
                                } else {
                                    var inline11057 bool = third__11 > 191
                                    jp6740 = inline11057
                                }
                            }
                            var jp6734 bool
                            if jp6740 {
                                jp6734 = true
                            } else {
                                var t6743 bool = first__8 == 224
                                if t6743 {
                                    var t6744 bool = second__10 < 160
                                    jp6734 = t6744
                                } else {
                                    jp6734 = false
                                }
                            }
                            var jp6723 bool
                            if jp6734 {
                                jp6723 = true
                            } else {
                                var t6737 bool = first__8 == 237
                                if t6737 {
                                    var t6738 bool = second__10 >= 160
                                    jp6723 = t6738
                                } else {
                                    jp6723 = false
                                }
                            }
                            if jp6723 {
                                var inline11059 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11059
                            } else {
                                var t6725_rhs uint32 = 15
                                var t6725 uint32 = first__8 & t6725_rhs
                                var t6726_rhs int = 12
                                var t6726 uint32 = t6725 << t6726_rhs
                                var t6727_rhs uint32 = 63
                                var t6727 uint32 = second__10 & t6727_rhs
                                var t6728_rhs int = 6
                                var t6728 uint32 = t6727 << t6728_rhs
                                var t6729 uint32 = t6726 | t6728
                                var t6730_rhs uint32 = 63
                                var t6730 uint32 = third__11 & t6730_rhs
                                var t6731 uint32 = t6729 | t6730
                                var inline11061 int = 3
                                var inline11062 Option__char = __goml_builtin_char_from_uint32(t6731)
                                switch inline11062.(type) {
                                case Option__char_None:
                                    var inline11063 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11063
                                case Option__char_Some:
                                    var inline11064 rune = inline11062.(Option__char_Some)._0
                                    var inline11066 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11064,
                                        _2: inline11061,
                                    }
                                    return inline11066
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6752 bool = first__8 < 245
                        if t6752 {
                            var t6793 int = length__7 - index__6
                            var t6794 bool = t6793 < 4
                            if t6794 {
                                var t6795 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6795
                            } else {
                                var t6754 int = index__6 + 1
                                var t6755 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6754)
                                var second__12 uint32 = uint32(uint8(t6755))
                                var t6756 int = index__6 + 2
                                var t6757 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6756)
                                var third__13 uint32 = uint32(uint8(t6757))
                                var t6758 int = index__6 + 3
                                var t6759 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6758)
                                var fourth__14 uint32 = uint32(uint8(t6759))
                                var t6791 bool = utf8_invalid_continuation(second__12)
                                var jp6789 bool
                                if t6791 {
                                    jp6789 = true
                                } else {
                                    var t6792 bool = utf8_invalid_continuation(third__13)
                                    jp6789 = t6792
                                }
                                var jp6783 bool
                                if jp6789 {
                                    jp6783 = true
                                } else {
                                    var t6790 bool = utf8_invalid_continuation(fourth__14)
                                    jp6783 = t6790
                                }
                                var jp6777 bool
                                if jp6783 {
                                    jp6777 = true
                                } else {
                                    var t6786 bool = first__8 == 240
                                    if t6786 {
                                        var t6787 bool = second__12 < 144
                                        jp6777 = t6787
                                    } else {
                                        jp6777 = false
                                    }
                                }
                                var jp6763 bool
                                if jp6777 {
                                    jp6763 = true
                                } else {
                                    var t6780 bool = first__8 == 244
                                    if t6780 {
                                        var t6781 bool = second__12 > 143
                                        jp6763 = t6781
                                    } else {
                                        jp6763 = false
                                    }
                                }
                                if jp6763 {
                                    var t6764 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6764
                                } else {
                                    var t6765_rhs uint32 = 7
                                    var t6765 uint32 = first__8 & t6765_rhs
                                    var t6766_rhs int = 18
                                    var t6766 uint32 = t6765 << t6766_rhs
                                    var t6767_rhs uint32 = 63
                                    var t6767 uint32 = second__12 & t6767_rhs
                                    var t6768_rhs int = 12
                                    var t6768 uint32 = t6767 << t6768_rhs
                                    var t6769 uint32 = t6766 | t6768
                                    var t6770_rhs uint32 = 63
                                    var t6770 uint32 = third__13 & t6770_rhs
                                    var t6771_rhs int = 6
                                    var t6771 uint32 = t6770 << t6771_rhs
                                    var t6772 uint32 = t6769 | t6771
                                    var t6773_rhs uint32 = 63
                                    var t6773 uint32 = fourth__14 & t6773_rhs
                                    var t6774 uint32 = t6772 | t6773
                                    var t6775 Tuple3_4bool_4char_3int = utf8_valid_decode(t6774, 4)
                                    return t6775
                                }
                            }
                        } else {
                            var t6796 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6796
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6821 uint32 = uint32(rune(value__29))
    var t6822 bool
    var inline11068 bool = t6821 <= 1114111
    if inline11068 {
        var inline11069 bool = t6821 >= 55296
        var inline11071 bool
        if inline11069 {
            var inline11073 bool = t6821 <= 57343
            inline11071 = inline11073
        } else {
            inline11071 = false
        }
        var inline11072 bool = !inline11071
        t6822 = inline11072
    } else {
        t6822 = false
    }
    if t6822 {
        var t6823 string = _goml_runtime_core_char_to_string(value__29)
        return t6823
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6837 bool = index__16 < 0
    var jp6829 bool
    if t6837 {
        jp6829 = true
    } else {
        var t6838 int
        var inline11075 int = _goml_runtime_core_string_len(value__15)
        t6838 = inline11075
        var t6839 bool = index__16 > t6838
        jp6829 = t6839
    }
    if jp6829 {
        return false
    } else {
        var t6832 int
        var inline11079 int = _goml_runtime_core_string_len(value__15)
        t6832 = inline11079
        var t6833 bool = index__16 == t6832
        if t6833 {
            return true
        } else {
            var t6834 uint8
            var inline11077 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6834 = inline11077
            var t6835_rhs uint8 = 192
            var t6835 uint8 = t6834 & t6835_rhs
            var t6836 bool = t6835 != 128
            return t6836
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6848 bool = string_is_char_boundary(value__21, start__22)
    var jp6845 bool
    if t6848 {
        var t6849 bool = string_is_char_boundary(value__21, end__23)
        jp6845 = t6849
    } else {
        jp6845 = false
    }
    if jp6845 {
        var t6846 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6846
    } else {
        var t6847 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6847
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t6875 bool
    var inline11083 bool = value__30 <= 1114111
    if inline11083 {
        var inline11084 bool = value__30 >= 55296
        var inline11086 bool
        if inline11084 {
            var inline11088 bool = value__30 <= 57343
            inline11086 = inline11088
        } else {
            inline11086 = false
        }
        var inline11087 bool = !inline11086
        t6875 = inline11087
    } else {
        t6875 = false
    }
    if t6875 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t6876 Option__char = Option__char_Some{
            _0: x24,
        }
        return t6876
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t6883 string = _goml_runtime_core_int_to_string(self__67)
    return t6883
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t6886 string = _goml_runtime_core_bool_to_string(self__64)
    return t6886
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6889 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6889
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11200 rune
    var inline11092 bool = utf8_valid_scalar(value__0)
    if inline11092 {
        var inline11093 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11094 rune = inline11093._1
        commute_field11200 = inline11094
        var t6895 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11200,
            _2: width__1,
        }
        return t6895
    } else {
        var inline11090 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11090
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6900 bool = value__3 < 128
    if t6900 {
        return true
    } else {
        var t6901 bool = value__3 > 191
        return t6901
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6906 bool = value__4 <= 1114111
    if t6906 {
        var t6910 bool = value__4 >= 55296
        var jp6908 bool
        if t6910 {
            var t6911 bool = value__4 <= 57343
            jp6908 = t6911
        } else {
            jp6908 = false
        }
        var t6909 bool = !jp6908
        return t6909
    } else {
        return false
    }
}

func main() {
    main0()
}
