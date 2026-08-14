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
    var vec_literal__178 *_goml_vec_uint8
    var inline8747 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline8747
    var t2897 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2897
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8762 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8762
    var t2911 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2911, length__5)
    var for_index1 int = 0
    Loop_loop2913:
    for {
        var t2914 bool = for_index1 < length__5
        if t2914 {
            var for_item3 int = for_index1
            var t2915 int = for_index1 + 1
            for_index1 = t2915
            var t2916 *_goml_vec_uint8 = self__3.values
            var t2917 uint8
            var inline8758 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2917 = inline8758
            vec_push__Vec_5uint8(t2916, t2917)
            continue
        } else {
            break Loop_loop2913
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2920 string
    var inline8764 string = char_to_string(value__8)
    t2920 = inline8764
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2920)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4626 string = "" + message__201
    var t4627 string = t4626 + " at byte "
    var t4628 *ref_int_x = value__200.index
    var t4629 int
    var inline10103 int = ref_get__Ref_3int(t4628)
    t4629 = inline10103
    var t4630 string
    var inline10101 string = _goml_runtime_core_int_to_string(t4629)
    t4630 = inline10101
    var t4631 string = t4627 + t4630
    return t4631
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4646:
    for {
        var t4654 *ref_int_x = value__203.index
        var t4655 int
        var inline10124 int = ref_get__Ref_3int(t4654)
        t4655 = inline10124
        var t4656 string = value__203.input
        var t4657 int
        var inline10122 int = _goml_runtime_core_string_len(t4656)
        t4657 = inline10122
        var t4658 bool = t4655 < t4657
        var jp4648 bool
        if t4658 {
            var t4659 string = value__203.input
            var t4660 *ref_int_x = value__203.index
            var t4661 int
            var inline10116 int = ref_get__Ref_3int(t4660)
            t4661 = inline10116
            var t4662 uint8
            var inline10114 uint8 = _goml_runtime_core_string_byte_get(t4659, t4661)
            t4662 = inline10114
            var inline10105 bool = t4662 == 9
            var inline10107 bool
            if inline10105 {
                inline10107 = true
            } else {
                var inline10112 bool = t4662 == 10
                inline10107 = inline10112
            }
            var inline10109 bool
            if inline10107 {
                inline10109 = true
            } else {
                var inline10111 bool = t4662 == 13
                inline10109 = inline10111
            }
            if inline10109 {
                jp4648 = true
            } else {
                var inline10110 bool = t4662 == 32
                jp4648 = inline10110
            }
        } else {
            jp4648 = false
        }
        if jp4648 {
            var t4649 *ref_int_x = value__203.index
            var t4650 *ref_int_x = value__203.index
            var t4651 int
            var inline10120 int = ref_get__Ref_3int(t4650)
            t4651 = inline10120
            var t4652 int = t4651 + 1
            ref_set__Ref_3int(t4649, t4652)
            continue
        } else {
            break Loop_loop4646
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4693 bool = value__204 >= 48
    var jp4669 bool
    if t4693 {
        var t4694 bool = value__204 <= 57
        jp4669 = t4694
    } else {
        jp4669 = false
    }
    if jp4669 {
        var t4670 uint8 = value__204 - 48
        var t4671 uint32 = uint32(uint8(t4670))
        var t4672 Option__uint32 = Option__uint32_Some{
            _0: t4671,
        }
        return t4672
    } else {
        var t4691 bool = value__204 >= 65
        var jp4676 bool
        if t4691 {
            var t4692 bool = value__204 <= 70
            jp4676 = t4692
        } else {
            jp4676 = false
        }
        if jp4676 {
            var t4677 uint8 = value__204 - 65
            var t4678 uint8 = t4677 + 10
            var t4679 uint32 = uint32(uint8(t4678))
            var t4680 Option__uint32 = Option__uint32_Some{
                _0: t4679,
            }
            return t4680
        } else {
            var t4689 bool = value__204 >= 97
            var jp4684 bool
            if t4689 {
                var t4690 bool = value__204 <= 102
                jp4684 = t4690
            } else {
                jp4684 = false
            }
            if jp4684 {
                var t4685 uint8 = value__204 - 97
                var t4686 uint8 = t4685 + 10
                var t4687 uint32 = uint32(uint8(t4686))
                var t4688 Option__uint32 = Option__uint32_Some{
                    _0: t4687,
                }
                return t4688
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4699 *ref_int_x = value__205.index
    var t4700 int
    var inline10152 int = ref_get__Ref_3int(t4699)
    t4700 = inline10152
    var t4701 int = t4700 + 4
    var t4702 string = value__205.input
    var t4703 int
    var inline10150 int = _goml_runtime_core_string_len(t4702)
    t4703 = inline10150
    var t4704 bool = t4701 > t4703
    if t4704 {
        var t4705 string
        var inline10126 string = "incomplete unicode escape"
        var inline10127 string = "" + inline10126
        var inline10128 string = inline10127 + " at byte "
        var inline10129 *ref_int_x = value__205.index
        var inline10130 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10129)
        var inline10131 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10130)
        var inline10132 string = inline10128 + inline10131
        t4705 = inline10132
        var t4706 Result__uint32__string = Result__uint32__string_Err{
            _0: t4705,
        }
        return t4706
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4713:
        for {
            var t4714 bool = for_index744 < for_limit745
            if t4714 {
                var for_item746 int = for_index744
                var t4715 int = for_index744 + 1
                for_index744 = t4715
                var t4716 string = value__205.input
                var t4717 *ref_int_x = value__205.index
                var t4718 int
                var inline10144 int = ref_get__Ref_3int(t4717)
                t4718 = inline10144
                var t4719 int = t4718 + for_item746
                var t4720 uint8
                var inline10142 uint8 = _goml_runtime_core_string_byte_get(t4716, t4719)
                t4720 = inline10142
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4720)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4722 string
                    var inline10134 string = "invalid unicode escape"
                    var inline10135 string = "" + inline10134
                    var inline10136 string = inline10135 + " at byte "
                    var inline10137 *ref_int_x = value__205.index
                    var inline10138 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10137)
                    var inline10139 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10138)
                    var inline10140 string = inline10136 + inline10139
                    t4722 = inline10140
                    var t4723 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4722,
                    }
                    return t4723
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4724 uint32 = result__206 * 16
                    var t4725 uint32 = t4724 + x749
                    result__206 = t4725
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4713
            }
        }
        var t4708 *ref_int_x = value__205.index
        var t4709 *ref_int_x = value__205.index
        var t4710 int
        var inline10148 int = ref_get__Ref_3int(t4709)
        t4710 = inline10148
        var t4711 int = t4710 + 4
        ref_set__Ref_3int(t4708, t4711)
        var t4712 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4712
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline10165 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline10165
    switch mtmp753.(type) {
    case Option__char_None:
        var t4730 string
        var inline10154 string = "invalid unicode codepoint"
        var inline10155 string = "" + inline10154
        var inline10156 string = inline10155 + " at byte "
        var inline10157 *ref_int_x = value__209.index
        var inline10158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10157)
        var inline10159 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10158)
        var inline10160 string = inline10156 + inline10159
        t4730 = inline10160
        var t4731 Result__unit__string = Result__unit__string_Err{
            _0: t4730,
        }
        return t4731
    case Option__char_Some:
        var x754 rune = mtmp753.(Option__char_Some)._0
        var inline10162 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10162)
        var t4732 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4732
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4736 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4736 = x757
        var t4796 bool = jp4736 >= 55296
        var jp4740 bool
        if t4796 {
            var t4797 bool = jp4736 <= 56319
            jp4740 = t4797
        } else {
            jp4740 = false
        }
        if jp4740 {
            var t4776 *ref_int_x = value__213.index
            var t4777 int
            var inline10205 int = ref_get__Ref_3int(t4776)
            t4777 = inline10205
            var t4778 int = t4777 + 2
            var t4779 string = value__213.input
            var t4780 int
            var inline10203 int = _goml_runtime_core_string_len(t4779)
            t4780 = inline10203
            var t4781 bool = t4778 > t4780
            var jp4769 bool
            if t4781 {
                jp4769 = true
            } else {
                var t4782 string = value__213.input
                var t4783 *ref_int_x = value__213.index
                var t4784 int
                var inline10169 int = ref_get__Ref_3int(t4783)
                t4784 = inline10169
                var t4785 uint8
                var inline10167 uint8 = _goml_runtime_core_string_byte_get(t4782, t4784)
                t4785 = inline10167
                var t4786 bool = t4785 != 92
                jp4769 = t4786
            }
            var jp4744 bool
            if jp4769 {
                jp4744 = true
            } else {
                var t4770 string = value__213.input
                var t4771 *ref_int_x = value__213.index
                var t4772 int
                var inline10173 int = ref_get__Ref_3int(t4771)
                t4772 = inline10173
                var t4773 int = t4772 + 1
                var t4774 uint8
                var inline10171 uint8 = _goml_runtime_core_string_byte_get(t4770, t4773)
                t4774 = inline10171
                var t4775 bool = t4774 != 117
                jp4744 = t4775
            }
            if jp4744 {
                var t4745 string
                var inline10175 string = "missing low surrogate"
                var inline10176 string = "" + inline10175
                var inline10177 string = inline10176 + " at byte "
                var inline10178 *ref_int_x = value__213.index
                var inline10179 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10178)
                var inline10180 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10179)
                var inline10181 string = inline10177 + inline10180
                t4745 = inline10181
                var t4746 Result__unit__string = Result__unit__string_Err{
                    _0: t4745,
                }
                return t4746
            } else {
                var t4747 *ref_int_x = value__213.index
                var t4748 *ref_int_x = value__213.index
                var t4749 int
                var inline10201 int = ref_get__Ref_3int(t4748)
                t4749 = inline10201
                var t4750 int = t4749 + 2
                ref_set__Ref_3int(t4747, t4750)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4752 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4752 = x761
                    var t4765 bool = jp4752 < 56320
                    var jp4756 bool
                    if t4765 {
                        jp4756 = true
                    } else {
                        var t4766 bool = jp4752 > 57343
                        jp4756 = t4766
                    }
                    if jp4756 {
                        var t4757 string
                        var inline10183 string = "invalid low surrogate"
                        var inline10184 string = "" + inline10183
                        var inline10185 string = inline10184 + " at byte "
                        var inline10186 *ref_int_x = value__213.index
                        var inline10187 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10186)
                        var inline10188 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10187)
                        var inline10189 string = inline10185 + inline10188
                        t4757 = inline10189
                        var t4758 Result__unit__string = Result__unit__string_Err{
                            _0: t4757,
                        }
                        return t4758
                    } else {
                        var t4759 uint32 = jp4736 - 55296
                        var t4760 uint32 = t4759 * 1024
                        var t4761 uint32 = 65536 + t4760
                        var t4762 uint32 = t4761 + jp4752
                        var t4763 uint32 = t4762 - 56320
                        var inline10191 Option__char = char_from_uint32(t4763)
                        switch inline10191.(type) {
                        case Option__char_None:
                            var inline10192 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10193 Result__unit__string = Result__unit__string_Err{
                                _0: inline10192,
                            }
                            return inline10193
                        case Option__char_Some:
                            var inline10194 rune = inline10191.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10194)
                            var inline10197 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline10197
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4767 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4767
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4794 bool = jp4736 >= 56320
            var jp4790 bool
            if t4794 {
                var t4795 bool = jp4736 <= 57343
                jp4790 = t4795
            } else {
                jp4790 = false
            }
            if jp4790 {
                var t4791 string
                var inline10207 string = "unexpected low surrogate"
                var inline10208 string = "" + inline10207
                var inline10209 string = inline10208 + " at byte "
                var inline10210 *ref_int_x = value__213.index
                var inline10211 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10210)
                var inline10212 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10211)
                var inline10213 string = inline10209 + inline10212
                t4791 = inline10213
                var t4792 Result__unit__string = Result__unit__string_Err{
                    _0: t4791,
                }
                return t4792
            } else {
                var t4793 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4736)
                return t4793
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4798 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4798
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4914 *ref_int_x = value__217.index
    var t4915 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4914)
    var t4916 string = value__217.input
    var t4917 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4916)
    var t4918 bool = t4915 >= t4917
    var jp4906 bool
    if t4918 {
        jp4906 = true
    } else {
        var t4919 string = value__217.input
        var t4920 *ref_int_x = value__217.index
        var t4921 int
        var inline10217 int = ref_get__Ref_3int(t4920)
        t4921 = inline10217
        var t4922 uint8
        var inline10215 uint8 = _goml_runtime_core_string_byte_get(t4919, t4921)
        t4922 = inline10215
        var t4923 bool = t4922 != 34
        jp4906 = t4923
    }
    if jp4906 {
        var t4907 string
        var inline10219 string = "expected string"
        var inline10220 string = "" + inline10219
        var inline10221 string = inline10220 + " at byte "
        var inline10222 *ref_int_x = value__217.index
        var inline10223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10222)
        var inline10224 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10223)
        var inline10225 string = inline10221 + inline10224
        t4907 = inline10225
        var t4908 Result__string__string = Result__string__string_Err{
            _0: t4907,
        }
        return t4908
    } else {
        var t4909 *ref_int_x = value__217.index
        var t4910 *ref_int_x = value__217.index
        var t4911 int
        var inline10229 int = ref_get__Ref_3int(t4910)
        t4911 = inline10229
        var t4912 int = t4911 + 1
        ref_set__Ref_3int(t4909, t4912)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4802 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4802)
        Loop_loop4806:
        for {
            var t4807 *ref_int_x = value__217.index
            var t4808 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4807)
            var t4809 string = value__217.input
            var t4810 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4809)
            var t4811 bool = t4808 < t4810
            if t4811 {
                var t4812 string = value__217.input
                var t4813 *ref_int_x = value__217.index
                var t4814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4813)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4812, t4814)
                var t4816 bool = byte__220 == 34
                if t4816 {
                    var t4824 *ref_int_x = value__217.index
                    var t4825 int
                    var inline10244 int = ref_get__Ref_3int(t4824)
                    t4825 = inline10244
                    var t4826 bool = segment__219 < t4825
                    if t4826 {
                        var t4827 string = value__217.input
                        var t4828 *ref_int_x = value__217.index
                        var t4829 int
                        var inline10233 int = ref_get__Ref_3int(t4828)
                        t4829 = inline10233
                        var t4830 string
                        var inline10231 string = string_byte_slice(t4827, segment__219, t4829)
                        t4830 = inline10231
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4830)
                    } else {}
                    var t4818 *ref_int_x = value__217.index
                    var t4819 *ref_int_x = value__217.index
                    var t4820 int
                    var inline10242 int = ref_get__Ref_3int(t4819)
                    t4820 = inline10242
                    var t4821 int = t4820 + 1
                    ref_set__Ref_3int(t4818, t4821)
                    var t4822 string
                    var inline10235 *_goml_vec_uint8 = builder__218.values
                    var inline10236 Tuple2_4bool_6string = string_from_utf8(inline10235)
                    var inline10237 string = inline10236._1
                    t4822 = inline10237
                    var t4823 Result__string__string = Result__string__string_Ok{
                        _0: t4822,
                    }
                    return t4823
                } else {
                    var t4833 bool = byte__220 == 92
                    if t4833 {
                        var t4888 *ref_int_x = value__217.index
                        var t4889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4888)
                        var t4890 bool = segment__219 < t4889
                        if t4890 {
                            var t4891 string = value__217.input
                            var t4892 *ref_int_x = value__217.index
                            var t4893 int
                            var inline10248 int = ref_get__Ref_3int(t4892)
                            t4893 = inline10248
                            var t4894 string
                            var inline10246 string = string_byte_slice(t4891, segment__219, t4893)
                            t4894 = inline10246
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4894)
                        } else {}
                        var t4835 *ref_int_x = value__217.index
                        var t4836 *ref_int_x = value__217.index
                        var t4837 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4836)
                        var t4838 int = t4837 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4835, t4838)
                        var t4881 *ref_int_x = value__217.index
                        var t4882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4881)
                        var t4883 string = value__217.input
                        var t4884 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4883)
                        var t4885 bool = t4882 >= t4884
                        if t4885 {
                            var t4886 string
                            var inline10250 string = "incomplete escape"
                            var inline10251 string = "" + inline10250
                            var inline10252 string = inline10251 + " at byte "
                            var inline10253 *ref_int_x = value__217.index
                            var inline10254 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10253)
                            var inline10255 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10254)
                            var inline10256 string = inline10252 + inline10255
                            t4886 = inline10256
                            var t4887 Result__string__string = Result__string__string_Err{
                                _0: t4886,
                            }
                            return t4887
                        } else {
                            var t4840 string = value__217.input
                            var t4841 *ref_int_x = value__217.index
                            var t4842 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4841)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4840, t4842)
                            var t4843 *ref_int_x = value__217.index
                            var t4844 *ref_int_x = value__217.index
                            var t4845 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4844)
                            var t4846 int = t4845 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4843, t4846)
                            var t4850 bool = escape__221 == 34
                            if t4850 {
                                var inline10258 rune = 34
                                var inline10259 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10258)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10259)
                                var t4848 *ref_int_x = value__217.index
                                var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                segment__219 = t4849
                                continue
                            } else {
                                var t4853 bool = escape__221 == 92
                                if t4853 {
                                    var inline10262 rune = 92
                                    var inline10263 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10262)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10263)
                                    var t4848 *ref_int_x = value__217.index
                                    var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                    segment__219 = t4849
                                    continue
                                } else {
                                    var t4856 bool = escape__221 == 47
                                    if t4856 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4848 *ref_int_x = value__217.index
                                        var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                        segment__219 = t4849
                                        continue
                                    } else {
                                        var t4859 bool = escape__221 == 98
                                        if t4859 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4848 *ref_int_x = value__217.index
                                                var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                segment__219 = t4849
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4848 *ref_int_x = value__217.index
                                                var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                segment__219 = t4849
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4863 bool = escape__221 == 102
                                            if t4863 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4848 *ref_int_x = value__217.index
                                                    var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                    segment__219 = t4849
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4848 *ref_int_x = value__217.index
                                                    var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                    segment__219 = t4849
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4867 bool = escape__221 == 110
                                                if t4867 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4848 *ref_int_x = value__217.index
                                                    var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                    segment__219 = t4849
                                                    continue
                                                } else {
                                                    var t4870 bool = escape__221 == 114
                                                    if t4870 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4848 *ref_int_x = value__217.index
                                                        var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                        segment__219 = t4849
                                                        continue
                                                    } else {
                                                        var t4873 bool = escape__221 == 116
                                                        if t4873 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4848 *ref_int_x = value__217.index
                                                            var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                            segment__219 = t4849
                                                            continue
                                                        } else {
                                                            var t4876 bool = escape__221 == 117
                                                            if t4876 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4848 *ref_int_x = value__217.index
                                                                    var t4849 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4848)
                                                                    segment__219 = t4849
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4878 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4878
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4879 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4880 Result__string__string = Result__string__string_Err{
                                                                    _0: t4879,
                                                                }
                                                                return t4880
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
                        var t4897 bool = byte__220 < 32
                        if t4897 {
                            var t4898 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4899 Result__string__string = Result__string__string_Err{
                                _0: t4898,
                            }
                            return t4899
                        } else {
                            var t4900 *ref_int_x = value__217.index
                            var t4901 *ref_int_x = value__217.index
                            var t4902 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4901)
                            var t4903 int = t4902 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4900, t4903)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4806
            }
        }
        var t4804 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4805 Result__string__string = Result__string__string_Err{
            _0: t4804,
        }
        return t4805
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4932 *ref_int_x = value__225.index
    var start__226 int
    var inline10283 int = ref_get__Ref_3int(t4932)
    start__226 = inline10283
    Loop_loop4937:
    for {
        var t4945 *ref_int_x = value__225.index
        var t4946 int
        var inline10279 int = ref_get__Ref_3int(t4945)
        t4946 = inline10279
        var t4947 string = value__225.input
        var t4948 int
        var inline10277 int = _goml_runtime_core_string_len(t4947)
        t4948 = inline10277
        var t4949 bool = t4946 < t4948
        var jp4939 bool
        if t4949 {
            var t4950 string = value__225.input
            var t4951 *ref_int_x = value__225.index
            var t4952 int
            var inline10271 int = ref_get__Ref_3int(t4951)
            t4952 = inline10271
            var t4953 uint8
            var inline10269 uint8 = _goml_runtime_core_string_byte_get(t4950, t4952)
            t4953 = inline10269
            var inline10266 bool = t4953 >= 48
            if inline10266 {
                var inline10267 bool = t4953 <= 57
                jp4939 = inline10267
            } else {
                jp4939 = false
            }
        } else {
            jp4939 = false
        }
        if jp4939 {
            var t4940 *ref_int_x = value__225.index
            var t4941 *ref_int_x = value__225.index
            var t4942 int
            var inline10275 int = ref_get__Ref_3int(t4941)
            t4942 = inline10275
            var t4943 int = t4942 + 1
            ref_set__Ref_3int(t4940, t4943)
            continue
        } else {
            break Loop_loop4937
        }
    }
    var t4934 *ref_int_x = value__225.index
    var t4935 int
    var inline10281 int = ref_get__Ref_3int(t4934)
    t4935 = inline10281
    var t4936 bool = t4935 > start__226
    return t4936
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4957 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4957)
    var t5078 string = value__227.input
    var t5079 *ref_int_x = value__227.index
    var t5080 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5079)
    var t5081 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5078, t5080)
    var t5082 bool = t5081 == 45
    if t5082 {
        var t5083 *ref_int_x = value__227.index
        var t5084 *ref_int_x = value__227.index
        var t5085 int
        var inline10287 int = ref_get__Ref_3int(t5084)
        t5085 = inline10287
        var t5086 int = t5085 + 1
        ref_set__Ref_3int(t5083, t5086)
    } else {}
    var t5041 *ref_int_x = value__227.index
    var t5042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5041)
    var t5043 string = value__227.input
    var t5044 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5043)
    var t5045 bool = t5042 >= t5044
    if t5045 {
        var t5046 string
        var inline10289 string = "incomplete number"
        var inline10290 string = "" + inline10289
        var inline10291 string = inline10290 + " at byte "
        var inline10292 *ref_int_x = value__227.index
        var inline10293 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10292)
        var inline10294 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10293)
        var inline10295 string = inline10291 + inline10294
        t5046 = inline10295
        var t5047 Result__string__string = Result__string__string_Err{
            _0: t5046,
        }
        return t5047
    } else {
        var t5049 string = value__227.input
        var t5050 *ref_int_x = value__227.index
        var t5051 int
        var inline10330 int = ref_get__Ref_3int(t5050)
        t5051 = inline10330
        var t5052 uint8
        var inline10328 uint8 = _goml_runtime_core_string_byte_get(t5049, t5051)
        t5052 = inline10328
        var t5053 bool = t5052 == 48
        if t5053 {
            var t5054 *ref_int_x = value__227.index
            var t5055 *ref_int_x = value__227.index
            var t5056 int
            var inline10318 int = ref_get__Ref_3int(t5055)
            t5056 = inline10318
            var t5057 int = t5056 + 1
            ref_set__Ref_3int(t5054, t5057)
            var t5063 *ref_int_x = value__227.index
            var t5064 int
            var inline10314 int = ref_get__Ref_3int(t5063)
            t5064 = inline10314
            var t5065 string = value__227.input
            var t5066 int
            var inline10312 int = _goml_runtime_core_string_len(t5065)
            t5066 = inline10312
            var t5067 bool = t5064 < t5066
            var jp5060 bool
            if t5067 {
                var t5068 string = value__227.input
                var t5069 *ref_int_x = value__227.index
                var t5070 int
                var inline10302 int = ref_get__Ref_3int(t5069)
                t5070 = inline10302
                var t5071 uint8
                var inline10300 uint8 = _goml_runtime_core_string_byte_get(t5068, t5070)
                t5071 = inline10300
                var inline10297 bool = t5071 >= 48
                if inline10297 {
                    var inline10298 bool = t5071 <= 57
                    jp5060 = inline10298
                } else {
                    jp5060 = false
                }
            } else {
                jp5060 = false
            }
            if jp5060 {
                var t5061 string
                var inline10304 string = "invalid leading zero"
                var inline10305 string = "" + inline10304
                var inline10306 string = inline10305 + " at byte "
                var inline10307 *ref_int_x = value__227.index
                var inline10308 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10307)
                var inline10309 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10308)
                var inline10310 string = inline10306 + inline10309
                t5061 = inline10310
                var t5062 Result__string__string = Result__string__string_Err{
                    _0: t5061,
                }
                return t5062
            } else {
                var t5031 *ref_int_x = value__227.index
                var t5032 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5031)
                var t5033 string = value__227.input
                var t5034 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5033)
                var t5035 bool = t5032 < t5034
                var jp5021 bool
                if t5035 {
                    var t5036 string = value__227.input
                    var t5037 *ref_int_x = value__227.index
                    var t5038 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5037)
                    var t5039 uint8
                    var inline10332 uint8 = _goml_runtime_core_string_byte_get(t5036, t5038)
                    t5039 = inline10332
                    var t5040 bool = t5039 == 46
                    jp5021 = t5040
                } else {
                    jp5021 = false
                }
                if jp5021 {
                    var t5022 *ref_int_x = value__227.index
                    var t5023 *ref_int_x = value__227.index
                    var t5024 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5023)
                    var t5025 int = t5024 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5022, t5025)
                    var t5027 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5028 bool = !t5027
                    if t5028 {
                        var t5029 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5030 Result__string__string = Result__string__string_Err{
                            _0: t5029,
                        }
                        return t5030
                    } else {
                        var t5003 *ref_int_x = value__227.index
                        var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                        var t5005 string = value__227.input
                        var t5006 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5005)
                        var t5007 bool = t5004 < t5006
                        var jp4968 bool
                        if t5007 {
                            var t5010 string = value__227.input
                            var t5011 *ref_int_x = value__227.index
                            var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5011)
                            var t5013 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5010, t5012)
                            var t5014 bool = t5013 == 101
                            if t5014 {
                                jp4968 = true
                            } else {
                                var t5015 string = value__227.input
                                var t5016 *ref_int_x = value__227.index
                                var t5017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5016)
                                var t5018 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5015, t5017)
                                var t5019 bool = t5018 == 69
                                jp4968 = t5019
                            }
                        } else {
                            jp4968 = false
                        }
                        if jp4968 {
                            var t4969 *ref_int_x = value__227.index
                            var t4970 *ref_int_x = value__227.index
                            var t4971 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4970)
                            var t4972 int = t4971 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4969, t4972)
                            var t4986 *ref_int_x = value__227.index
                            var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                            var t4988 string = value__227.input
                            var t4989 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4988)
                            var t4990 bool = t4987 < t4989
                            var jp4980 bool
                            if t4990 {
                                var t4993 string = value__227.input
                                var t4994 *ref_int_x = value__227.index
                                var t4995 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4994)
                                var t4996 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4993, t4995)
                                var t4997 bool = t4996 == 43
                                if t4997 {
                                    jp4980 = true
                                } else {
                                    var t4998 string = value__227.input
                                    var t4999 *ref_int_x = value__227.index
                                    var t5000 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4999)
                                    var t5001 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4998, t5000)
                                    var t5002 bool = t5001 == 45
                                    jp4980 = t5002
                                }
                            } else {
                                jp4980 = false
                            }
                            if jp4980 {
                                var t4981 *ref_int_x = value__227.index
                                var t4982 *ref_int_x = value__227.index
                                var t4983 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4982)
                                var t4984 int = t4983 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4981, t4984)
                            } else {}
                            var t4975 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4976 bool = !t4975
                            if t4976 {
                                var t4977 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4978 Result__string__string = Result__string__string_Err{
                                    _0: t4977,
                                }
                                return t4978
                            } else {
                                var t4962 string = value__227.input
                                var t4963 *ref_int_x = value__227.index
                                var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                                var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                                var t4966 Result__string__string = Result__string__string_Ok{
                                    _0: t4965,
                                }
                                return t4966
                            }
                        } else {
                            var t4962 string = value__227.input
                            var t4963 *ref_int_x = value__227.index
                            var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                            var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                            var t4966 Result__string__string = Result__string__string_Ok{
                                _0: t4965,
                            }
                            return t4966
                        }
                    }
                } else {
                    var t5003 *ref_int_x = value__227.index
                    var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                    var t5005 string = value__227.input
                    var t5006 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5005)
                    var t5007 bool = t5004 < t5006
                    var jp4968 bool
                    if t5007 {
                        var t5010 string = value__227.input
                        var t5011 *ref_int_x = value__227.index
                        var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5011)
                        var t5013 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5010, t5012)
                        var t5014 bool = t5013 == 101
                        if t5014 {
                            jp4968 = true
                        } else {
                            var t5015 string = value__227.input
                            var t5016 *ref_int_x = value__227.index
                            var t5017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5016)
                            var t5018 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5015, t5017)
                            var t5019 bool = t5018 == 69
                            jp4968 = t5019
                        }
                    } else {
                        jp4968 = false
                    }
                    if jp4968 {
                        var t4969 *ref_int_x = value__227.index
                        var t4970 *ref_int_x = value__227.index
                        var t4971 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4970)
                        var t4972 int = t4971 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4969, t4972)
                        var t4986 *ref_int_x = value__227.index
                        var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                        var t4988 string = value__227.input
                        var t4989 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4988)
                        var t4990 bool = t4987 < t4989
                        var jp4980 bool
                        if t4990 {
                            var t4993 string = value__227.input
                            var t4994 *ref_int_x = value__227.index
                            var t4995 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4994)
                            var t4996 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4993, t4995)
                            var t4997 bool = t4996 == 43
                            if t4997 {
                                jp4980 = true
                            } else {
                                var t4998 string = value__227.input
                                var t4999 *ref_int_x = value__227.index
                                var t5000 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4999)
                                var t5001 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4998, t5000)
                                var t5002 bool = t5001 == 45
                                jp4980 = t5002
                            }
                        } else {
                            jp4980 = false
                        }
                        if jp4980 {
                            var t4981 *ref_int_x = value__227.index
                            var t4982 *ref_int_x = value__227.index
                            var t4983 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4982)
                            var t4984 int = t4983 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4981, t4984)
                        } else {}
                        var t4975 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4976 bool = !t4975
                        if t4976 {
                            var t4977 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4978 Result__string__string = Result__string__string_Err{
                                _0: t4977,
                            }
                            return t4978
                        } else {
                            var t4962 string = value__227.input
                            var t4963 *ref_int_x = value__227.index
                            var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                            var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                            var t4966 Result__string__string = Result__string__string_Ok{
                                _0: t4965,
                            }
                            return t4966
                        }
                    } else {
                        var t4962 string = value__227.input
                        var t4963 *ref_int_x = value__227.index
                        var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                        var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                        var t4966 Result__string__string = Result__string__string_Ok{
                            _0: t4965,
                        }
                        return t4966
                    }
                }
            }
        } else {
            var t5074 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5075 bool = !t5074
            if t5075 {
                var t5076 string
                var inline10320 string = "expected number"
                var inline10321 string = "" + inline10320
                var inline10322 string = inline10321 + " at byte "
                var inline10323 *ref_int_x = value__227.index
                var inline10324 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10323)
                var inline10325 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10324)
                var inline10326 string = inline10322 + inline10325
                t5076 = inline10326
                var t5077 Result__string__string = Result__string__string_Err{
                    _0: t5076,
                }
                return t5077
            } else {
                var t5031 *ref_int_x = value__227.index
                var t5032 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5031)
                var t5033 string = value__227.input
                var t5034 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5033)
                var t5035 bool = t5032 < t5034
                var jp5021 bool
                if t5035 {
                    var t5036 string = value__227.input
                    var t5037 *ref_int_x = value__227.index
                    var t5038 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5037)
                    var t5039 uint8
                    var inline10332 uint8 = _goml_runtime_core_string_byte_get(t5036, t5038)
                    t5039 = inline10332
                    var t5040 bool = t5039 == 46
                    jp5021 = t5040
                } else {
                    jp5021 = false
                }
                if jp5021 {
                    var t5022 *ref_int_x = value__227.index
                    var t5023 *ref_int_x = value__227.index
                    var t5024 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5023)
                    var t5025 int = t5024 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5022, t5025)
                    var t5027 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5028 bool = !t5027
                    if t5028 {
                        var t5029 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5030 Result__string__string = Result__string__string_Err{
                            _0: t5029,
                        }
                        return t5030
                    } else {
                        var t5003 *ref_int_x = value__227.index
                        var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                        var t5005 string = value__227.input
                        var t5006 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5005)
                        var t5007 bool = t5004 < t5006
                        var jp4968 bool
                        if t5007 {
                            var t5010 string = value__227.input
                            var t5011 *ref_int_x = value__227.index
                            var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5011)
                            var t5013 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5010, t5012)
                            var t5014 bool = t5013 == 101
                            if t5014 {
                                jp4968 = true
                            } else {
                                var t5015 string = value__227.input
                                var t5016 *ref_int_x = value__227.index
                                var t5017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5016)
                                var t5018 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5015, t5017)
                                var t5019 bool = t5018 == 69
                                jp4968 = t5019
                            }
                        } else {
                            jp4968 = false
                        }
                        if jp4968 {
                            var t4969 *ref_int_x = value__227.index
                            var t4970 *ref_int_x = value__227.index
                            var t4971 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4970)
                            var t4972 int = t4971 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4969, t4972)
                            var t4986 *ref_int_x = value__227.index
                            var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                            var t4988 string = value__227.input
                            var t4989 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4988)
                            var t4990 bool = t4987 < t4989
                            var jp4980 bool
                            if t4990 {
                                var t4993 string = value__227.input
                                var t4994 *ref_int_x = value__227.index
                                var t4995 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4994)
                                var t4996 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4993, t4995)
                                var t4997 bool = t4996 == 43
                                if t4997 {
                                    jp4980 = true
                                } else {
                                    var t4998 string = value__227.input
                                    var t4999 *ref_int_x = value__227.index
                                    var t5000 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4999)
                                    var t5001 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4998, t5000)
                                    var t5002 bool = t5001 == 45
                                    jp4980 = t5002
                                }
                            } else {
                                jp4980 = false
                            }
                            if jp4980 {
                                var t4981 *ref_int_x = value__227.index
                                var t4982 *ref_int_x = value__227.index
                                var t4983 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4982)
                                var t4984 int = t4983 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4981, t4984)
                            } else {}
                            var t4975 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4976 bool = !t4975
                            if t4976 {
                                var t4977 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4978 Result__string__string = Result__string__string_Err{
                                    _0: t4977,
                                }
                                return t4978
                            } else {
                                var t4962 string = value__227.input
                                var t4963 *ref_int_x = value__227.index
                                var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                                var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                                var t4966 Result__string__string = Result__string__string_Ok{
                                    _0: t4965,
                                }
                                return t4966
                            }
                        } else {
                            var t4962 string = value__227.input
                            var t4963 *ref_int_x = value__227.index
                            var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                            var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                            var t4966 Result__string__string = Result__string__string_Ok{
                                _0: t4965,
                            }
                            return t4966
                        }
                    }
                } else {
                    var t5003 *ref_int_x = value__227.index
                    var t5004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5003)
                    var t5005 string = value__227.input
                    var t5006 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5005)
                    var t5007 bool = t5004 < t5006
                    var jp4968 bool
                    if t5007 {
                        var t5010 string = value__227.input
                        var t5011 *ref_int_x = value__227.index
                        var t5012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5011)
                        var t5013 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5010, t5012)
                        var t5014 bool = t5013 == 101
                        if t5014 {
                            jp4968 = true
                        } else {
                            var t5015 string = value__227.input
                            var t5016 *ref_int_x = value__227.index
                            var t5017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5016)
                            var t5018 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5015, t5017)
                            var t5019 bool = t5018 == 69
                            jp4968 = t5019
                        }
                    } else {
                        jp4968 = false
                    }
                    if jp4968 {
                        var t4969 *ref_int_x = value__227.index
                        var t4970 *ref_int_x = value__227.index
                        var t4971 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4970)
                        var t4972 int = t4971 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4969, t4972)
                        var t4986 *ref_int_x = value__227.index
                        var t4987 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4986)
                        var t4988 string = value__227.input
                        var t4989 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4988)
                        var t4990 bool = t4987 < t4989
                        var jp4980 bool
                        if t4990 {
                            var t4993 string = value__227.input
                            var t4994 *ref_int_x = value__227.index
                            var t4995 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4994)
                            var t4996 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4993, t4995)
                            var t4997 bool = t4996 == 43
                            if t4997 {
                                jp4980 = true
                            } else {
                                var t4998 string = value__227.input
                                var t4999 *ref_int_x = value__227.index
                                var t5000 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4999)
                                var t5001 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4998, t5000)
                                var t5002 bool = t5001 == 45
                                jp4980 = t5002
                            }
                        } else {
                            jp4980 = false
                        }
                        if jp4980 {
                            var t4981 *ref_int_x = value__227.index
                            var t4982 *ref_int_x = value__227.index
                            var t4983 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4982)
                            var t4984 int = t4983 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4981, t4984)
                        } else {}
                        var t4975 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4976 bool = !t4975
                        if t4976 {
                            var t4977 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4978 Result__string__string = Result__string__string_Err{
                                _0: t4977,
                            }
                            return t4978
                        } else {
                            var t4962 string = value__227.input
                            var t4963 *ref_int_x = value__227.index
                            var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                            var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                            var t4966 Result__string__string = Result__string__string_Ok{
                                _0: t4965,
                            }
                            return t4966
                        }
                    } else {
                        var t4962 string = value__227.input
                        var t4963 *ref_int_x = value__227.index
                        var t4964 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4963)
                        var t4965 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4962, start__228, t4964)
                        var t4966 Result__string__string = Result__string__string_Ok{
                            _0: t4965,
                        }
                        return t4966
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5109 *ref_int_x = value__230.index
    var t5110 int
    var inline10360 int = ref_get__Ref_3int(t5109)
    t5110 = inline10360
    var t5111 int
    var inline10358 int = _goml_runtime_core_string_len(expected__231)
    t5111 = inline10358
    var t5112 int = t5110 + t5111
    var t5113 string = value__230.input
    var t5114 int
    var inline10356 int = _goml_runtime_core_string_len(t5113)
    t5114 = inline10356
    var t5115 bool = t5112 <= t5114
    var jp5100 bool
    if t5115 {
        var t5116 string = value__230.input
        var t5117 *ref_int_x = value__230.index
        var t5118 int
        var inline10340 int = ref_get__Ref_3int(t5117)
        t5118 = inline10340
        var t5119 *ref_int_x = value__230.index
        var t5120 int
        var inline10338 int = ref_get__Ref_3int(t5119)
        t5120 = inline10338
        var t5121 int
        var inline10336 int = _goml_runtime_core_string_len(expected__231)
        t5121 = inline10336
        var t5122 int = t5120 + t5121
        var t5123 string
        var inline10334 string = string_byte_slice(t5116, t5118, t5122)
        t5123 = inline10334
        var t5124 bool = t5123 == expected__231
        jp5100 = t5124
    } else {
        jp5100 = false
    }
    if jp5100 {
        var t5101 *ref_int_x = value__230.index
        var t5102 *ref_int_x = value__230.index
        var t5103 int
        var inline10346 int = ref_get__Ref_3int(t5102)
        t5103 = inline10346
        var t5104 int
        var inline10344 int = _goml_runtime_core_string_len(expected__231)
        t5104 = inline10344
        var t5105 int = t5103 + t5104
        ref_set__Ref_3int(t5101, t5105)
        var t5106 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t5106
    } else {
        var t5107 string
        var inline10348 string = "invalid literal"
        var inline10349 string = "" + inline10348
        var inline10350 string = inline10349 + " at byte "
        var inline10351 *ref_int_x = value__230.index
        var inline10352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10351)
        var inline10353 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10352)
        var inline10354 string = inline10350 + inline10353
        t5107 = inline10354
        var t5108 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5107,
        }
        return t5108
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5127 *ref_int_x = value__233.index
    var t5128 *ref_int_x = value__233.index
    var t5129 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5128)
    var t5130 int = t5129 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5127, t5130)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8833 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t5185 *ref_int_x = value__233.index
    var t5186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5185)
    var t5187 string = value__233.input
    var t5188 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5187)
    var t5189 bool = t5186 < t5188
    var jp5178 bool
    if t5189 {
        var t5190 string = value__233.input
        var t5191 *ref_int_x = value__233.index
        var t5192 int
        var inline10364 int = ref_get__Ref_3int(t5191)
        t5192 = inline10364
        var t5193 uint8
        var inline10362 uint8 = _goml_runtime_core_string_byte_get(t5190, t5192)
        t5193 = inline10362
        var t5194 bool = t5193 == 93
        jp5178 = t5194
    } else {
        jp5178 = false
    }
    if jp5178 {
        var t5179 *ref_int_x = value__233.index
        var t5180 *ref_int_x = value__233.index
        var t5181 int
        var inline10368 int = ref_get__Ref_3int(t5180)
        t5181 = inline10368
        var t5182 int = t5181 + 1
        ref_set__Ref_3int(t5179, t5182)
        var t5183 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8833,
        }
        var t5184 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5183,
        }
        return t5184
    } else {
        Loop_loop5135:
        for {
            var t5136 *ref_int_x = value__233.index
            var t5137 int
            var inline10410 int = ref_get__Ref_3int(t5136)
            t5137 = inline10410
            var t5138 string = value__233.input
            var t5139 int
            var inline10408 int = _goml_runtime_core_string_len(t5138)
            t5139 = inline10408
            var t5140 bool = t5137 < t5139
            if t5140 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5142 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp5142 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(vec_literal__8833, jp5142)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5144 *ref_int_x = value__233.index
                    var t5145 int
                    var inline10404 int = ref_get__Ref_3int(t5144)
                    t5145 = inline10404
                    var t5146 string = value__233.input
                    var t5147 int
                    var inline10402 int = _goml_runtime_core_string_len(t5146)
                    t5147 = inline10402
                    var t5148 bool = t5145 >= t5147
                    if t5148 {
                        var t5149 string
                        var inline10370 string = "unterminated array"
                        var inline10371 string = "" + inline10370
                        var inline10372 string = inline10371 + " at byte "
                        var inline10373 *ref_int_x = value__233.index
                        var inline10374 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10373)
                        var inline10375 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10374)
                        var inline10376 string = inline10372 + inline10375
                        t5149 = inline10376
                        var t5150 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5149,
                        }
                        return t5150
                    } else {
                        var t5152 string = value__233.input
                        var t5153 *ref_int_x = value__233.index
                        var t5154 int
                        var inline10400 int = ref_get__Ref_3int(t5153)
                        t5154 = inline10400
                        var t5155 uint8
                        var inline10398 uint8 = _goml_runtime_core_string_byte_get(t5152, t5154)
                        t5155 = inline10398
                        var t5156 bool = t5155 == 93
                        if t5156 {
                            var t5157 *ref_int_x = value__233.index
                            var t5158 *ref_int_x = value__233.index
                            var t5159 int
                            var inline10380 int = ref_get__Ref_3int(t5158)
                            t5159 = inline10380
                            var t5160 int = t5159 + 1
                            ref_set__Ref_3int(t5157, t5160)
                            var t5161 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8833,
                            }
                            var t5162 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t5161,
                            }
                            return t5162
                        } else {
                            var t5164 string = value__233.input
                            var t5165 *ref_int_x = value__233.index
                            var t5166 int
                            var inline10396 int = ref_get__Ref_3int(t5165)
                            t5166 = inline10396
                            var t5167 uint8
                            var inline10394 uint8 = _goml_runtime_core_string_byte_get(t5164, t5166)
                            t5167 = inline10394
                            var t5168 bool = t5167 == 44
                            if t5168 {
                                var t5169 *ref_int_x = value__233.index
                                var t5170 *ref_int_x = value__233.index
                                var t5171 int
                                var inline10384 int = ref_get__Ref_3int(t5170)
                                t5171 = inline10384
                                var t5172 int = t5171 + 1
                                ref_set__Ref_3int(t5169, t5172)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5174 string
                                var inline10386 string = "expected array separator"
                                var inline10387 string = "" + inline10386
                                var inline10388 string = inline10387 + " at byte "
                                var inline10389 *ref_int_x = value__233.index
                                var inline10390 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10389)
                                var inline10391 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10390)
                                var inline10392 string = inline10388 + inline10391
                                t5174 = inline10392
                                var t5175 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5174,
                                }
                                return t5175
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t5176 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t5176
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5135
            }
        }
        var t5133 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5134 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5133,
        }
        return t5134
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5197 *ref_int_x = value__236.index
    var t5198 *ref_int_x = value__236.index
    var t5199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5198)
    var t5200 int = t5199 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5197, t5200)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10035 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5279 *ref_int_x = value__236.index
    var t5280 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5279)
    var t5281 string = value__236.input
    var t5282 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5281)
    var t5283 bool = t5280 < t5282
    var jp5272 bool
    if t5283 {
        var t5284 string = value__236.input
        var t5285 *ref_int_x = value__236.index
        var t5286 int
        var inline10414 int = ref_get__Ref_3int(t5285)
        t5286 = inline10414
        var t5287 uint8
        var inline10412 uint8 = _goml_runtime_core_string_byte_get(t5284, t5286)
        t5287 = inline10412
        var t5288 bool = t5287 == 125
        jp5272 = t5288
    } else {
        jp5272 = false
    }
    if jp5272 {
        var t5273 *ref_int_x = value__236.index
        var t5274 *ref_int_x = value__236.index
        var t5275 int
        var inline10418 int = ref_get__Ref_3int(t5274)
        t5275 = inline10418
        var t5276 int = t5275 + 1
        ref_set__Ref_3int(t5273, t5276)
        var t5277 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10035,
        }
        var t5278 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5277,
        }
        return t5278
    } else {
        Loop_loop5205:
        for {
            var t5206 *ref_int_x = value__236.index
            var t5207 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5206)
            var t5208 string = value__236.input
            var t5209 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5208)
            var t5210 bool = t5207 < t5209
            if t5210 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5212 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5212 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5260 *ref_int_x = value__236.index
                    var t5261 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5260)
                    var t5262 string = value__236.input
                    var t5263 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5262)
                    var t5264 bool = t5261 >= t5263
                    var jp5252 bool
                    if t5264 {
                        jp5252 = true
                    } else {
                        var t5265 string = value__236.input
                        var t5266 *ref_int_x = value__236.index
                        var t5267 int
                        var inline10422 int = ref_get__Ref_3int(t5266)
                        t5267 = inline10422
                        var t5268 uint8
                        var inline10420 uint8 = _goml_runtime_core_string_byte_get(t5265, t5267)
                        t5268 = inline10420
                        var t5269 bool = t5268 != 58
                        jp5252 = t5269
                    }
                    if jp5252 {
                        var t5253 string
                        var inline10424 string = "expected object colon"
                        var inline10425 string = "" + inline10424
                        var inline10426 string = inline10425 + " at byte "
                        var inline10427 *ref_int_x = value__236.index
                        var inline10428 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10427)
                        var inline10429 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10428)
                        var inline10430 string = inline10426 + inline10429
                        t5253 = inline10430
                        var t5254 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5253,
                        }
                        return t5254
                    } else {
                        var t5255 *ref_int_x = value__236.index
                        var t5256 *ref_int_x = value__236.index
                        var t5257 int
                        var inline10434 int = ref_get__Ref_3int(t5256)
                        t5257 = inline10434
                        var t5258 int = t5257 + 1
                        ref_set__Ref_3int(t5255, t5258)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5215 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5215 = x816
                            var t5216 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5212,
                                _1: jp5215,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10035, t5216)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5218 *ref_int_x = value__236.index
                            var t5219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5218)
                            var t5220 string = value__236.input
                            var t5221 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5220)
                            var t5222 bool = t5219 >= t5221
                            if t5222 {
                                var t5223 string
                                var inline10436 string = "unterminated object"
                                var inline10437 string = "" + inline10436
                                var inline10438 string = inline10437 + " at byte "
                                var inline10439 *ref_int_x = value__236.index
                                var inline10440 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10439)
                                var inline10441 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10440)
                                var inline10442 string = inline10438 + inline10441
                                t5223 = inline10442
                                var t5224 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5223,
                                }
                                return t5224
                            } else {
                                var t5226 string = value__236.input
                                var t5227 *ref_int_x = value__236.index
                                var t5228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5227)
                                var t5229 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5226, t5228)
                                var t5230 bool = t5229 == 125
                                if t5230 {
                                    var t5231 *ref_int_x = value__236.index
                                    var t5232 *ref_int_x = value__236.index
                                    var t5233 int
                                    var inline10446 int = ref_get__Ref_3int(t5232)
                                    t5233 = inline10446
                                    var t5234 int = t5233 + 1
                                    ref_set__Ref_3int(t5231, t5234)
                                    var t5235 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10035,
                                    }
                                    var t5236 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5235,
                                    }
                                    return t5236
                                } else {
                                    var t5238 string = value__236.input
                                    var t5239 *ref_int_x = value__236.index
                                    var t5240 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5239)
                                    var t5241 uint8
                                    var inline10460 uint8 = _goml_runtime_core_string_byte_get(t5238, t5240)
                                    t5241 = inline10460
                                    var t5242 bool = t5241 == 44
                                    if t5242 {
                                        var t5243 *ref_int_x = value__236.index
                                        var t5244 *ref_int_x = value__236.index
                                        var t5245 int
                                        var inline10450 int = ref_get__Ref_3int(t5244)
                                        t5245 = inline10450
                                        var t5246 int = t5245 + 1
                                        ref_set__Ref_3int(t5243, t5246)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5248 string
                                        var inline10452 string = "expected object separator"
                                        var inline10453 string = "" + inline10452
                                        var inline10454 string = inline10453 + " at byte "
                                        var inline10455 *ref_int_x = value__236.index
                                        var inline10456 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10455)
                                        var inline10457 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10456)
                                        var inline10458 string = inline10454 + inline10457
                                        t5248 = inline10458
                                        var t5249 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5248,
                                        }
                                        return t5249
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5250 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5250
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5270 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5270
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5205
            }
        }
        var t5203 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5204 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5203,
        }
        return t5204
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5293 *ref_int_x = value__240.index
    var t5294 int
    var inline10498 int = ref_get__Ref_3int(t5293)
    t5294 = inline10498
    var t5295 string = value__240.input
    var t5296 int
    var inline10496 int = _goml_runtime_core_string_len(t5295)
    t5296 = inline10496
    var t5297 bool = t5294 >= t5296
    if t5297 {
        var t5298 string
        var inline10462 string = "expected JSON value"
        var inline10463 string = "" + inline10462
        var inline10464 string = inline10463 + " at byte "
        var inline10465 *ref_int_x = value__240.index
        var inline10466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10465)
        var inline10467 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10466)
        var inline10468 string = inline10464 + inline10467
        t5298 = inline10468
        var t5299 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5298,
        }
        return t5299
    } else {
        var t5300 string = value__240.input
        var t5301 *ref_int_x = value__240.index
        var t5302 int
        var inline10494 int = ref_get__Ref_3int(t5301)
        t5302 = inline10494
        var mtmp824 uint8
        var inline10492 uint8 = _goml_runtime_core_string_byte_get(t5300, t5302)
        mtmp824 = inline10492
        switch mtmp824 {
        case 123:
            var t5305 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5305
        case 91:
            var t5306 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5306
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5309 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5310 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5309,
                }
                return t5310
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5311 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5311
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5312 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5313 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5312)
            return t5313
        case 102:
            var t5314 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5315 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5314)
            return t5315
        case 110:
            var t5316 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5316
        default:
            var t5324 bool = mtmp824 == 45
            var jp5320 bool
            if t5324 {
                jp5320 = true
            } else {
                var inline10470 bool = mtmp824 >= 48
                if inline10470 {
                    var inline10471 bool = mtmp824 <= 57
                    jp5320 = inline10471
                } else {
                    jp5320 = false
                }
            }
            if jp5320 {
                var inline10473 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10475 string
                switch inline10473.(type) {
                case Result__string__string_Ok:
                    var inline10478 string = inline10473.(Result__string__string_Ok)._0
                    inline10475 = inline10478
                    var inline10476 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10475,
                    }
                    var inline10477 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10476,
                    }
                    return inline10477
                case Result__string__string_Err:
                    var inline10480 string = inline10473.(Result__string__string_Err)._0
                    var inline10482 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10480,
                    }
                    return inline10482
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5322 string
                var inline10484 string = "unexpected JSON token"
                var inline10485 string = "" + inline10484
                var inline10486 string = inline10485 + " at byte "
                var inline10487 *ref_int_x = value__240.index
                var inline10488 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10487)
                var inline10489 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10488)
                var inline10490 string = inline10486 + inline10489
                t5322 = inline10490
                var t5323 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5322,
                }
                return t5323
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10512 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10513 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10512,
    }
    parser__245 = inline10513
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5329 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5329 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5332 *ref_int_x = parser__245.index
        var t5333 int
        var inline10510 int = ref_get__Ref_3int(t5332)
        t5333 = inline10510
        var t5334 int
        var inline10508 int = _goml_runtime_core_string_len(input__244)
        t5334 = inline10508
        var t5335 bool = t5333 == t5334
        if t5335 {
            var t5336 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5329,
            }
            return t5336
        } else {
            var t5337 string
            var inline10500 string = "trailing JSON data"
            var inline10501 string = "" + inline10500
            var inline10502 string = inline10501 + " at byte "
            var inline10503 *ref_int_x = parser__245.index
            var inline10504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10503)
            var inline10505 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10504)
            var inline10506 string = inline10502 + inline10505
            t5337 = inline10506
            var t5338 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5337,
            }
            return t5338
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5339 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5339
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10546 rune = 34
    var inline10547 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10546)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10547)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10544 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10544
    Loop_loop5353:
    for {
        var t5354 bool = for_index833 < for_limit834
        if t5354 {
            var for_item835 int = for_index833
            var t5355 int = for_index833 + 1
            for_index833 = t5355
            var byte__252 uint8
            var inline10532 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10532
            var t5408 bool = byte__252 == 34
            var jp5406 bool
            if t5408 {
                jp5406 = true
            } else {
                var t5409 bool = byte__252 == 92
                jp5406 = t5409
            }
            var jp5403 bool
            if jp5406 {
                jp5403 = true
            } else {
                var t5407 bool = byte__252 == 8
                jp5403 = t5407
            }
            var jp5400 bool
            if jp5403 {
                jp5400 = true
            } else {
                var t5404 bool = byte__252 == 9
                jp5400 = t5404
            }
            var jp5397 bool
            if jp5400 {
                jp5397 = true
            } else {
                var t5401 bool = byte__252 == 10
                jp5397 = t5401
            }
            var jp5394 bool
            if jp5397 {
                jp5394 = true
            } else {
                var t5398 bool = byte__252 == 12
                jp5394 = t5398
            }
            var jp5391 bool
            if jp5394 {
                jp5391 = true
            } else {
                var t5395 bool = byte__252 == 13
                jp5391 = t5395
            }
            var jp5358 bool
            if jp5391 {
                jp5358 = true
            } else {
                var t5392 bool = byte__252 < 32
                jp5358 = t5392
            }
            if jp5358 {
                var t5387 bool = start__250 < for_item835
                if t5387 {
                    var t5388 string
                    var inline10518 string = string_byte_slice(value__249, start__250, for_item835)
                    t5388 = inline10518
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5388)
                } else {}
                var t5362 bool = byte__252 == 34
                if t5362 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5365 bool = byte__252 == 92
                    if t5365 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5368 bool = byte__252 == 8
                        if t5368 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5371 bool = byte__252 == 9
                            if t5371 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5374 bool = byte__252 == 10
                                if t5374 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5377 bool = byte__252 == 12
                                    if t5377 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5380 bool = byte__252 == 13
                                        if t5380 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5382 uint8 = byte__252 / 16
                                            var t5383 rune
                                            var inline10529 int = int(uint8(t5382))
                                            var inline10530 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10529)
                                            t5383 = inline10530
                                            var inline10526 string = _goml_m_inherent_i_char_i_char_i_to__string(t5383)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10526)
                                            var t5384_rhs uint8 = 16
                                            var t5384 uint8 = byte__252 % t5384_rhs
                                            var t5385 rune
                                            var inline10523 int = int(uint8(t5384))
                                            var inline10524 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10523)
                                            t5385 = inline10524
                                            var inline10520 string = _goml_m_inherent_i_char_i_char_i_to__string(t5385)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10520)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5361 int = for_item835 + 1
                start__250 = t5361
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5353
        }
    }
    var t5348 int
    var inline10542 int = _goml_runtime_core_string_len(value__249)
    t5348 = inline10542
    var t5349 bool = start__250 < t5348
    if t5349 {
        var t5350 int
        var inline10536 int = _goml_runtime_core_string_len(value__249)
        t5350 = inline10536
        var t5351 string
        var inline10534 string = string_byte_slice(value__249, start__250, t5350)
        t5351 = inline10534
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5351)
    } else {}
    var inline10538 rune = 34
    var inline10539 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10538)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10539)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10562 rune = 123
        var inline10563 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10562)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10563)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5414:
        for {
            var t5415 bool = for_index852 < for_limit851
            if t5415 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5416 int = for_index852 + 1
                for_index852 = t5416
                var t5422 bool = index__256 > 0
                if t5422 {
                    var inline10550 rune = 44
                    var inline10551 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10550)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10551)
                } else {}
                var t5418 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5418)
                var inline10554 rune = 58
                var inline10555 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10554)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10555)
                var t5419 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5419)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5420 int = compound_old859 + compound_value860
                index__256 = t5420
                continue
            } else {
                break Loop_loop5414
            }
        }
        var inline10558 rune = 125
        var inline10559 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10558)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10559)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10574 rune = 91
        var inline10575 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10574)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10575)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5426:
        for {
            var t5427 bool = for_index866 < for_limit865
            if t5427 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5428 int = for_index866 + 1
                for_index866 = t5428
                var t5432 bool = index__259 > 0
                if t5432 {
                    var inline10566 rune = 44
                    var inline10567 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10566)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10567)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5430 int = compound_old871 + compound_value872
                index__259 = t5430
                continue
            } else {
                break Loop_loop5426
            }
        }
        var inline10570 rune = 93
        var inline10571 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10570)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10571)
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
        var jp5437 string
        if x848 {
            jp5437 = "true"
        } else {
            jp5437 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5437)
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
    var inline10583 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10584 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10583,
    }
    builder__265 = inline10584
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10578 *_goml_vec_uint8 = builder__265.values
    var inline10579 Tuple2_4bool_6string = string_from_utf8(inline10578)
    var inline10580 string = inline10579._1
    return inline10580
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5448:
        for {
            var t5449 bool = for_index883 < for_limit882
            if t5449 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5450 int = for_index883 + 1
                for_index883 = t5450
                var t5452 string = for_item884._0
                var t5453 bool = t5452 == name__267
                if t5453 {
                    var t5454 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5455 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5454,
                    }
                    return t5455
                } else {
                    continue
                }
            } else {
                break Loop_loop5448
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5465 int
    var inline10595 int = _goml_runtime_core_string_len(value__272)
    t5465 = inline10595
    var t5466 bool = t5465 == 0
    if t5466 {
        return Option__int_None{}
    } else {
        var t5467 uint8
        var inline10592 int = 0
        var inline10593 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10592)
        t5467 = inline10593
        var negative__273 bool = t5467 == 45
        var jp5469 int
        if negative__273 {
            jp5469 = 1
        } else {
            jp5469 = 0
        }
        var index__274 int = jp5469
        var result__275 int = 0
        var t5490 int
        var inline10590 int = _goml_runtime_core_string_len(value__272)
        t5490 = inline10590
        var t5491 bool = index__274 == t5490
        if t5491 {
            return Option__int_None{}
        } else {
            Loop_loop5476:
            for {
                var t5477 int
                var inline10588 int = _goml_runtime_core_string_len(value__272)
                t5477 = inline10588
                var t5478 bool = index__274 < t5477
                if t5478 {
                    var byte__276 uint8
                    var inline10586 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10586
                    var t5488 bool = byte__276 < 48
                    var jp5483 bool
                    if t5488 {
                        jp5483 = true
                    } else {
                        var t5489 bool = byte__276 > 57
                        jp5483 = t5489
                    }
                    if jp5483 {
                        return Option__int_None{}
                    } else {
                        var t5484 int = result__275 * 10
                        var t5485 uint8 = byte__276 - 48
                        var t5486 int = int(uint8(t5485))
                        var t5487 int = t5484 + t5486
                        result__275 = t5487
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5480 int = compound_old895 + compound_value896
                        index__274 = t5480
                        continue
                    }
                } else {
                    break Loop_loop5476
                }
            }
            var jp5473 int
            if negative__273 {
                var t5475 int = 0 - result__275
                jp5473 = t5475
            } else {
                jp5473 = result__275
            }
            var t5474 Option__int = Option__int_Some{
                _0: jp5473,
            }
            return t5474
        }
    }
}

func main0() struct{} {
    var mtmp408 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6263 _goml_m_std_p_json_p_Value
    switch mtmp408.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x409 _goml_m_std_p_json_p_Value = mtmp408.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6263 = x409
        var mtmp412 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "name")
        switch mtmp412.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline11042 string = "missing name"
            var inline11043 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11042)
            _goml_runtime_core_string_println(inline11043)
            var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "version")
            switch mtmp417.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline11057 string = "missing version"
                var inline11058 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11057)
                _goml_runtime_core_string_println(inline11058)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x418 _goml_m_std_p_json_p_Value = mtmp417.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp419 Option__int
                switch x418.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline11068 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline11070 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11068)
                    mtmp419 = inline11070
                default:
                    mtmp419 = Option__int_None{}
                }
                switch mtmp419.(type) {
                case Option__int_None:
                    var inline11061 string = "invalid version"
                    var inline11062 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11061)
                    _goml_runtime_core_string_println(inline11062)
                case Option__int_Some:
                    var x420 int = mtmp419.(Option__int_Some)._0
                    var inline11065 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                    _goml_runtime_core_string_println(inline11065)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "stable")
            switch mtmp422.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline11072 string = "missing stable"
                var inline11073 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11072)
                _goml_runtime_core_string_println(inline11073)
                var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                println__T_string(t6267)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x423 _goml_m_std_p_json_p_Value = mtmp422.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11619 bool
                switch x423.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline11083 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11619 = inline11083
                    var inline11080 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11619)
                    _goml_runtime_core_string_println(inline11080)
                    var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                    println__T_string(t6267)
                    return struct{}{}
                default:
                    var inline11076 string = "invalid stable"
                    var inline11077 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11076)
                    _goml_runtime_core_string_println(inline11077)
                    var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                    println__T_string(t6267)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x413 _goml_m_std_p_json_p_Value = mtmp412.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11625 string
            switch x413.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline11053 string = x413.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11625 = inline11053
                var inline11050 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11625)
                _goml_runtime_core_string_println(inline11050)
                var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "version")
                switch mtmp417.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11057 string = "missing version"
                    var inline11058 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11057)
                    _goml_runtime_core_string_println(inline11058)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x418 _goml_m_std_p_json_p_Value = mtmp417.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp419 Option__int
                    switch x418.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11068 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11070 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11068)
                        mtmp419 = inline11070
                    default:
                        mtmp419 = Option__int_None{}
                    }
                    switch mtmp419.(type) {
                    case Option__int_None:
                        var inline11061 string = "invalid version"
                        var inline11062 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11061)
                        _goml_runtime_core_string_println(inline11062)
                    case Option__int_Some:
                        var x420 int = mtmp419.(Option__int_Some)._0
                        var inline11065 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                        _goml_runtime_core_string_println(inline11065)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "stable")
                switch mtmp422.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11072 string = "missing stable"
                    var inline11073 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11072)
                    _goml_runtime_core_string_println(inline11073)
                    var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                    println__T_string(t6267)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x423 _goml_m_std_p_json_p_Value = mtmp422.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11619 bool
                    switch x423.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11083 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11619 = inline11083
                        var inline11080 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11619)
                        _goml_runtime_core_string_println(inline11080)
                        var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                        println__T_string(t6267)
                        return struct{}{}
                    default:
                        var inline11076 string = "invalid stable"
                        var inline11077 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11076)
                        _goml_runtime_core_string_println(inline11077)
                        var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                        println__T_string(t6267)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline11046 string = "invalid name"
                var inline11047 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11046)
                _goml_runtime_core_string_println(inline11047)
                var mtmp417 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "version")
                switch mtmp417.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11057 string = "missing version"
                    var inline11058 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11057)
                    _goml_runtime_core_string_println(inline11058)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x418 _goml_m_std_p_json_p_Value = mtmp417.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp419 Option__int
                    switch x418.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11068 string = x418.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11070 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11068)
                        mtmp419 = inline11070
                    default:
                        mtmp419 = Option__int_None{}
                    }
                    switch mtmp419.(type) {
                    case Option__int_None:
                        var inline11061 string = "invalid version"
                        var inline11062 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11061)
                        _goml_runtime_core_string_println(inline11062)
                    case Option__int_Some:
                        var x420 int = mtmp419.(Option__int_Some)._0
                        var inline11065 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x420)
                        _goml_runtime_core_string_println(inline11065)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp422 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6263, "stable")
                switch mtmp422.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11072 string = "missing stable"
                    var inline11073 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11072)
                    _goml_runtime_core_string_println(inline11073)
                    var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                    println__T_string(t6267)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x423 _goml_m_std_p_json_p_Value = mtmp422.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11619 bool
                    switch x423.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11083 bool = x423.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11619 = inline11083
                        var inline11080 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11619)
                        _goml_runtime_core_string_println(inline11080)
                        var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                        println__T_string(t6267)
                        return struct{}{}
                    default:
                        var inline11076 string = "invalid stable"
                        var inline11077 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11076)
                        _goml_runtime_core_string_println(inline11077)
                        var t6267 string = _goml_m_std_p_json_p_encode(jp6263)
                        println__T_string(t6267)
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
        var inline11039 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x410)
        _goml_runtime_core_string_println(inline11039)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6283 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6283
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6323:
    for {
        var t6324 int
        var inline11094 int = _goml_runtime_core_string_len(x12)
        t6324 = inline11094
        var t6325 bool = index__26 < t6324
        if t6325 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6327 int = compound_old17 + x16
                index__26 = t6327
                continue
            } else {
                var t6329 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6329
            }
        } else {
            break Loop_loop6323
        }
    }
    var t6322 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6322
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t6359 string = _goml_runtime_core_int_to_string(self__32)
    return t6359
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline11104 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline11105 bool = inline11104._0
    var inline11106 rune = inline11104._1
    if inline11105 {
        return inline11106
    } else {
        var inline11109 rune = _goml_runtime_core_string_get("", -1)
        return inline11109
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t6444 *ref_int_x = ref__Ref_3int(value__431)
    return t6444
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t6447 int = ref_get__Ref_3int(self__432)
    return t6447
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__34 rune) string {
    var inline11116 uint32 = uint32(rune(self__34))
    var inline11117 bool = utf8_valid_scalar(inline11116)
    if inline11117 {
        var inline11118 string = _goml_runtime_core_char_to_string(self__34)
        return inline11118
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t6512 int = _goml_runtime_core_string_len(self__36)
    return t6512
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t6515 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t6515
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline11416 bool = string_is_char_boundary(self__41, start__42)
    var inline11418 bool
    if inline11416 {
        var inline11421 bool = string_is_char_boundary(self__41, end__43)
        inline11418 = inline11421
    } else {
        inline11418 = false
    }
    if inline11418 {
        var inline11419 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline11419
    } else {
        var inline11420 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline11420
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline11427 bool = utf8_valid_scalar(value__2)
    if inline11427 {
        var inline11428 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline11429 rune = inline11428._1
        var inline11431 Option__char = Option__char_Some{
            _0: inline11429,
        }
        return inline11431
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6853 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6853
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6858 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6858
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__258 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__259 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__258, elem__259)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6894 string
    t6894 = value__1
    _goml_runtime_core_string_println(t6894)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t7020 bool = index__6 < 0
    var jp7018 bool
    if t7020 {
        jp7018 = true
    } else {
        var t7021 bool = index__6 >= length__7
        jp7018 = t7021
    }
    if jp7018 {
        var inline11442 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11442
    } else {
        var t6905 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6905))
        var t6908 bool = first__8 < 128
        if t6908 {
            var inline11444 int = 1
            var inline11445 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline11445.(type) {
            case Option__char_None:
                var inline11446 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11446
            case Option__char_Some:
                var inline11447 rune = inline11445.(Option__char_Some)._0
                var inline11449 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11447,
                    _2: inline11444,
                }
                return inline11449
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6912 bool = first__8 < 194
            if t6912 {
                var inline11451 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11451
            } else {
                var t6916 bool = first__8 < 224
                if t6916 {
                    var t6929 int = length__7 - index__6
                    var t6930 bool = t6929 < 2
                    if t6930 {
                        var inline11453 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11453
                    } else {
                        var t6918 int = index__6 + 1
                        var t6919 uint8
                        var inline11467 uint8 = _goml_runtime_core_string_byte_get(value__5, t6918)
                        t6919 = inline11467
                        var second__9 uint32 = uint32(uint8(t6919))
                        var t6922 bool
                        var inline11464 bool = second__9 < 128
                        if inline11464 {
                            t6922 = true
                        } else {
                            var inline11465 bool = second__9 > 191
                            t6922 = inline11465
                        }
                        if t6922 {
                            var inline11455 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11455
                        } else {
                            var t6924_rhs uint32 = 31
                            var t6924 uint32 = first__8 & t6924_rhs
                            var t6925_rhs int = 6
                            var t6925 uint32 = t6924 << t6925_rhs
                            var t6926_rhs uint32 = 63
                            var t6926 uint32 = second__9 & t6926_rhs
                            var t6927 uint32 = t6925 | t6926
                            var inline11457 int = 2
                            var inline11458 Option__char = __goml_builtin_char_from_uint32(t6927)
                            switch inline11458.(type) {
                            case Option__char_None:
                                var inline11459 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11459
                            case Option__char_Some:
                                var inline11460 rune = inline11458.(Option__char_Some)._0
                                var inline11462 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11460,
                                    _2: inline11457,
                                }
                                return inline11462
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6934 bool = first__8 < 240
                    if t6934 {
                        var t6967 int = length__7 - index__6
                        var t6968 bool = t6967 < 3
                        if t6968 {
                            var inline11469 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11469
                        } else {
                            var t6936 int = index__6 + 1
                            var t6937 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6936)
                            var second__10 uint32 = uint32(uint8(t6937))
                            var t6938 int = index__6 + 2
                            var t6939 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6938)
                            var third__11 uint32 = uint32(uint8(t6939))
                            var t6965 bool = utf8_invalid_continuation(second__10)
                            var jp6960 bool
                            if t6965 {
                                jp6960 = true
                            } else {
                                var inline11471 bool = third__11 < 128
                                if inline11471 {
                                    jp6960 = true
                                } else {
                                    var inline11472 bool = third__11 > 191
                                    jp6960 = inline11472
                                }
                            }
                            var jp6954 bool
                            if jp6960 {
                                jp6954 = true
                            } else {
                                var t6963 bool = first__8 == 224
                                if t6963 {
                                    var t6964 bool = second__10 < 160
                                    jp6954 = t6964
                                } else {
                                    jp6954 = false
                                }
                            }
                            var jp6943 bool
                            if jp6954 {
                                jp6943 = true
                            } else {
                                var t6957 bool = first__8 == 237
                                if t6957 {
                                    var t6958 bool = second__10 >= 160
                                    jp6943 = t6958
                                } else {
                                    jp6943 = false
                                }
                            }
                            if jp6943 {
                                var inline11474 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11474
                            } else {
                                var t6945_rhs uint32 = 15
                                var t6945 uint32 = first__8 & t6945_rhs
                                var t6946_rhs int = 12
                                var t6946 uint32 = t6945 << t6946_rhs
                                var t6947_rhs uint32 = 63
                                var t6947 uint32 = second__10 & t6947_rhs
                                var t6948_rhs int = 6
                                var t6948 uint32 = t6947 << t6948_rhs
                                var t6949 uint32 = t6946 | t6948
                                var t6950_rhs uint32 = 63
                                var t6950 uint32 = third__11 & t6950_rhs
                                var t6951 uint32 = t6949 | t6950
                                var inline11476 int = 3
                                var inline11477 Option__char = __goml_builtin_char_from_uint32(t6951)
                                switch inline11477.(type) {
                                case Option__char_None:
                                    var inline11478 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline11478
                                case Option__char_Some:
                                    var inline11479 rune = inline11477.(Option__char_Some)._0
                                    var inline11481 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline11479,
                                        _2: inline11476,
                                    }
                                    return inline11481
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6972 bool = first__8 < 245
                        if t6972 {
                            var t7013 int = length__7 - index__6
                            var t7014 bool = t7013 < 4
                            if t7014 {
                                var t7015 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t7015
                            } else {
                                var t6974 int = index__6 + 1
                                var t6975 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6974)
                                var second__12 uint32 = uint32(uint8(t6975))
                                var t6976 int = index__6 + 2
                                var t6977 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6976)
                                var third__13 uint32 = uint32(uint8(t6977))
                                var t6978 int = index__6 + 3
                                var t6979 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6978)
                                var fourth__14 uint32 = uint32(uint8(t6979))
                                var t7011 bool = utf8_invalid_continuation(second__12)
                                var jp7009 bool
                                if t7011 {
                                    jp7009 = true
                                } else {
                                    var t7012 bool = utf8_invalid_continuation(third__13)
                                    jp7009 = t7012
                                }
                                var jp7003 bool
                                if jp7009 {
                                    jp7003 = true
                                } else {
                                    var t7010 bool = utf8_invalid_continuation(fourth__14)
                                    jp7003 = t7010
                                }
                                var jp6997 bool
                                if jp7003 {
                                    jp6997 = true
                                } else {
                                    var t7006 bool = first__8 == 240
                                    if t7006 {
                                        var t7007 bool = second__12 < 144
                                        jp6997 = t7007
                                    } else {
                                        jp6997 = false
                                    }
                                }
                                var jp6983 bool
                                if jp6997 {
                                    jp6983 = true
                                } else {
                                    var t7000 bool = first__8 == 244
                                    if t7000 {
                                        var t7001 bool = second__12 > 143
                                        jp6983 = t7001
                                    } else {
                                        jp6983 = false
                                    }
                                }
                                if jp6983 {
                                    var t6984 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6984
                                } else {
                                    var t6985_rhs uint32 = 7
                                    var t6985 uint32 = first__8 & t6985_rhs
                                    var t6986_rhs int = 18
                                    var t6986 uint32 = t6985 << t6986_rhs
                                    var t6987_rhs uint32 = 63
                                    var t6987 uint32 = second__12 & t6987_rhs
                                    var t6988_rhs int = 12
                                    var t6988 uint32 = t6987 << t6988_rhs
                                    var t6989 uint32 = t6986 | t6988
                                    var t6990_rhs uint32 = 63
                                    var t6990 uint32 = third__13 & t6990_rhs
                                    var t6991_rhs int = 6
                                    var t6991 uint32 = t6990 << t6991_rhs
                                    var t6992 uint32 = t6989 | t6991
                                    var t6993_rhs uint32 = 63
                                    var t6993 uint32 = fourth__14 & t6993_rhs
                                    var t6994 uint32 = t6992 | t6993
                                    var t6995 Tuple3_4bool_4char_3int = utf8_valid_decode(t6994, 4)
                                    return t6995
                                }
                            }
                        } else {
                            var t7016 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t7016
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t7041 uint32 = uint32(rune(value__29))
    var t7042 bool
    var inline11483 bool = t7041 <= 1114111
    if inline11483 {
        var inline11484 bool = t7041 >= 55296
        var inline11486 bool
        if inline11484 {
            var inline11488 bool = t7041 <= 57343
            inline11486 = inline11488
        } else {
            inline11486 = false
        }
        var inline11487 bool = !inline11486
        t7042 = inline11487
    } else {
        t7042 = false
    }
    if t7042 {
        var t7043 string = _goml_runtime_core_char_to_string(value__29)
        return t7043
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t7172 bool = string_is_char_boundary(value__21, start__22)
    var jp7169 bool
    if t7172 {
        var t7173 bool = string_is_char_boundary(value__21, end__23)
        jp7169 = t7173
    } else {
        jp7169 = false
    }
    if jp7169 {
        var t7170 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t7170
    } else {
        var t7171 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t7171
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t7180 bool
    var inline11516 bool = value__30 <= 1114111
    if inline11516 {
        var inline11517 bool = value__30 >= 55296
        var inline11519 bool
        if inline11517 {
            var inline11521 bool = value__30 <= 57343
            inline11519 = inline11521
        } else {
            inline11519 = false
        }
        var inline11520 bool = !inline11519
        t7180 = inline11520
    } else {
        t7180 = false
    }
    if t7180 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t7181 Option__char = Option__char_Some{
            _0: x24,
        }
        return t7181
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t7186 string = _goml_runtime_core_int_to_string(self__151)
    return t7186
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t7189 string = _goml_runtime_core_bool_to_string(self__148)
    return t7189
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t7192 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t7192
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11662 rune
    var inline11525 bool = utf8_valid_scalar(value__0)
    if inline11525 {
        var inline11526 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11527 rune = inline11526._1
        commute_field11662 = inline11527
        var t7198 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11662,
            _2: width__1,
        }
        return t7198
    } else {
        var inline11523 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11523
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t7203 bool = value__3 < 128
    if t7203 {
        return true
    } else {
        var t7204 bool = value__3 > 191
        return t7204
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t7209 bool = value__4 <= 1114111
    if t7209 {
        var t7213 bool = value__4 >= 55296
        var jp7211 bool
        if t7213 {
            var t7214 bool = value__4 <= 57343
            jp7211 = t7214
        } else {
            jp7211 = false
        }
        var t7212 bool = !jp7211
        return t7212
    } else {
        return false
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t7228 bool = index__16 < 0
    var jp7220 bool
    if t7228 {
        jp7220 = true
    } else {
        var t7229 int
        var inline11531 int = _goml_runtime_core_string_len(value__15)
        t7229 = inline11531
        var t7230 bool = index__16 > t7229
        jp7220 = t7230
    }
    if jp7220 {
        return false
    } else {
        var t7223 int
        var inline11535 int = _goml_runtime_core_string_len(value__15)
        t7223 = inline11535
        var t7224 bool = index__16 == t7223
        if t7224 {
            return true
        } else {
            var t7225 uint8
            var inline11533 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t7225 = inline11533
            var t7226_rhs uint8 = 192
            var t7226 uint8 = t7225 & t7226_rhs
            var t7227 bool = t7226 != 128
            return t7227
        }
    }
}

func main() {
    main0()
}
