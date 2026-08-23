package main

import (
    _goml_context "context"
    _goml_os "os"
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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

type _goml_vec_uint32 struct {
    items []uint32
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

type Tuple2_5int64_14Receiver_4unit struct {
    _0 int64
    _1 <-chan struct{}
}

type Tuple4_4bool_3int_3int_4bool struct {
    _0 bool
    _1 int
    _2 int
    _3 bool
}

type Tuple3_4bool_6uint64_6string struct {
    _0 bool
    _1 uint64
    _2 string
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

type Tuple2_4bool_7float64 struct {
    _0 bool
    _1 float64
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
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

type Tuple2_6uint64_4bool struct {
    _0 uint64
    _1 bool
}

type Tuple2_4bool_3int struct {
    _0 bool
    _1 int
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
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
    var t3494 *_goml_vec_uint8
    var inline10535 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    t3494 = inline10535
    var t3495 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t3494,
    }
    return t3495
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline10550 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline10550
    var t3509 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t3509, length__5)
    var for_index1 int = 0
    Loop_loop3511:
    for {
        var t3512 bool = for_index1 < length__5
        if t3512 {
            var for_item3 int = for_index1
            var t3513 int = for_index1 + 1
            for_index1 = t3513
            var t3514 *_goml_vec_uint8 = self__3.values
            var t3515 uint8
            var inline10546 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t3515 = inline10546
            vec_push__Vec_5uint8(t3514, t3515)
            continue
        } else {
            break Loop_loop3511
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t3518 string
    var inline10552 string = char_to_string(value__8)
    t3518 = inline10552
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t3518)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t5224 string = "" + message__201
    var t5225 string = t5224 + " at byte "
    var t5226 *ref_int_x = value__200.index
    var t5227 int
    var inline11891 int = ref_get__Ref_3int(t5226)
    t5227 = inline11891
    var t5228 string
    var inline11889 string = __goml_builtin_int_to_string(t5227)
    t5228 = inline11889
    var t5229 string = t5225 + t5228
    return t5229
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop5244:
    for {
        var t5252 *ref_int_x = value__203.index
        var t5253 int
        var inline11912 int = ref_get__Ref_3int(t5252)
        t5253 = inline11912
        var t5254 string = value__203.input
        var t5255 int
        var inline11910 int = _goml_runtime_core_string_len(t5254)
        t5255 = inline11910
        var t5256 bool = t5253 < t5255
        var jp5246 bool
        if t5256 {
            var t5257 string = value__203.input
            var t5258 *ref_int_x = value__203.index
            var t5259 int
            var inline11904 int = ref_get__Ref_3int(t5258)
            t5259 = inline11904
            var t5260 uint8
            var inline11902 uint8 = _goml_runtime_core_string_byte_get(t5257, t5259)
            t5260 = inline11902
            var inline11893 bool = t5260 == 9
            var inline11895 bool
            if inline11893 {
                inline11895 = true
            } else {
                var inline11900 bool = t5260 == 10
                inline11895 = inline11900
            }
            var inline11897 bool
            if inline11895 {
                inline11897 = true
            } else {
                var inline11899 bool = t5260 == 13
                inline11897 = inline11899
            }
            if inline11897 {
                jp5246 = true
            } else {
                var inline11898 bool = t5260 == 32
                jp5246 = inline11898
            }
        } else {
            jp5246 = false
        }
        if jp5246 {
            var t5247 *ref_int_x = value__203.index
            var t5248 *ref_int_x = value__203.index
            var t5249 int
            var inline11908 int = ref_get__Ref_3int(t5248)
            t5249 = inline11908
            var t5250 int = t5249 + 1
            ref_set__Ref_3int(t5247, t5250)
            continue
        } else {
            break Loop_loop5244
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__u32 {
    var t5291 bool = value__204 >= 48
    var jp5267 bool
    if t5291 {
        var t5292 bool = value__204 <= 57
        jp5267 = t5292
    } else {
        jp5267 = false
    }
    if jp5267 {
        var t5268 uint8 = value__204 - 48
        var t5269 uint32 = uint32(uint8(t5268))
        var t5270 Option__u32 = Option__u32{
            _tag: 1,
            _v1_0: t5269,
        }
        return t5270
    } else {
        var t5289 bool = value__204 >= 65
        var jp5274 bool
        if t5289 {
            var t5290 bool = value__204 <= 70
            jp5274 = t5290
        } else {
            jp5274 = false
        }
        if jp5274 {
            var t5275 uint8 = value__204 - 65
            var t5276 uint8 = t5275 + 10
            var t5277 uint32 = uint32(uint8(t5276))
            var t5278 Option__u32 = Option__u32{
                _tag: 1,
                _v1_0: t5277,
            }
            return t5278
        } else {
            var t5287 bool = value__204 >= 97
            var jp5282 bool
            if t5287 {
                var t5288 bool = value__204 <= 102
                jp5282 = t5288
            } else {
                jp5282 = false
            }
            if jp5282 {
                var t5283 uint8 = value__204 - 97
                var t5284 uint8 = t5283 + 10
                var t5285 uint32 = uint32(uint8(t5284))
                var t5286 Option__u32 = Option__u32{
                    _tag: 1,
                    _v1_0: t5285,
                }
                return t5286
            } else {
                return Option__u32{
                    _tag: 0,
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__u32__string {
    var t5297 *ref_int_x = value__205.index
    var t5298 int
    var inline11940 int = ref_get__Ref_3int(t5297)
    t5298 = inline11940
    var t5299 int = t5298 + 4
    var t5300 string = value__205.input
    var t5301 int
    var inline11938 int = _goml_runtime_core_string_len(t5300)
    t5301 = inline11938
    var t5302 bool = t5299 > t5301
    if t5302 {
        var t5303 string
        var inline11914 string = "incomplete unicode escape"
        var inline11915 string = "" + inline11914
        var inline11916 string = inline11915 + " at byte "
        var inline11917 *ref_int_x = value__205.index
        var inline11918 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11917)
        var inline11919 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11918)
        var inline11920 string = inline11916 + inline11919
        t5303 = inline11920
        var t5304 Result__u32__string = Result__u32__string{
            _tag: 1,
            _v1_0: t5303,
        }
        return t5304
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop5311:
        for {
            var t5312 bool = for_index744 < for_limit745
            if t5312 {
                var for_item746 int = for_index744
                var t5313 int = for_index744 + 1
                for_index744 = t5313
                var t5314 string = value__205.input
                var t5315 *ref_int_x = value__205.index
                var t5316 int
                var inline11932 int = ref_get__Ref_3int(t5315)
                t5316 = inline11932
                var t5317 int = t5316 + for_item746
                var t5318 uint8
                var inline11930 uint8 = _goml_runtime_core_string_byte_get(t5314, t5317)
                t5318 = inline11930
                var mtmp748 Option__u32 = _goml_m_std_p_json_p_hex__digit(t5318)
                switch mtmp748._tag {
                case 0:
                    var t5320 string
                    var inline11922 string = "invalid unicode escape"
                    var inline11923 string = "" + inline11922
                    var inline11924 string = inline11923 + " at byte "
                    var inline11925 *ref_int_x = value__205.index
                    var inline11926 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11925)
                    var inline11927 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11926)
                    var inline11928 string = inline11924 + inline11927
                    t5320 = inline11928
                    var t5321 Result__u32__string = Result__u32__string{
                        _tag: 1,
                        _v1_0: t5320,
                    }
                    return t5321
                case 1:
                    var x749 uint32 = mtmp748._v1_0
                    var t5322 uint32 = result__206 * 16
                    var t5323 uint32 = t5322 + x749
                    result__206 = t5323
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5311
            }
        }
        var t5306 *ref_int_x = value__205.index
        var t5307 *ref_int_x = value__205.index
        var t5308 int
        var inline11936 int = ref_get__Ref_3int(t5307)
        t5308 = inline11936
        var t5309 int = t5308 + 4
        ref_set__Ref_3int(t5306, t5309)
        var t5310 Result__u32__string = Result__u32__string{
            _tag: 0,
            _v0_0: result__206,
        }
        return t5310
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline11953 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline11953
    switch mtmp753._tag {
    case 0:
        var t5328 string
        var inline11942 string = "invalid unicode codepoint"
        var inline11943 string = "" + inline11942
        var inline11944 string = inline11943 + " at byte "
        var inline11945 *ref_int_x = value__209.index
        var inline11946 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11945)
        var inline11947 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11946)
        var inline11948 string = inline11944 + inline11947
        t5328 = inline11948
        var t5329 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: t5328,
        }
        return t5329
    case 1:
        var x754 rune = mtmp753._v1_0
        var inline11950 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline11950)
        var t5330 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t5330
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__u32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp5334 uint32
    switch mtmp756._tag {
    case 0:
        var x757 uint32 = mtmp756._v0_0
        jp5334 = x757
        var t5394 bool = jp5334 >= 55296
        var jp5338 bool
        if t5394 {
            var t5395 bool = jp5334 <= 56319
            jp5338 = t5395
        } else {
            jp5338 = false
        }
        if jp5338 {
            var t5374 *ref_int_x = value__213.index
            var t5375 int
            var inline11993 int = ref_get__Ref_3int(t5374)
            t5375 = inline11993
            var t5376 int = t5375 + 2
            var t5377 string = value__213.input
            var t5378 int
            var inline11991 int = _goml_runtime_core_string_len(t5377)
            t5378 = inline11991
            var t5379 bool = t5376 > t5378
            var jp5367 bool
            if t5379 {
                jp5367 = true
            } else {
                var t5380 string = value__213.input
                var t5381 *ref_int_x = value__213.index
                var t5382 int
                var inline11957 int = ref_get__Ref_3int(t5381)
                t5382 = inline11957
                var t5383 uint8
                var inline11955 uint8 = _goml_runtime_core_string_byte_get(t5380, t5382)
                t5383 = inline11955
                var t5384 bool = t5383 != 92
                jp5367 = t5384
            }
            var jp5342 bool
            if jp5367 {
                jp5342 = true
            } else {
                var t5368 string = value__213.input
                var t5369 *ref_int_x = value__213.index
                var t5370 int
                var inline11961 int = ref_get__Ref_3int(t5369)
                t5370 = inline11961
                var t5371 int = t5370 + 1
                var t5372 uint8
                var inline11959 uint8 = _goml_runtime_core_string_byte_get(t5368, t5371)
                t5372 = inline11959
                var t5373 bool = t5372 != 117
                jp5342 = t5373
            }
            if jp5342 {
                var t5343 string
                var inline11963 string = "missing low surrogate"
                var inline11964 string = "" + inline11963
                var inline11965 string = inline11964 + " at byte "
                var inline11966 *ref_int_x = value__213.index
                var inline11967 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11966)
                var inline11968 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11967)
                var inline11969 string = inline11965 + inline11968
                t5343 = inline11969
                var t5344 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t5343,
                }
                return t5344
            } else {
                var t5345 *ref_int_x = value__213.index
                var t5346 *ref_int_x = value__213.index
                var t5347 int
                var inline11989 int = ref_get__Ref_3int(t5346)
                t5347 = inline11989
                var t5348 int = t5347 + 2
                ref_set__Ref_3int(t5345, t5348)
                var mtmp760 Result__u32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp5350 uint32
                switch mtmp760._tag {
                case 0:
                    var x761 uint32 = mtmp760._v0_0
                    jp5350 = x761
                    var t5363 bool = jp5350 < 56320
                    var jp5354 bool
                    if t5363 {
                        jp5354 = true
                    } else {
                        var t5364 bool = jp5350 > 57343
                        jp5354 = t5364
                    }
                    if jp5354 {
                        var t5355 string
                        var inline11971 string = "invalid low surrogate"
                        var inline11972 string = "" + inline11971
                        var inline11973 string = inline11972 + " at byte "
                        var inline11974 *ref_int_x = value__213.index
                        var inline11975 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11974)
                        var inline11976 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11975)
                        var inline11977 string = inline11973 + inline11976
                        t5355 = inline11977
                        var t5356 Result__unit__string = Result__unit__string{
                            _tag: 1,
                            _v1_0: t5355,
                        }
                        return t5356
                    } else {
                        var t5357 uint32 = jp5334 - 55296
                        var t5358 uint32 = t5357 * 1024
                        var t5359 uint32 = 65536 + t5358
                        var t5360 uint32 = t5359 + jp5350
                        var t5361 uint32 = t5360 - 56320
                        var inline11979 Option__char = char_from_u32(t5361)
                        switch inline11979._tag {
                        case 0:
                            var inline11980 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline11981 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: inline11980,
                            }
                            return inline11981
                        case 1:
                            var inline11982 rune = inline11979._v1_0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline11982)
                            var inline11985 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return inline11985
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x762 string = mtmp760._v1_0
                    var t5365 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x762,
                    }
                    return t5365
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t5392 bool = jp5334 >= 56320
            var jp5388 bool
            if t5392 {
                var t5393 bool = jp5334 <= 57343
                jp5388 = t5393
            } else {
                jp5388 = false
            }
            if jp5388 {
                var t5389 string
                var inline11995 string = "unexpected low surrogate"
                var inline11996 string = "" + inline11995
                var inline11997 string = inline11996 + " at byte "
                var inline11998 *ref_int_x = value__213.index
                var inline11999 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11998)
                var inline12000 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11999)
                var inline12001 string = inline11997 + inline12000
                t5389 = inline12001
                var t5390 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t5389,
                }
                return t5390
            } else {
                var t5391 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp5334)
                return t5391
            }
        }
    case 1:
        var x758 string = mtmp756._v1_0
        var t5396 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x758,
        }
        return t5396
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t5512 *ref_int_x = value__217.index
    var t5513 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5512)
    var t5514 string = value__217.input
    var t5515 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5514)
    var t5516 bool = t5513 >= t5515
    var jp5504 bool
    if t5516 {
        jp5504 = true
    } else {
        var t5517 string = value__217.input
        var t5518 *ref_int_x = value__217.index
        var t5519 int
        var inline12005 int = ref_get__Ref_3int(t5518)
        t5519 = inline12005
        var t5520 uint8
        var inline12003 uint8 = _goml_runtime_core_string_byte_get(t5517, t5519)
        t5520 = inline12003
        var t5521 bool = t5520 != 34
        jp5504 = t5521
    }
    if jp5504 {
        var t5505 string
        var inline12007 string = "expected string"
        var inline12008 string = "" + inline12007
        var inline12009 string = inline12008 + " at byte "
        var inline12010 *ref_int_x = value__217.index
        var inline12011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12010)
        var inline12012 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12011)
        var inline12013 string = inline12009 + inline12012
        t5505 = inline12013
        var t5506 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5505,
        }
        return t5506
    } else {
        var t5507 *ref_int_x = value__217.index
        var t5508 *ref_int_x = value__217.index
        var t5509 int
        var inline12017 int = ref_get__Ref_3int(t5508)
        t5509 = inline12017
        var t5510 int = t5509 + 1
        ref_set__Ref_3int(t5507, t5510)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t5400 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5400)
        Loop_loop5404:
        for {
            var t5405 *ref_int_x = value__217.index
            var t5406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5405)
            var t5407 string = value__217.input
            var t5408 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5407)
            var t5409 bool = t5406 < t5408
            if t5409 {
                var t5410 string = value__217.input
                var t5411 *ref_int_x = value__217.index
                var t5412 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5411)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5410, t5412)
                var t5414 bool = byte__220 == 34
                if t5414 {
                    var t5422 *ref_int_x = value__217.index
                    var t5423 int
                    var inline12032 int = ref_get__Ref_3int(t5422)
                    t5423 = inline12032
                    var t5424 bool = segment__219 < t5423
                    if t5424 {
                        var t5425 string = value__217.input
                        var t5426 *ref_int_x = value__217.index
                        var t5427 int
                        var inline12021 int = ref_get__Ref_3int(t5426)
                        t5427 = inline12021
                        var t5428 string
                        var inline12019 string = string_byte_slice(t5425, segment__219, t5427)
                        t5428 = inline12019
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t5428)
                    } else {}
                    var t5416 *ref_int_x = value__217.index
                    var t5417 *ref_int_x = value__217.index
                    var t5418 int
                    var inline12030 int = ref_get__Ref_3int(t5417)
                    t5418 = inline12030
                    var t5419 int = t5418 + 1
                    ref_set__Ref_3int(t5416, t5419)
                    var t5420 string
                    var inline12023 *_goml_vec_uint8 = builder__218.values
                    var inline12024 Tuple2_4bool_6string = string_from_utf8(inline12023)
                    var inline12025 string = inline12024._1
                    t5420 = inline12025
                    var t5421 Result__string__string = Result__string__string{
                        _tag: 0,
                        _v0_0: t5420,
                    }
                    return t5421
                } else {
                    var t5431 bool = byte__220 == 92
                    if t5431 {
                        var t5486 *ref_int_x = value__217.index
                        var t5487 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5486)
                        var t5488 bool = segment__219 < t5487
                        if t5488 {
                            var t5489 string = value__217.input
                            var t5490 *ref_int_x = value__217.index
                            var t5491 int
                            var inline12036 int = ref_get__Ref_3int(t5490)
                            t5491 = inline12036
                            var t5492 string
                            var inline12034 string = string_byte_slice(t5489, segment__219, t5491)
                            t5492 = inline12034
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t5492)
                        } else {}
                        var t5433 *ref_int_x = value__217.index
                        var t5434 *ref_int_x = value__217.index
                        var t5435 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5434)
                        var t5436 int = t5435 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5433, t5436)
                        var t5479 *ref_int_x = value__217.index
                        var t5480 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5479)
                        var t5481 string = value__217.input
                        var t5482 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5481)
                        var t5483 bool = t5480 >= t5482
                        if t5483 {
                            var t5484 string
                            var inline12038 string = "incomplete escape"
                            var inline12039 string = "" + inline12038
                            var inline12040 string = inline12039 + " at byte "
                            var inline12041 *ref_int_x = value__217.index
                            var inline12042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12041)
                            var inline12043 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12042)
                            var inline12044 string = inline12040 + inline12043
                            t5484 = inline12044
                            var t5485 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5484,
                            }
                            return t5485
                        } else {
                            var t5438 string = value__217.input
                            var t5439 *ref_int_x = value__217.index
                            var t5440 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5439)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5438, t5440)
                            var t5441 *ref_int_x = value__217.index
                            var t5442 *ref_int_x = value__217.index
                            var t5443 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5442)
                            var t5444 int = t5443 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5441, t5444)
                            var t5448 bool = escape__221 == 34
                            if t5448 {
                                var inline12046 rune = 34
                                var inline12047 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12046)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline12047)
                                var t5446 *ref_int_x = value__217.index
                                var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                segment__219 = t5447
                                continue
                            } else {
                                var t5451 bool = escape__221 == 92
                                if t5451 {
                                    var inline12050 rune = 92
                                    var inline12051 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12050)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline12051)
                                    var t5446 *ref_int_x = value__217.index
                                    var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                    segment__219 = t5447
                                    continue
                                } else {
                                    var t5454 bool = escape__221 == 47
                                    if t5454 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t5446 *ref_int_x = value__217.index
                                        var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                        segment__219 = t5447
                                        continue
                                    } else {
                                        var t5457 bool = escape__221 == 98
                                        if t5457 {
                                            var mtmp770 Option__char = char_from_u32(8)
                                            switch mtmp770._tag {
                                            case 0:
                                                var t5446 *ref_int_x = value__217.index
                                                var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                segment__219 = t5447
                                                continue
                                            case 1:
                                                var x771 rune = mtmp770._v1_0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t5446 *ref_int_x = value__217.index
                                                var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                segment__219 = t5447
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t5461 bool = escape__221 == 102
                                            if t5461 {
                                                var mtmp772 Option__char = char_from_u32(12)
                                                switch mtmp772._tag {
                                                case 0:
                                                    var t5446 *ref_int_x = value__217.index
                                                    var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                    segment__219 = t5447
                                                    continue
                                                case 1:
                                                    var x773 rune = mtmp772._v1_0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t5446 *ref_int_x = value__217.index
                                                    var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                    segment__219 = t5447
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t5465 bool = escape__221 == 110
                                                if t5465 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t5446 *ref_int_x = value__217.index
                                                    var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                    segment__219 = t5447
                                                    continue
                                                } else {
                                                    var t5468 bool = escape__221 == 114
                                                    if t5468 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t5446 *ref_int_x = value__217.index
                                                        var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                        segment__219 = t5447
                                                        continue
                                                    } else {
                                                        var t5471 bool = escape__221 == 116
                                                        if t5471 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t5446 *ref_int_x = value__217.index
                                                            var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                            segment__219 = t5447
                                                            continue
                                                        } else {
                                                            var t5474 bool = escape__221 == 117
                                                            if t5474 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774._tag {
                                                                case 0:
                                                                    var t5446 *ref_int_x = value__217.index
                                                                    var t5447 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5446)
                                                                    segment__219 = t5447
                                                                    continue
                                                                case 1:
                                                                    var x776 string = mtmp774._v1_0
                                                                    var t5476 Result__string__string = Result__string__string{
                                                                        _tag: 1,
                                                                        _v1_0: x776,
                                                                    }
                                                                    return t5476
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t5477 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t5478 Result__string__string = Result__string__string{
                                                                    _tag: 1,
                                                                    _v1_0: t5477,
                                                                }
                                                                return t5478
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
                        var t5495 bool = byte__220 < 32
                        if t5495 {
                            var t5496 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t5497 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5496,
                            }
                            return t5497
                        } else {
                            var t5498 *ref_int_x = value__217.index
                            var t5499 *ref_int_x = value__217.index
                            var t5500 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5499)
                            var t5501 int = t5500 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5498, t5501)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop5404
            }
        }
        var t5402 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t5403 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5402,
        }
        return t5403
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t5530 *ref_int_x = value__225.index
    var start__226 int
    var inline12071 int = ref_get__Ref_3int(t5530)
    start__226 = inline12071
    Loop_loop5535:
    for {
        var t5543 *ref_int_x = value__225.index
        var t5544 int
        var inline12067 int = ref_get__Ref_3int(t5543)
        t5544 = inline12067
        var t5545 string = value__225.input
        var t5546 int
        var inline12065 int = _goml_runtime_core_string_len(t5545)
        t5546 = inline12065
        var t5547 bool = t5544 < t5546
        var jp5537 bool
        if t5547 {
            var t5548 string = value__225.input
            var t5549 *ref_int_x = value__225.index
            var t5550 int
            var inline12059 int = ref_get__Ref_3int(t5549)
            t5550 = inline12059
            var t5551 uint8
            var inline12057 uint8 = _goml_runtime_core_string_byte_get(t5548, t5550)
            t5551 = inline12057
            var inline12054 bool = t5551 >= 48
            if inline12054 {
                var inline12055 bool = t5551 <= 57
                jp5537 = inline12055
            } else {
                jp5537 = false
            }
        } else {
            jp5537 = false
        }
        if jp5537 {
            var t5538 *ref_int_x = value__225.index
            var t5539 *ref_int_x = value__225.index
            var t5540 int
            var inline12063 int = ref_get__Ref_3int(t5539)
            t5540 = inline12063
            var t5541 int = t5540 + 1
            ref_set__Ref_3int(t5538, t5541)
            continue
        } else {
            break Loop_loop5535
        }
    }
    var t5532 *ref_int_x = value__225.index
    var t5533 int
    var inline12069 int = ref_get__Ref_3int(t5532)
    t5533 = inline12069
    var t5534 bool = t5533 > start__226
    return t5534
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t5555 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5555)
    var t5676 string = value__227.input
    var t5677 *ref_int_x = value__227.index
    var t5678 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5677)
    var t5679 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5676, t5678)
    var t5680 bool = t5679 == 45
    if t5680 {
        var t5681 *ref_int_x = value__227.index
        var t5682 *ref_int_x = value__227.index
        var t5683 int
        var inline12075 int = ref_get__Ref_3int(t5682)
        t5683 = inline12075
        var t5684 int = t5683 + 1
        ref_set__Ref_3int(t5681, t5684)
    } else {}
    var t5639 *ref_int_x = value__227.index
    var t5640 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5639)
    var t5641 string = value__227.input
    var t5642 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5641)
    var t5643 bool = t5640 >= t5642
    if t5643 {
        var t5644 string
        var inline12077 string = "incomplete number"
        var inline12078 string = "" + inline12077
        var inline12079 string = inline12078 + " at byte "
        var inline12080 *ref_int_x = value__227.index
        var inline12081 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12080)
        var inline12082 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12081)
        var inline12083 string = inline12079 + inline12082
        t5644 = inline12083
        var t5645 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5644,
        }
        return t5645
    } else {
        var t5647 string = value__227.input
        var t5648 *ref_int_x = value__227.index
        var t5649 int
        var inline12118 int = ref_get__Ref_3int(t5648)
        t5649 = inline12118
        var t5650 uint8
        var inline12116 uint8 = _goml_runtime_core_string_byte_get(t5647, t5649)
        t5650 = inline12116
        var t5651 bool = t5650 == 48
        if t5651 {
            var t5652 *ref_int_x = value__227.index
            var t5653 *ref_int_x = value__227.index
            var t5654 int
            var inline12106 int = ref_get__Ref_3int(t5653)
            t5654 = inline12106
            var t5655 int = t5654 + 1
            ref_set__Ref_3int(t5652, t5655)
            var t5661 *ref_int_x = value__227.index
            var t5662 int
            var inline12102 int = ref_get__Ref_3int(t5661)
            t5662 = inline12102
            var t5663 string = value__227.input
            var t5664 int
            var inline12100 int = _goml_runtime_core_string_len(t5663)
            t5664 = inline12100
            var t5665 bool = t5662 < t5664
            var jp5658 bool
            if t5665 {
                var t5666 string = value__227.input
                var t5667 *ref_int_x = value__227.index
                var t5668 int
                var inline12090 int = ref_get__Ref_3int(t5667)
                t5668 = inline12090
                var t5669 uint8
                var inline12088 uint8 = _goml_runtime_core_string_byte_get(t5666, t5668)
                t5669 = inline12088
                var inline12085 bool = t5669 >= 48
                if inline12085 {
                    var inline12086 bool = t5669 <= 57
                    jp5658 = inline12086
                } else {
                    jp5658 = false
                }
            } else {
                jp5658 = false
            }
            if jp5658 {
                var t5659 string
                var inline12092 string = "invalid leading zero"
                var inline12093 string = "" + inline12092
                var inline12094 string = inline12093 + " at byte "
                var inline12095 *ref_int_x = value__227.index
                var inline12096 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12095)
                var inline12097 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12096)
                var inline12098 string = inline12094 + inline12097
                t5659 = inline12098
                var t5660 Result__string__string = Result__string__string{
                    _tag: 1,
                    _v1_0: t5659,
                }
                return t5660
            } else {
                var t5629 *ref_int_x = value__227.index
                var t5630 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5629)
                var t5631 string = value__227.input
                var t5632 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5631)
                var t5633 bool = t5630 < t5632
                var jp5619 bool
                if t5633 {
                    var t5634 string = value__227.input
                    var t5635 *ref_int_x = value__227.index
                    var t5636 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5635)
                    var t5637 uint8
                    var inline12120 uint8 = _goml_runtime_core_string_byte_get(t5634, t5636)
                    t5637 = inline12120
                    var t5638 bool = t5637 == 46
                    jp5619 = t5638
                } else {
                    jp5619 = false
                }
                if jp5619 {
                    var t5620 *ref_int_x = value__227.index
                    var t5621 *ref_int_x = value__227.index
                    var t5622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5621)
                    var t5623 int = t5622 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5620, t5623)
                    var t5625 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5626 bool = !t5625
                    if t5626 {
                        var t5627 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5628 Result__string__string = Result__string__string{
                            _tag: 1,
                            _v1_0: t5627,
                        }
                        return t5628
                    } else {
                        var t5601 *ref_int_x = value__227.index
                        var t5602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5601)
                        var t5603 string = value__227.input
                        var t5604 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5603)
                        var t5605 bool = t5602 < t5604
                        var jp5566 bool
                        if t5605 {
                            var t5608 string = value__227.input
                            var t5609 *ref_int_x = value__227.index
                            var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                            var t5611 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5608, t5610)
                            var t5612 bool = t5611 == 101
                            if t5612 {
                                jp5566 = true
                            } else {
                                var t5613 string = value__227.input
                                var t5614 *ref_int_x = value__227.index
                                var t5615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5614)
                                var t5616 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5613, t5615)
                                var t5617 bool = t5616 == 69
                                jp5566 = t5617
                            }
                        } else {
                            jp5566 = false
                        }
                        if jp5566 {
                            var t5567 *ref_int_x = value__227.index
                            var t5568 *ref_int_x = value__227.index
                            var t5569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5568)
                            var t5570 int = t5569 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5567, t5570)
                            var t5584 *ref_int_x = value__227.index
                            var t5585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5584)
                            var t5586 string = value__227.input
                            var t5587 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5586)
                            var t5588 bool = t5585 < t5587
                            var jp5578 bool
                            if t5588 {
                                var t5591 string = value__227.input
                                var t5592 *ref_int_x = value__227.index
                                var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                                var t5594 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5591, t5593)
                                var t5595 bool = t5594 == 43
                                if t5595 {
                                    jp5578 = true
                                } else {
                                    var t5596 string = value__227.input
                                    var t5597 *ref_int_x = value__227.index
                                    var t5598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5597)
                                    var t5599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5596, t5598)
                                    var t5600 bool = t5599 == 45
                                    jp5578 = t5600
                                }
                            } else {
                                jp5578 = false
                            }
                            if jp5578 {
                                var t5579 *ref_int_x = value__227.index
                                var t5580 *ref_int_x = value__227.index
                                var t5581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5580)
                                var t5582 int = t5581 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5579, t5582)
                            } else {}
                            var t5573 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t5574 bool = !t5573
                            if t5574 {
                                var t5575 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t5576 Result__string__string = Result__string__string{
                                    _tag: 1,
                                    _v1_0: t5575,
                                }
                                return t5576
                            } else {
                                var t5560 string = value__227.input
                                var t5561 *ref_int_x = value__227.index
                                var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                                var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                                var t5564 Result__string__string = Result__string__string{
                                    _tag: 0,
                                    _v0_0: t5563,
                                }
                                return t5564
                            }
                        } else {
                            var t5560 string = value__227.input
                            var t5561 *ref_int_x = value__227.index
                            var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                            var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                            var t5564 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5563,
                            }
                            return t5564
                        }
                    }
                } else {
                    var t5601 *ref_int_x = value__227.index
                    var t5602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5601)
                    var t5603 string = value__227.input
                    var t5604 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5603)
                    var t5605 bool = t5602 < t5604
                    var jp5566 bool
                    if t5605 {
                        var t5608 string = value__227.input
                        var t5609 *ref_int_x = value__227.index
                        var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                        var t5611 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5608, t5610)
                        var t5612 bool = t5611 == 101
                        if t5612 {
                            jp5566 = true
                        } else {
                            var t5613 string = value__227.input
                            var t5614 *ref_int_x = value__227.index
                            var t5615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5614)
                            var t5616 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5613, t5615)
                            var t5617 bool = t5616 == 69
                            jp5566 = t5617
                        }
                    } else {
                        jp5566 = false
                    }
                    if jp5566 {
                        var t5567 *ref_int_x = value__227.index
                        var t5568 *ref_int_x = value__227.index
                        var t5569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5568)
                        var t5570 int = t5569 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5567, t5570)
                        var t5584 *ref_int_x = value__227.index
                        var t5585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5584)
                        var t5586 string = value__227.input
                        var t5587 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5586)
                        var t5588 bool = t5585 < t5587
                        var jp5578 bool
                        if t5588 {
                            var t5591 string = value__227.input
                            var t5592 *ref_int_x = value__227.index
                            var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                            var t5594 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5591, t5593)
                            var t5595 bool = t5594 == 43
                            if t5595 {
                                jp5578 = true
                            } else {
                                var t5596 string = value__227.input
                                var t5597 *ref_int_x = value__227.index
                                var t5598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5597)
                                var t5599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5596, t5598)
                                var t5600 bool = t5599 == 45
                                jp5578 = t5600
                            }
                        } else {
                            jp5578 = false
                        }
                        if jp5578 {
                            var t5579 *ref_int_x = value__227.index
                            var t5580 *ref_int_x = value__227.index
                            var t5581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5580)
                            var t5582 int = t5581 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5579, t5582)
                        } else {}
                        var t5573 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t5574 bool = !t5573
                        if t5574 {
                            var t5575 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t5576 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5575,
                            }
                            return t5576
                        } else {
                            var t5560 string = value__227.input
                            var t5561 *ref_int_x = value__227.index
                            var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                            var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                            var t5564 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5563,
                            }
                            return t5564
                        }
                    } else {
                        var t5560 string = value__227.input
                        var t5561 *ref_int_x = value__227.index
                        var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                        var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                        var t5564 Result__string__string = Result__string__string{
                            _tag: 0,
                            _v0_0: t5563,
                        }
                        return t5564
                    }
                }
            }
        } else {
            var t5672 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5673 bool = !t5672
            if t5673 {
                var t5674 string
                var inline12108 string = "expected number"
                var inline12109 string = "" + inline12108
                var inline12110 string = inline12109 + " at byte "
                var inline12111 *ref_int_x = value__227.index
                var inline12112 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12111)
                var inline12113 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12112)
                var inline12114 string = inline12110 + inline12113
                t5674 = inline12114
                var t5675 Result__string__string = Result__string__string{
                    _tag: 1,
                    _v1_0: t5674,
                }
                return t5675
            } else {
                var t5629 *ref_int_x = value__227.index
                var t5630 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5629)
                var t5631 string = value__227.input
                var t5632 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5631)
                var t5633 bool = t5630 < t5632
                var jp5619 bool
                if t5633 {
                    var t5634 string = value__227.input
                    var t5635 *ref_int_x = value__227.index
                    var t5636 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5635)
                    var t5637 uint8
                    var inline12120 uint8 = _goml_runtime_core_string_byte_get(t5634, t5636)
                    t5637 = inline12120
                    var t5638 bool = t5637 == 46
                    jp5619 = t5638
                } else {
                    jp5619 = false
                }
                if jp5619 {
                    var t5620 *ref_int_x = value__227.index
                    var t5621 *ref_int_x = value__227.index
                    var t5622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5621)
                    var t5623 int = t5622 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5620, t5623)
                    var t5625 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5626 bool = !t5625
                    if t5626 {
                        var t5627 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5628 Result__string__string = Result__string__string{
                            _tag: 1,
                            _v1_0: t5627,
                        }
                        return t5628
                    } else {
                        var t5601 *ref_int_x = value__227.index
                        var t5602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5601)
                        var t5603 string = value__227.input
                        var t5604 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5603)
                        var t5605 bool = t5602 < t5604
                        var jp5566 bool
                        if t5605 {
                            var t5608 string = value__227.input
                            var t5609 *ref_int_x = value__227.index
                            var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                            var t5611 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5608, t5610)
                            var t5612 bool = t5611 == 101
                            if t5612 {
                                jp5566 = true
                            } else {
                                var t5613 string = value__227.input
                                var t5614 *ref_int_x = value__227.index
                                var t5615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5614)
                                var t5616 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5613, t5615)
                                var t5617 bool = t5616 == 69
                                jp5566 = t5617
                            }
                        } else {
                            jp5566 = false
                        }
                        if jp5566 {
                            var t5567 *ref_int_x = value__227.index
                            var t5568 *ref_int_x = value__227.index
                            var t5569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5568)
                            var t5570 int = t5569 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5567, t5570)
                            var t5584 *ref_int_x = value__227.index
                            var t5585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5584)
                            var t5586 string = value__227.input
                            var t5587 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5586)
                            var t5588 bool = t5585 < t5587
                            var jp5578 bool
                            if t5588 {
                                var t5591 string = value__227.input
                                var t5592 *ref_int_x = value__227.index
                                var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                                var t5594 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5591, t5593)
                                var t5595 bool = t5594 == 43
                                if t5595 {
                                    jp5578 = true
                                } else {
                                    var t5596 string = value__227.input
                                    var t5597 *ref_int_x = value__227.index
                                    var t5598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5597)
                                    var t5599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5596, t5598)
                                    var t5600 bool = t5599 == 45
                                    jp5578 = t5600
                                }
                            } else {
                                jp5578 = false
                            }
                            if jp5578 {
                                var t5579 *ref_int_x = value__227.index
                                var t5580 *ref_int_x = value__227.index
                                var t5581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5580)
                                var t5582 int = t5581 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5579, t5582)
                            } else {}
                            var t5573 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t5574 bool = !t5573
                            if t5574 {
                                var t5575 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t5576 Result__string__string = Result__string__string{
                                    _tag: 1,
                                    _v1_0: t5575,
                                }
                                return t5576
                            } else {
                                var t5560 string = value__227.input
                                var t5561 *ref_int_x = value__227.index
                                var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                                var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                                var t5564 Result__string__string = Result__string__string{
                                    _tag: 0,
                                    _v0_0: t5563,
                                }
                                return t5564
                            }
                        } else {
                            var t5560 string = value__227.input
                            var t5561 *ref_int_x = value__227.index
                            var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                            var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                            var t5564 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5563,
                            }
                            return t5564
                        }
                    }
                } else {
                    var t5601 *ref_int_x = value__227.index
                    var t5602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5601)
                    var t5603 string = value__227.input
                    var t5604 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5603)
                    var t5605 bool = t5602 < t5604
                    var jp5566 bool
                    if t5605 {
                        var t5608 string = value__227.input
                        var t5609 *ref_int_x = value__227.index
                        var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                        var t5611 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5608, t5610)
                        var t5612 bool = t5611 == 101
                        if t5612 {
                            jp5566 = true
                        } else {
                            var t5613 string = value__227.input
                            var t5614 *ref_int_x = value__227.index
                            var t5615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5614)
                            var t5616 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5613, t5615)
                            var t5617 bool = t5616 == 69
                            jp5566 = t5617
                        }
                    } else {
                        jp5566 = false
                    }
                    if jp5566 {
                        var t5567 *ref_int_x = value__227.index
                        var t5568 *ref_int_x = value__227.index
                        var t5569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5568)
                        var t5570 int = t5569 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5567, t5570)
                        var t5584 *ref_int_x = value__227.index
                        var t5585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5584)
                        var t5586 string = value__227.input
                        var t5587 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5586)
                        var t5588 bool = t5585 < t5587
                        var jp5578 bool
                        if t5588 {
                            var t5591 string = value__227.input
                            var t5592 *ref_int_x = value__227.index
                            var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                            var t5594 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5591, t5593)
                            var t5595 bool = t5594 == 43
                            if t5595 {
                                jp5578 = true
                            } else {
                                var t5596 string = value__227.input
                                var t5597 *ref_int_x = value__227.index
                                var t5598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5597)
                                var t5599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5596, t5598)
                                var t5600 bool = t5599 == 45
                                jp5578 = t5600
                            }
                        } else {
                            jp5578 = false
                        }
                        if jp5578 {
                            var t5579 *ref_int_x = value__227.index
                            var t5580 *ref_int_x = value__227.index
                            var t5581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5580)
                            var t5582 int = t5581 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5579, t5582)
                        } else {}
                        var t5573 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t5574 bool = !t5573
                        if t5574 {
                            var t5575 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t5576 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5575,
                            }
                            return t5576
                        } else {
                            var t5560 string = value__227.input
                            var t5561 *ref_int_x = value__227.index
                            var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                            var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                            var t5564 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5563,
                            }
                            return t5564
                        }
                    } else {
                        var t5560 string = value__227.input
                        var t5561 *ref_int_x = value__227.index
                        var t5562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5561)
                        var t5563 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5560, start__228, t5562)
                        var t5564 Result__string__string = Result__string__string{
                            _tag: 0,
                            _v0_0: t5563,
                        }
                        return t5564
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5707 *ref_int_x = value__230.index
    var t5708 int
    var inline12148 int = ref_get__Ref_3int(t5707)
    t5708 = inline12148
    var t5709 int
    var inline12146 int = _goml_runtime_core_string_len(expected__231)
    t5709 = inline12146
    var t5710 int = t5708 + t5709
    var t5711 string = value__230.input
    var t5712 int
    var inline12144 int = _goml_runtime_core_string_len(t5711)
    t5712 = inline12144
    var t5713 bool = t5710 <= t5712
    var jp5698 bool
    if t5713 {
        var t5714 string = value__230.input
        var t5715 *ref_int_x = value__230.index
        var t5716 int
        var inline12128 int = ref_get__Ref_3int(t5715)
        t5716 = inline12128
        var t5717 *ref_int_x = value__230.index
        var t5718 int
        var inline12126 int = ref_get__Ref_3int(t5717)
        t5718 = inline12126
        var t5719 int
        var inline12124 int = _goml_runtime_core_string_len(expected__231)
        t5719 = inline12124
        var t5720 int = t5718 + t5719
        var t5721 string
        var inline12122 string = string_byte_slice(t5714, t5716, t5720)
        t5721 = inline12122
        var t5722 bool = t5721 == expected__231
        jp5698 = t5722
    } else {
        jp5698 = false
    }
    if jp5698 {
        var t5699 *ref_int_x = value__230.index
        var t5700 *ref_int_x = value__230.index
        var t5701 int
        var inline12134 int = ref_get__Ref_3int(t5700)
        t5701 = inline12134
        var t5702 int
        var inline12132 int = _goml_runtime_core_string_len(expected__231)
        t5702 = inline12132
        var t5703 int = t5701 + t5702
        ref_set__Ref_3int(t5699, t5703)
        var t5704 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: result__232,
        }
        return t5704
    } else {
        var t5705 string
        var inline12136 string = "invalid literal"
        var inline12137 string = "" + inline12136
        var inline12138 string = inline12137 + " at byte "
        var inline12139 *ref_int_x = value__230.index
        var inline12140 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12139)
        var inline12141 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12140)
        var inline12142 string = inline12138 + inline12141
        t5705 = inline12142
        var t5706 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5705,
        }
        return t5706
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5725 *ref_int_x = value__233.index
    var t5726 *ref_int_x = value__233.index
    var t5727 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5726)
    var t5728 int = t5727 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5725, t5728)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var result__234 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t5783 *ref_int_x = value__233.index
    var t5784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5783)
    var t5785 string = value__233.input
    var t5786 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5785)
    var t5787 bool = t5784 < t5786
    var jp5776 bool
    if t5787 {
        var t5788 string = value__233.input
        var t5789 *ref_int_x = value__233.index
        var t5790 int
        var inline12152 int = ref_get__Ref_3int(t5789)
        t5790 = inline12152
        var t5791 uint8
        var inline12150 uint8 = _goml_runtime_core_string_byte_get(t5788, t5790)
        t5791 = inline12150
        var t5792 bool = t5791 == 93
        jp5776 = t5792
    } else {
        jp5776 = false
    }
    if jp5776 {
        var t5777 *ref_int_x = value__233.index
        var t5778 *ref_int_x = value__233.index
        var t5779 int
        var inline12156 int = ref_get__Ref_3int(t5778)
        t5779 = inline12156
        var t5780 int = t5779 + 1
        ref_set__Ref_3int(t5777, t5780)
        var t5781 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: result__234,
        }
        var t5782 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: t5781,
        }
        return t5782
    } else {
        Loop_loop5733:
        for {
            var t5734 *ref_int_x = value__233.index
            var t5735 int
            var inline12198 int = ref_get__Ref_3int(t5734)
            t5735 = inline12198
            var t5736 string = value__233.input
            var t5737 int
            var inline12196 int = _goml_runtime_core_string_len(t5736)
            t5737 = inline12196
            var t5738 bool = t5735 < t5737
            if t5738 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5740 _goml_m_std_p_json_p_Value
                switch mtmp797._tag {
                case 0:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797._v0_0
                    jp5740 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(result__234, jp5740)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5742 *ref_int_x = value__233.index
                    var t5743 int
                    var inline12192 int = ref_get__Ref_3int(t5742)
                    t5743 = inline12192
                    var t5744 string = value__233.input
                    var t5745 int
                    var inline12190 int = _goml_runtime_core_string_len(t5744)
                    t5745 = inline12190
                    var t5746 bool = t5743 >= t5745
                    if t5746 {
                        var t5747 string
                        var inline12158 string = "unterminated array"
                        var inline12159 string = "" + inline12158
                        var inline12160 string = inline12159 + " at byte "
                        var inline12161 *ref_int_x = value__233.index
                        var inline12162 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12161)
                        var inline12163 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12162)
                        var inline12164 string = inline12160 + inline12163
                        t5747 = inline12164
                        var t5748 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                            _tag: 1,
                            _v1_0: t5747,
                        }
                        return t5748
                    } else {
                        var t5750 string = value__233.input
                        var t5751 *ref_int_x = value__233.index
                        var t5752 int
                        var inline12188 int = ref_get__Ref_3int(t5751)
                        t5752 = inline12188
                        var t5753 uint8
                        var inline12186 uint8 = _goml_runtime_core_string_byte_get(t5750, t5752)
                        t5753 = inline12186
                        var t5754 bool = t5753 == 93
                        if t5754 {
                            var t5755 *ref_int_x = value__233.index
                            var t5756 *ref_int_x = value__233.index
                            var t5757 int
                            var inline12168 int = ref_get__Ref_3int(t5756)
                            t5757 = inline12168
                            var t5758 int = t5757 + 1
                            ref_set__Ref_3int(t5755, t5758)
                            var t5759 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: result__234,
                            }
                            var t5760 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                _tag: 0,
                                _v0_0: t5759,
                            }
                            return t5760
                        } else {
                            var t5762 string = value__233.input
                            var t5763 *ref_int_x = value__233.index
                            var t5764 int
                            var inline12184 int = ref_get__Ref_3int(t5763)
                            t5764 = inline12184
                            var t5765 uint8
                            var inline12182 uint8 = _goml_runtime_core_string_byte_get(t5762, t5764)
                            t5765 = inline12182
                            var t5766 bool = t5765 == 44
                            if t5766 {
                                var t5767 *ref_int_x = value__233.index
                                var t5768 *ref_int_x = value__233.index
                                var t5769 int
                                var inline12172 int = ref_get__Ref_3int(t5768)
                                t5769 = inline12172
                                var t5770 int = t5769 + 1
                                ref_set__Ref_3int(t5767, t5770)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5772 string
                                var inline12174 string = "expected array separator"
                                var inline12175 string = "" + inline12174
                                var inline12176 string = inline12175 + " at byte "
                                var inline12177 *ref_int_x = value__233.index
                                var inline12178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12177)
                                var inline12179 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12178)
                                var inline12180 string = inline12176 + inline12179
                                t5772 = inline12180
                                var t5773 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                    _tag: 1,
                                    _v1_0: t5772,
                                }
                                return t5773
                            }
                        }
                    }
                case 1:
                    var x799 string = mtmp797._v1_0
                    var t5774 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: x799,
                    }
                    return t5774
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5733
            }
        }
        var t5731 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5732 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5731,
        }
        return t5732
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5795 *ref_int_x = value__236.index
    var t5796 *ref_int_x = value__236.index
    var t5797 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5796)
    var t5798 int = t5797 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5795, t5798)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var result__237 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5877 *ref_int_x = value__236.index
    var t5878 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5877)
    var t5879 string = value__236.input
    var t5880 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5879)
    var t5881 bool = t5878 < t5880
    var jp5870 bool
    if t5881 {
        var t5882 string = value__236.input
        var t5883 *ref_int_x = value__236.index
        var t5884 int
        var inline12202 int = ref_get__Ref_3int(t5883)
        t5884 = inline12202
        var t5885 uint8
        var inline12200 uint8 = _goml_runtime_core_string_byte_get(t5882, t5884)
        t5885 = inline12200
        var t5886 bool = t5885 == 125
        jp5870 = t5886
    } else {
        jp5870 = false
    }
    if jp5870 {
        var t5871 *ref_int_x = value__236.index
        var t5872 *ref_int_x = value__236.index
        var t5873 int
        var inline12206 int = ref_get__Ref_3int(t5872)
        t5873 = inline12206
        var t5874 int = t5873 + 1
        ref_set__Ref_3int(t5871, t5874)
        var t5875 _goml_m_std_p_json_p_Value = Object{
            _0: result__237,
        }
        var t5876 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: t5875,
        }
        return t5876
    } else {
        Loop_loop5803:
        for {
            var t5804 *ref_int_x = value__236.index
            var t5805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5804)
            var t5806 string = value__236.input
            var t5807 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5806)
            var t5808 bool = t5805 < t5807
            if t5808 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5810 string
                switch mtmp809._tag {
                case 0:
                    var x810 string = mtmp809._v0_0
                    jp5810 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5858 *ref_int_x = value__236.index
                    var t5859 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5858)
                    var t5860 string = value__236.input
                    var t5861 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5860)
                    var t5862 bool = t5859 >= t5861
                    var jp5850 bool
                    if t5862 {
                        jp5850 = true
                    } else {
                        var t5863 string = value__236.input
                        var t5864 *ref_int_x = value__236.index
                        var t5865 int
                        var inline12210 int = ref_get__Ref_3int(t5864)
                        t5865 = inline12210
                        var t5866 uint8
                        var inline12208 uint8 = _goml_runtime_core_string_byte_get(t5863, t5865)
                        t5866 = inline12208
                        var t5867 bool = t5866 != 58
                        jp5850 = t5867
                    }
                    if jp5850 {
                        var t5851 string
                        var inline12212 string = "expected object colon"
                        var inline12213 string = "" + inline12212
                        var inline12214 string = inline12213 + " at byte "
                        var inline12215 *ref_int_x = value__236.index
                        var inline12216 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12215)
                        var inline12217 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12216)
                        var inline12218 string = inline12214 + inline12217
                        t5851 = inline12218
                        var t5852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                            _tag: 1,
                            _v1_0: t5851,
                        }
                        return t5852
                    } else {
                        var t5853 *ref_int_x = value__236.index
                        var t5854 *ref_int_x = value__236.index
                        var t5855 int
                        var inline12222 int = ref_get__Ref_3int(t5854)
                        t5855 = inline12222
                        var t5856 int = t5855 + 1
                        ref_set__Ref_3int(t5853, t5856)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5813 _goml_m_std_p_json_p_Value
                        switch mtmp815._tag {
                        case 0:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815._v0_0
                            jp5813 = x816
                            var t5814 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5810,
                                _1: jp5813,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__237, t5814)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5816 *ref_int_x = value__236.index
                            var t5817 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5816)
                            var t5818 string = value__236.input
                            var t5819 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5818)
                            var t5820 bool = t5817 >= t5819
                            if t5820 {
                                var t5821 string
                                var inline12224 string = "unterminated object"
                                var inline12225 string = "" + inline12224
                                var inline12226 string = inline12225 + " at byte "
                                var inline12227 *ref_int_x = value__236.index
                                var inline12228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12227)
                                var inline12229 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12228)
                                var inline12230 string = inline12226 + inline12229
                                t5821 = inline12230
                                var t5822 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                    _tag: 1,
                                    _v1_0: t5821,
                                }
                                return t5822
                            } else {
                                var t5824 string = value__236.input
                                var t5825 *ref_int_x = value__236.index
                                var t5826 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5825)
                                var t5827 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5824, t5826)
                                var t5828 bool = t5827 == 125
                                if t5828 {
                                    var t5829 *ref_int_x = value__236.index
                                    var t5830 *ref_int_x = value__236.index
                                    var t5831 int
                                    var inline12234 int = ref_get__Ref_3int(t5830)
                                    t5831 = inline12234
                                    var t5832 int = t5831 + 1
                                    ref_set__Ref_3int(t5829, t5832)
                                    var t5833 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__237,
                                    }
                                    var t5834 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                        _tag: 0,
                                        _v0_0: t5833,
                                    }
                                    return t5834
                                } else {
                                    var t5836 string = value__236.input
                                    var t5837 *ref_int_x = value__236.index
                                    var t5838 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5837)
                                    var t5839 uint8
                                    var inline12248 uint8 = _goml_runtime_core_string_byte_get(t5836, t5838)
                                    t5839 = inline12248
                                    var t5840 bool = t5839 == 44
                                    if t5840 {
                                        var t5841 *ref_int_x = value__236.index
                                        var t5842 *ref_int_x = value__236.index
                                        var t5843 int
                                        var inline12238 int = ref_get__Ref_3int(t5842)
                                        t5843 = inline12238
                                        var t5844 int = t5843 + 1
                                        ref_set__Ref_3int(t5841, t5844)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5846 string
                                        var inline12240 string = "expected object separator"
                                        var inline12241 string = "" + inline12240
                                        var inline12242 string = inline12241 + " at byte "
                                        var inline12243 *ref_int_x = value__236.index
                                        var inline12244 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12243)
                                        var inline12245 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12244)
                                        var inline12246 string = inline12242 + inline12245
                                        t5846 = inline12246
                                        var t5847 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                            _tag: 1,
                                            _v1_0: t5846,
                                        }
                                        return t5847
                                    }
                                }
                            }
                        case 1:
                            var x817 string = mtmp815._v1_0
                            var t5848 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                _tag: 1,
                                _v1_0: x817,
                            }
                            return t5848
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x811 string = mtmp809._v1_0
                    var t5868 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: x811,
                    }
                    return t5868
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5803
            }
        }
        var t5801 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5802 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5801,
        }
        return t5802
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5891 *ref_int_x = value__240.index
    var t5892 int
    var inline12286 int = ref_get__Ref_3int(t5891)
    t5892 = inline12286
    var t5893 string = value__240.input
    var t5894 int
    var inline12284 int = _goml_runtime_core_string_len(t5893)
    t5894 = inline12284
    var t5895 bool = t5892 >= t5894
    if t5895 {
        var t5896 string
        var inline12250 string = "expected JSON value"
        var inline12251 string = "" + inline12250
        var inline12252 string = inline12251 + " at byte "
        var inline12253 *ref_int_x = value__240.index
        var inline12254 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12253)
        var inline12255 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12254)
        var inline12256 string = inline12252 + inline12255
        t5896 = inline12256
        var t5897 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5896,
        }
        return t5897
    } else {
        var t5898 string = value__240.input
        var t5899 *ref_int_x = value__240.index
        var t5900 int
        var inline12282 int = ref_get__Ref_3int(t5899)
        t5900 = inline12282
        var mtmp824 uint8
        var inline12280 uint8 = _goml_runtime_core_string_byte_get(t5898, t5900)
        mtmp824 = inline12280
        switch mtmp824 {
        case 123:
            var t5903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5903
        case 91:
            var t5904 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5904
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825._tag {
            case 0:
                var x826 string = mtmp825._v0_0
                var t5907 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5908 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 0,
                    _v0_0: t5907,
                }
                return t5908
            case 1:
                var x827 string = mtmp825._v1_0
                var t5909 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 1,
                    _v1_0: x827,
                }
                return t5909
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5910 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5911 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5910)
            return t5911
        case 102:
            var t5912 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5913 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5912)
            return t5913
        case 110:
            var t5914 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5914
        default:
            var t5922 bool = mtmp824 == 45
            var jp5918 bool
            if t5922 {
                jp5918 = true
            } else {
                var inline12258 bool = mtmp824 >= 48
                if inline12258 {
                    var inline12259 bool = mtmp824 <= 57
                    jp5918 = inline12259
                } else {
                    jp5918 = false
                }
            }
            if jp5918 {
                var inline12261 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline12263 string
                switch inline12261._tag {
                case 0:
                    var inline12266 string = inline12261._v0_0
                    inline12263 = inline12266
                    var inline12264 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline12263,
                    }
                    var inline12265 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 0,
                        _v0_0: inline12264,
                    }
                    return inline12265
                case 1:
                    var inline12268 string = inline12261._v1_0
                    var inline12270 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: inline12268,
                    }
                    return inline12270
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5920 string
                var inline12272 string = "unexpected JSON token"
                var inline12273 string = "" + inline12272
                var inline12274 string = inline12273 + " at byte "
                var inline12275 *ref_int_x = value__240.index
                var inline12276 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12275)
                var inline12277 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12276)
                var inline12278 string = inline12274 + inline12277
                t5920 = inline12278
                var t5921 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 1,
                    _v1_0: t5920,
                }
                return t5921
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline12300 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var inline12301 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline12300,
    }
    parser__245 = inline12301
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5927 _goml_m_std_p_json_p_Value
    switch mtmp828._tag {
    case 0:
        var x829 _goml_m_std_p_json_p_Value = mtmp828._v0_0
        jp5927 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5930 *ref_int_x = parser__245.index
        var t5931 int
        var inline12298 int = ref_get__Ref_3int(t5930)
        t5931 = inline12298
        var t5932 int
        var inline12296 int = _goml_runtime_core_string_len(input__244)
        t5932 = inline12296
        var t5933 bool = t5931 == t5932
        if t5933 {
            var t5934 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                _tag: 0,
                _v0_0: jp5927,
            }
            return t5934
        } else {
            var t5935 string
            var inline12288 string = "trailing JSON data"
            var inline12289 string = "" + inline12288
            var inline12290 string = inline12289 + " at byte "
            var inline12291 *ref_int_x = parser__245.index
            var inline12292 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12291)
            var inline12293 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12292)
            var inline12294 string = inline12290 + inline12293
            t5935 = inline12294
            var t5936 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                _tag: 1,
                _v1_0: t5935,
            }
            return t5936
        }
    case 1:
        var x830 string = mtmp828._v1_0
        var t5937 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: x830,
        }
        return t5937
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline12334 rune = 34
    var inline12335 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12334)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12335)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline12332 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline12332
    Loop_loop5951:
    for {
        var t5952 bool = for_index833 < for_limit834
        if t5952 {
            var for_item835 int = for_index833
            var t5953 int = for_index833 + 1
            for_index833 = t5953
            var byte__252 uint8
            var inline12320 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline12320
            var t6006 bool = byte__252 == 34
            var jp6004 bool
            if t6006 {
                jp6004 = true
            } else {
                var t6007 bool = byte__252 == 92
                jp6004 = t6007
            }
            var jp6001 bool
            if jp6004 {
                jp6001 = true
            } else {
                var t6005 bool = byte__252 == 8
                jp6001 = t6005
            }
            var jp5998 bool
            if jp6001 {
                jp5998 = true
            } else {
                var t6002 bool = byte__252 == 9
                jp5998 = t6002
            }
            var jp5995 bool
            if jp5998 {
                jp5995 = true
            } else {
                var t5999 bool = byte__252 == 10
                jp5995 = t5999
            }
            var jp5992 bool
            if jp5995 {
                jp5992 = true
            } else {
                var t5996 bool = byte__252 == 12
                jp5992 = t5996
            }
            var jp5989 bool
            if jp5992 {
                jp5989 = true
            } else {
                var t5993 bool = byte__252 == 13
                jp5989 = t5993
            }
            var jp5956 bool
            if jp5989 {
                jp5956 = true
            } else {
                var t5990 bool = byte__252 < 32
                jp5956 = t5990
            }
            if jp5956 {
                var t5985 bool = start__250 < for_item835
                if t5985 {
                    var t5986 string
                    var inline12306 string = string_byte_slice(value__249, start__250, for_item835)
                    t5986 = inline12306
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5986)
                } else {}
                var t5960 bool = byte__252 == 34
                if t5960 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5963 bool = byte__252 == 92
                    if t5963 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5966 bool = byte__252 == 8
                        if t5966 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5969 bool = byte__252 == 9
                            if t5969 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5972 bool = byte__252 == 10
                                if t5972 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5975 bool = byte__252 == 12
                                    if t5975 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5978 bool = byte__252 == 13
                                        if t5978 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5980 uint8 = byte__252 / 16
                                            var t5981 rune
                                            var inline12317 int = int(uint8(t5980))
                                            var inline12318 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline12317)
                                            t5981 = inline12318
                                            var inline12314 string = _goml_m_inherent_i_char_i_char_i_to__string(t5981)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12314)
                                            var t5982_rhs uint8 = 16
                                            var t5982 uint8 = byte__252 % t5982_rhs
                                            var t5983 rune
                                            var inline12311 int = int(uint8(t5982))
                                            var inline12312 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline12311)
                                            t5983 = inline12312
                                            var inline12308 string = _goml_m_inherent_i_char_i_char_i_to__string(t5983)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12308)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5959 int = for_item835 + 1
                start__250 = t5959
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5951
        }
    }
    var t5946 int
    var inline12330 int = _goml_runtime_core_string_len(value__249)
    t5946 = inline12330
    var t5947 bool = start__250 < t5946
    if t5947 {
        var t5948 int
        var inline12324 int = _goml_runtime_core_string_len(value__249)
        t5948 = inline12324
        var t5949 string
        var inline12322 string = string_byte_slice(value__249, start__250, t5948)
        t5949 = inline12322
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5949)
    } else {}
    var inline12326 rune = 34
    var inline12327 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12326)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12327)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline12350 rune = 123
        var inline12351 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12350)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12351)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop6012:
        for {
            var t6013 bool = for_index852 < for_limit851
            if t6013 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t6014 int = for_index852 + 1
                for_index852 = t6014
                var t6020 bool = index__256 > 0
                if t6020 {
                    var inline12338 rune = 44
                    var inline12339 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12338)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12339)
                } else {}
                var t6016 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t6016)
                var inline12342 rune = 58
                var inline12343 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12342)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12343)
                var t6017 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t6017)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t6018 int = compound_old859 + compound_value860
                index__256 = t6018
                continue
            } else {
                break Loop_loop6012
            }
        }
        var inline12346 rune = 125
        var inline12347 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12346)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12347)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline12362 rune = 91
        var inline12363 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12362)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12363)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop6024:
        for {
            var t6025 bool = for_index866 < for_limit865
            if t6025 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t6026 int = for_index866 + 1
                for_index866 = t6026
                var t6030 bool = index__259 > 0
                if t6030 {
                    var inline12354 rune = 44
                    var inline12355 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12354)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12355)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t6028 int = compound_old871 + compound_value872
                index__259 = t6028
                continue
            } else {
                break Loop_loop6024
            }
        }
        var inline12358 rune = 93
        var inline12359 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12358)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12359)
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
        var jp6035 string
        if x848 {
            jp6035 = "true"
        } else {
            jp6035 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp6035)
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
    var inline12371 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__u8()
    var inline12372 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline12371,
    }
    builder__265 = inline12372
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline12366 *_goml_vec_uint8 = builder__265.values
    var inline12367 Tuple2_4bool_6string = string_from_utf8(inline12366)
    var inline12368 string = inline12367._1
    return inline12368
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop6046:
        for {
            var t6047 bool = for_index883 < for_limit882
            if t6047 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t6048 int = for_index883 + 1
                for_index883 = t6048
                var t6050 string = for_item884._0
                var t6051 bool = t6050 == name__267
                if t6051 {
                    var t6052 _goml_m_std_p_json_p_Value = for_item884._1
                    var t6053 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value{
                        _tag: 1,
                        _v1_0: t6052,
                    }
                    return t6053
                } else {
                    continue
                }
            } else {
                break Loop_loop6046
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
    var t6063 int
    var inline12383 int = _goml_runtime_core_string_len(value__272)
    t6063 = inline12383
    var t6064 bool = t6063 == 0
    if t6064 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var t6065 uint8
        var inline12380 int = 0
        var inline12381 uint8 = _goml_runtime_core_string_byte_get(value__272, inline12380)
        t6065 = inline12381
        var negative__273 bool = t6065 == 45
        var jp6067 int
        if negative__273 {
            jp6067 = 1
        } else {
            jp6067 = 0
        }
        var index__274 int = jp6067
        var result__275 int = 0
        var t6088 int
        var inline12378 int = _goml_runtime_core_string_len(value__272)
        t6088 = inline12378
        var t6089 bool = index__274 == t6088
        if t6089 {
            return Option__isize{
                _tag: 0,
            }
        } else {
            Loop_loop6074:
            for {
                var t6075 int
                var inline12376 int = _goml_runtime_core_string_len(value__272)
                t6075 = inline12376
                var t6076 bool = index__274 < t6075
                if t6076 {
                    var byte__276 uint8
                    var inline12374 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline12374
                    var t6086 bool = byte__276 < 48
                    var jp6081 bool
                    if t6086 {
                        jp6081 = true
                    } else {
                        var t6087 bool = byte__276 > 57
                        jp6081 = t6087
                    }
                    if jp6081 {
                        return Option__isize{
                            _tag: 0,
                        }
                    } else {
                        var t6082 int = result__275 * 10
                        var t6083 uint8 = byte__276 - 48
                        var t6084 int = int(uint8(t6083))
                        var t6085 int = t6082 + t6084
                        result__275 = t6085
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t6078 int = compound_old895 + compound_value896
                        index__274 = t6078
                        continue
                    }
                } else {
                    break Loop_loop6074
                }
            }
            var jp6071 int
            if negative__273 {
                var t6073 int = 0 - result__275
                jp6071 = t6073
            } else {
                jp6071 = result__275
            }
            var t6072 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: jp6071,
            }
            return t6072
        }
    }
}

func main0() struct{} {
    var mtmp796 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6865 _goml_m_std_p_json_p_Value
    switch mtmp796._tag {
    case 0:
        var x797 _goml_m_std_p_json_p_Value = mtmp796._v0_0
        jp6865 = x797
        var mtmp800 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "name")
        switch mtmp800._tag {
        case 0:
            var inline12822 string = "missing name"
            var inline12823 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12822)
            _goml_runtime_core_string_println(inline12823)
            var mtmp805 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "version")
            switch mtmp805._tag {
            case 0:
                var inline12837 string = "missing version"
                var inline12838 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12837)
                _goml_runtime_core_string_println(inline12838)
            case 1:
                var x806 _goml_m_std_p_json_p_Value = mtmp805._v1_0
                var mtmp807 Option__isize
                switch x806.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline12848 string = x806.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline12850 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline12848)
                    mtmp807 = inline12850
                default:
                    mtmp807 = Option__isize{
                        _tag: 0,
                    }
                }
                switch mtmp807._tag {
                case 0:
                    var inline12841 string = "invalid version"
                    var inline12842 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12841)
                    _goml_runtime_core_string_println(inline12842)
                case 1:
                    var x808 int = mtmp807._v1_0
                    var inline12845 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x808)
                    _goml_runtime_core_string_println(inline12845)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp810 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "stable")
            switch mtmp810._tag {
            case 0:
                var inline12852 string = "missing stable"
                var inline12853 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12852)
                _goml_runtime_core_string_println(inline12853)
                var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                println__T_string(t6869)
                return struct{}{}
            case 1:
                var x811 _goml_m_std_p_json_p_Value = mtmp810._v1_0
                var commute_field13794 bool
                switch x811.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline12863 bool = x811.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field13794 = inline12863
                    var inline12860 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field13794)
                    _goml_runtime_core_string_println(inline12860)
                    var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                    println__T_string(t6869)
                    return struct{}{}
                default:
                    var inline12856 string = "invalid stable"
                    var inline12857 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12856)
                    _goml_runtime_core_string_println(inline12857)
                    var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                    println__T_string(t6869)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case 1:
            var x801 _goml_m_std_p_json_p_Value = mtmp800._v1_0
            var commute_field13800 string
            switch x801.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline12833 string = x801.(_goml_m_std_p_json_p_Value_String)._0
                commute_field13800 = inline12833
                var inline12830 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field13800)
                _goml_runtime_core_string_println(inline12830)
                var mtmp805 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "version")
                switch mtmp805._tag {
                case 0:
                    var inline12837 string = "missing version"
                    var inline12838 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12837)
                    _goml_runtime_core_string_println(inline12838)
                case 1:
                    var x806 _goml_m_std_p_json_p_Value = mtmp805._v1_0
                    var mtmp807 Option__isize
                    switch x806.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline12848 string = x806.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline12850 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline12848)
                        mtmp807 = inline12850
                    default:
                        mtmp807 = Option__isize{
                            _tag: 0,
                        }
                    }
                    switch mtmp807._tag {
                    case 0:
                        var inline12841 string = "invalid version"
                        var inline12842 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12841)
                        _goml_runtime_core_string_println(inline12842)
                    case 1:
                        var x808 int = mtmp807._v1_0
                        var inline12845 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x808)
                        _goml_runtime_core_string_println(inline12845)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp810 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "stable")
                switch mtmp810._tag {
                case 0:
                    var inline12852 string = "missing stable"
                    var inline12853 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12852)
                    _goml_runtime_core_string_println(inline12853)
                    var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                    println__T_string(t6869)
                    return struct{}{}
                case 1:
                    var x811 _goml_m_std_p_json_p_Value = mtmp810._v1_0
                    var commute_field13794 bool
                    switch x811.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline12863 bool = x811.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field13794 = inline12863
                        var inline12860 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field13794)
                        _goml_runtime_core_string_println(inline12860)
                        var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                        println__T_string(t6869)
                        return struct{}{}
                    default:
                        var inline12856 string = "invalid stable"
                        var inline12857 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12856)
                        _goml_runtime_core_string_println(inline12857)
                        var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                        println__T_string(t6869)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline12826 string = "invalid name"
                var inline12827 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12826)
                _goml_runtime_core_string_println(inline12827)
                var mtmp805 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "version")
                switch mtmp805._tag {
                case 0:
                    var inline12837 string = "missing version"
                    var inline12838 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12837)
                    _goml_runtime_core_string_println(inline12838)
                case 1:
                    var x806 _goml_m_std_p_json_p_Value = mtmp805._v1_0
                    var mtmp807 Option__isize
                    switch x806.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline12848 string = x806.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline12850 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline12848)
                        mtmp807 = inline12850
                    default:
                        mtmp807 = Option__isize{
                            _tag: 0,
                        }
                    }
                    switch mtmp807._tag {
                    case 0:
                        var inline12841 string = "invalid version"
                        var inline12842 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12841)
                        _goml_runtime_core_string_println(inline12842)
                    case 1:
                        var x808 int = mtmp807._v1_0
                        var inline12845 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x808)
                        _goml_runtime_core_string_println(inline12845)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp810 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6865, "stable")
                switch mtmp810._tag {
                case 0:
                    var inline12852 string = "missing stable"
                    var inline12853 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12852)
                    _goml_runtime_core_string_println(inline12853)
                    var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                    println__T_string(t6869)
                    return struct{}{}
                case 1:
                    var x811 _goml_m_std_p_json_p_Value = mtmp810._v1_0
                    var commute_field13794 bool
                    switch x811.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline12863 bool = x811.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field13794 = inline12863
                        var inline12860 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field13794)
                        _goml_runtime_core_string_println(inline12860)
                        var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                        println__T_string(t6869)
                        return struct{}{}
                    default:
                        var inline12856 string = "invalid stable"
                        var inline12857 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12856)
                        _goml_runtime_core_string_println(inline12857)
                        var t6869 string = _goml_m_std_p_json_p_encode(jp6865)
                        println__T_string(t6869)
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
        var x798 string = mtmp796._v1_0
        var inline12819 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x798)
        _goml_runtime_core_string_println(inline12819)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__u8() *_goml_vec_uint8 {
    var t6885 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6885
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop6925:
    for {
        var t6926 int
        var inline12874 int = _goml_runtime_core_string_len(x397)
        t6926 = inline12874
        var t6927 bool = index__279 < t6926
        if t6927 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t6929 int = compound_old402 + x401
                index__279 = t6929
                continue
            } else {
                var t6931 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6931
            }
        } else {
            break Loop_loop6925
        }
    }
    var t6924 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t6924
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t6952 int = _goml_runtime_core_string_len(self__289)
    return t6952
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t6955 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t6955
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline12884 int64 = int64(int(self__285))
    var inline12885 string = signed_decimal_string(inline12884)
    return inline12885
}

func _goml_m_inherent_i_string_i_string_i_get(self__290 string, index__291 int) rune {
    var inline12923 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__290, index__291)
    var inline12924 bool = inline12923._0
    var inline12925 rune = inline12923._1
    if inline12924 {
        return inline12925
    } else {
        var inline12928 rune = _goml_runtime_core_string_get("", -1)
        return inline12928
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t7065 *ref_int_x = ref__Ref_3int(value__684)
    return t7065
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t7068 int = ref_get__Ref_3int(self__685)
    return t7068
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__686 *ref_int_x, value__687 int) struct{} {
    ref_set__Ref_3int(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__287 rune) string {
    var inline12935 uint32 = uint32(rune(self__287))
    var inline12936 bool = utf8_valid_scalar(inline12935)
    if inline12936 {
        var inline12937 string = _goml_runtime_core_char_to_string(self__287)
        return inline12937
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline13235 bool = string_is_char_boundary(self__294, start__295)
    var inline13237 bool
    if inline13235 {
        var inline13240 bool = string_is_char_boundary(self__294, end__296)
        inline13237 = inline13240
    } else {
        inline13237 = false
    }
    if inline13237 {
        var inline13238 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline13238
    } else {
        var inline13239 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline13239
    }
}

func char_from_u32(value__2 uint32) Option__char {
    var inline13246 bool = utf8_valid_scalar(value__2)
    if inline13246 {
        var inline13247 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline13248 rune = inline13247._1
        var inline13250 Option__char = Option__char{
            _tag: 1,
            _v1_0: inline13248,
        }
        return inline13250
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t7468 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t7468
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t7473 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t7473
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__511 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__512 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__511, elem__512)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t7509 string
    t7509 = value__1
    _goml_runtime_core_string_println(t7509)
    return struct{}{}
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t7635 bool = index__259 < 0
    var jp7633 bool
    if t7635 {
        jp7633 = true
    } else {
        var t7636 bool = index__259 >= length__260
        jp7633 = t7636
    }
    if jp7633 {
        var inline13261 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline13261
    } else {
        var t7520 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t7520))
        var t7523 bool = first__261 < 128
        if t7523 {
            var inline13263 int = 1
            var inline13264 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline13264._tag {
            case 0:
                var inline13265 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline13265
            case 1:
                var inline13266 rune = inline13264._v1_0
                var inline13268 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline13266,
                    _2: inline13263,
                }
                return inline13268
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t7527 bool = first__261 < 194
            if t7527 {
                var inline13270 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline13270
            } else {
                var t7531 bool = first__261 < 224
                if t7531 {
                    var t7544 int = length__260 - index__259
                    var t7545 bool = t7544 < 2
                    if t7545 {
                        var inline13272 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline13272
                    } else {
                        var t7533 int = index__259 + 1
                        var t7534 uint8
                        var inline13286 uint8 = _goml_runtime_core_string_byte_get(value__258, t7533)
                        t7534 = inline13286
                        var second__262 uint32 = uint32(uint8(t7534))
                        var t7537 bool
                        var inline13283 bool = second__262 < 128
                        if inline13283 {
                            t7537 = true
                        } else {
                            var inline13284 bool = second__262 > 191
                            t7537 = inline13284
                        }
                        if t7537 {
                            var inline13274 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline13274
                        } else {
                            var t7539_rhs uint32 = 31
                            var t7539 uint32 = first__261 & t7539_rhs
                            var t7540_rhs int = 6
                            var t7540 uint32 = t7539 << t7540_rhs
                            var t7541_rhs uint32 = 63
                            var t7541 uint32 = second__262 & t7541_rhs
                            var t7542 uint32 = t7540 | t7541
                            var inline13276 int = 2
                            var inline13277 Option__char = __goml_builtin_char_from_uint32(t7542)
                            switch inline13277._tag {
                            case 0:
                                var inline13278 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline13278
                            case 1:
                                var inline13279 rune = inline13277._v1_0
                                var inline13281 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline13279,
                                    _2: inline13276,
                                }
                                return inline13281
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t7549 bool = first__261 < 240
                    if t7549 {
                        var t7582 int = length__260 - index__259
                        var t7583 bool = t7582 < 3
                        if t7583 {
                            var inline13288 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline13288
                        } else {
                            var t7551 int = index__259 + 1
                            var t7552 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7551)
                            var second__263 uint32 = uint32(uint8(t7552))
                            var t7553 int = index__259 + 2
                            var t7554 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7553)
                            var third__264 uint32 = uint32(uint8(t7554))
                            var t7580 bool = utf8_invalid_continuation(second__263)
                            var jp7575 bool
                            if t7580 {
                                jp7575 = true
                            } else {
                                var inline13290 bool = third__264 < 128
                                if inline13290 {
                                    jp7575 = true
                                } else {
                                    var inline13291 bool = third__264 > 191
                                    jp7575 = inline13291
                                }
                            }
                            var jp7569 bool
                            if jp7575 {
                                jp7569 = true
                            } else {
                                var t7578 bool = first__261 == 224
                                if t7578 {
                                    var t7579 bool = second__263 < 160
                                    jp7569 = t7579
                                } else {
                                    jp7569 = false
                                }
                            }
                            var jp7558 bool
                            if jp7569 {
                                jp7558 = true
                            } else {
                                var t7572 bool = first__261 == 237
                                if t7572 {
                                    var t7573 bool = second__263 >= 160
                                    jp7558 = t7573
                                } else {
                                    jp7558 = false
                                }
                            }
                            if jp7558 {
                                var inline13293 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline13293
                            } else {
                                var t7560_rhs uint32 = 15
                                var t7560 uint32 = first__261 & t7560_rhs
                                var t7561_rhs int = 12
                                var t7561 uint32 = t7560 << t7561_rhs
                                var t7562_rhs uint32 = 63
                                var t7562 uint32 = second__263 & t7562_rhs
                                var t7563_rhs int = 6
                                var t7563 uint32 = t7562 << t7563_rhs
                                var t7564 uint32 = t7561 | t7563
                                var t7565_rhs uint32 = 63
                                var t7565 uint32 = third__264 & t7565_rhs
                                var t7566 uint32 = t7564 | t7565
                                var inline13295 int = 3
                                var inline13296 Option__char = __goml_builtin_char_from_uint32(t7566)
                                switch inline13296._tag {
                                case 0:
                                    var inline13297 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline13297
                                case 1:
                                    var inline13298 rune = inline13296._v1_0
                                    var inline13300 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline13298,
                                        _2: inline13295,
                                    }
                                    return inline13300
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t7587 bool = first__261 < 245
                        if t7587 {
                            var t7628 int = length__260 - index__259
                            var t7629 bool = t7628 < 4
                            if t7629 {
                                var t7630 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t7630
                            } else {
                                var t7589 int = index__259 + 1
                                var t7590 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7589)
                                var second__265 uint32 = uint32(uint8(t7590))
                                var t7591 int = index__259 + 2
                                var t7592 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7591)
                                var third__266 uint32 = uint32(uint8(t7592))
                                var t7593 int = index__259 + 3
                                var t7594 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7593)
                                var fourth__267 uint32 = uint32(uint8(t7594))
                                var t7626 bool = utf8_invalid_continuation(second__265)
                                var jp7624 bool
                                if t7626 {
                                    jp7624 = true
                                } else {
                                    var t7627 bool = utf8_invalid_continuation(third__266)
                                    jp7624 = t7627
                                }
                                var jp7618 bool
                                if jp7624 {
                                    jp7618 = true
                                } else {
                                    var t7625 bool = utf8_invalid_continuation(fourth__267)
                                    jp7618 = t7625
                                }
                                var jp7612 bool
                                if jp7618 {
                                    jp7612 = true
                                } else {
                                    var t7621 bool = first__261 == 240
                                    if t7621 {
                                        var t7622 bool = second__265 < 144
                                        jp7612 = t7622
                                    } else {
                                        jp7612 = false
                                    }
                                }
                                var jp7598 bool
                                if jp7612 {
                                    jp7598 = true
                                } else {
                                    var t7615 bool = first__261 == 244
                                    if t7615 {
                                        var t7616 bool = second__265 > 143
                                        jp7598 = t7616
                                    } else {
                                        jp7598 = false
                                    }
                                }
                                if jp7598 {
                                    var t7599 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t7599
                                } else {
                                    var t7600_rhs uint32 = 7
                                    var t7600 uint32 = first__261 & t7600_rhs
                                    var t7601_rhs int = 18
                                    var t7601 uint32 = t7600 << t7601_rhs
                                    var t7602_rhs uint32 = 63
                                    var t7602 uint32 = second__265 & t7602_rhs
                                    var t7603_rhs int = 12
                                    var t7603 uint32 = t7602 << t7603_rhs
                                    var t7604 uint32 = t7601 | t7603
                                    var t7605_rhs uint32 = 63
                                    var t7605 uint32 = third__266 & t7605_rhs
                                    var t7606_rhs int = 6
                                    var t7606 uint32 = t7605 << t7606_rhs
                                    var t7607 uint32 = t7604 | t7606
                                    var t7608_rhs uint32 = 63
                                    var t7608 uint32 = fourth__267 & t7608_rhs
                                    var t7609 uint32 = t7607 | t7608
                                    var t7610 Tuple3_4bool_4char_3int = utf8_valid_decode(t7609, 4)
                                    return t7610
                                }
                            }
                        } else {
                            var t7631 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t7631
                        }
                    }
                }
            }
        }
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t7790 int64 = int64(int(value__222))
    var inline13309 bool = t7790 < 0
    if inline13309 {
        var inline13310 uint64 = uint64(int64(t7790))
        var inline13311 uint64 = 0 - inline13310
        var inline13312 string = decimal_string(inline13311)
        var inline13313 string = "-" + inline13312
        return inline13313
    } else {
        var inline13314 uint64 = uint64(int64(t7790))
        var inline13315 string = decimal_string(inline13314)
        return inline13315
    }
}

func char_to_string(value__282 rune) string {
    var t7848 uint32 = uint32(rune(value__282))
    var t7849 bool
    var inline13349 bool = t7848 <= 1114111
    if inline13349 {
        var inline13350 bool = t7848 >= 55296
        var inline13352 bool
        if inline13350 {
            var inline13354 bool = t7848 <= 57343
            inline13352 = inline13354
        } else {
            inline13352 = false
        }
        var inline13353 bool = !inline13352
        t7849 = inline13353
    } else {
        t7849 = false
    }
    if t7849 {
        var t7850 string = _goml_runtime_core_char_to_string(value__282)
        return t7850
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t8003 bool = string_is_char_boundary(value__274, start__275)
    var jp8000 bool
    if t8003 {
        var t8004 bool = string_is_char_boundary(value__274, end__276)
        jp8000 = t8004
    } else {
        jp8000 = false
    }
    if jp8000 {
        var t8001 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t8001
    } else {
        var t8002 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t8002
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t8011 bool
    var inline13394 bool = value__283 <= 1114111
    if inline13394 {
        var inline13395 bool = value__283 >= 55296
        var inline13397 bool
        if inline13395 {
            var inline13399 bool = value__283 <= 57343
            inline13397 = inline13399
        } else {
            inline13397 = false
        }
        var inline13398 bool = !inline13397
        t8011 = inline13398
    } else {
        t8011 = false
    }
    if t8011 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t8012 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t8012
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline13401 int64 = int64(int(self__404))
    var inline13402 string = signed_decimal_string(inline13401)
    return inline13402
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t8020 string = _goml_runtime_core_bool_to_string(self__401)
    return t8020
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t8023 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t8023
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field13837 rune
    var inline13406 bool = utf8_valid_scalar(value__253)
    if inline13406 {
        var inline13407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline13408 rune = inline13407._1
        commute_field13837 = inline13408
        var t8029 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field13837,
            _2: width__254,
        }
        return t8029
    } else {
        var inline13404 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline13404
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t8034 bool = value__256 < 128
    if t8034 {
        return true
    } else {
        var t8035 bool = value__256 > 191
        return t8035
    }
}

func signed_decimal_string(value__214 int64) string {
    var t8393 bool = value__214 < 0
    if t8393 {
        var t8394 uint64 = uint64(int64(value__214))
        var t8395 uint64 = 0 - t8394
        var t8396 string = decimal_string(t8395)
        var t8397 string = "-" + t8396
        return t8397
    } else {
        var t8398 uint64 = uint64(int64(value__214))
        var t8399 string = decimal_string(t8398)
        return t8399
    }
}

func decimal_string(value__208 uint64) string {
    var t8422 bool = value__208 == 0
    if t8422 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop8415:
        for {
            var t8416 bool = remaining__210 > 0
            if t8416 {
                var t8417_rhs uint64 = 10
                var t8417 uint64 = remaining__210 % t8417_rhs
                var t8418 uint8 = uint8(uint64(t8417))
                var t8419 uint8 = t8418 + 48
                vec_push__Vec_5uint8(reversed__209, t8419)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t8420 uint64 = compound_old353 / compound_value354
                remaining__210 = t8420
                continue
            } else {
                break Loop_loop8415
            }
        }
        var t8404 int
        var inline13491 int = vec_len__Vec_5uint8(reversed__209)
        t8404 = inline13491
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t8404)
        var offset__212 int = 0
        Loop_loop8406:
        for {
            var t8407 int
            var inline13489 int = vec_len__Vec_5uint8(reversed__209)
            t8407 = inline13489
            var t8408 bool = offset__212 < t8407
            if t8408 {
                var t8409 int
                var inline13487 int = vec_len__Vec_5uint8(reversed__209)
                t8409 = inline13487
                var t8410 int = t8409 - offset__212
                var t8411 int = t8410 - 1
                var t8412 uint8 = vec_get__Vec_5uint8(reversed__209, t8411)
                vec_push__Vec_5uint8(bytes__211, t8412)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t8413 int = compound_old358 + compound_value359
                offset__212 = t8413
                continue
            } else {
                break Loop_loop8406
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t8508 bool = value__257 <= 1114111
    if t8508 {
        var t8512 bool = value__257 >= 55296
        var jp8510 bool
        if t8512 {
            var t8513 bool = value__257 <= 57343
            jp8510 = t8513
        } else {
            jp8510 = false
        }
        var t8511 bool = !jp8510
        return t8511
    } else {
        return false
    }
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t8527 bool = index__269 < 0
    var jp8519 bool
    if t8527 {
        jp8519 = true
    } else {
        var t8528 int
        var inline13507 int = _goml_runtime_core_string_len(value__268)
        t8528 = inline13507
        var t8529 bool = index__269 > t8528
        jp8519 = t8529
    }
    if jp8519 {
        return false
    } else {
        var t8522 int
        var inline13511 int = _goml_runtime_core_string_len(value__268)
        t8522 = inline13511
        var t8523 bool = index__269 == t8522
        if t8523 {
            return true
        } else {
            var t8524 uint8
            var inline13509 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t8524 = inline13509
            var t8525_rhs uint8 = 192
            var t8525 uint8 = t8524 & t8525_rhs
            var t8526 bool = t8525 != 128
            return t8526
        }
    }
}

func main() {
    main0()
}
