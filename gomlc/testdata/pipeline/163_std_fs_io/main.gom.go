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

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_std_fs_read_bytes(path string) Tuple3_4bool_10Vec_5uint8_6string {
    var data []uint8
    var err error
    data, err = _goml_os.ReadFile(path)
    if err != nil {
        return Tuple3_4bool_10Vec_5uint8_6string{
            _0: false,
            _1: &_goml_vec_uint8{
                items: nil,
            },
            _2: err.Error(),
        }
    }
    return Tuple3_4bool_10Vec_5uint8_6string{
        _0: true,
        _1: &_goml_vec_uint8{
            items: data,
        },
        _2: "",
    }
}

func _goml_runtime_std_fs_write_bytes(path string, data *_goml_vec_uint8) Tuple2_4bool_6string {
    var err error = _goml_os.WriteFile(path, data.items, 0644)
    if err != nil {
        return Tuple2_4bool_6string{
            _0: false,
            _1: err.Error(),
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: "",
    }
}

func _goml_runtime_std_fs_file_exists(path string) bool {
    var err error
    _, err = _goml_os.Stat(path)
    return err == nil
}

func _goml_runtime_std_fs_read_dir(path string) Tuple3_4bool_11Vec_6string_6string {
    var entries []_goml_os.DirEntry
    var err error
    entries, err = _goml_os.ReadDir(path)
    if err != nil {
        return Tuple3_4bool_11Vec_6string_6string{
            _0: false,
            _1: &_goml_vec_string{
                items: nil,
            },
            _2: err.Error(),
        }
    }
    var names []string
    var i int = 0
    for {
        if i >= int(len(entries)) {
            break
        }
        var entry _goml_os.DirEntry = entries[i]
        names = append(names, entry.Name())
        i = i + 1
    }
    return Tuple3_4bool_11Vec_6string_6string{
        _0: true,
        _1: &_goml_vec_string{
            items: names,
        },
        _2: "",
    }
}

func _goml_runtime_std_io_println(value string) struct{} {
    _goml_os.Stdout.WriteString(value + "\n")
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
}

type _goml_vec__goml_m_std_p_fs_p_DirEntry struct {
    items []_goml_m_std_p_fs_p_DirEntry
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
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

type _goml_m_std_p_fs_p_Error struct {
    details _goml_m_std_p_io_p_ErrorDetails
}

type _goml_m_std_p_fs_p_Permissions struct {
    mode_value uint32
}

type _goml_m_std_p_fs_p_Metadata struct {
    file_type_value _goml_m_std_p_fs_p_FileType
    length_value int64
    permissions_value _goml_m_std_p_fs_p_Permissions
    modified_unix_nanoseconds_value int64
}

type _goml_m_std_p_fs_p_DirEntry struct {
    name_value string
    path_value string
    file_type_value _goml_m_std_p_fs_p_FileType
}

type Ordering int32

type _goml_m_std_p_io_p_ErrorKind int32

type _goml_m_std_p_fs_p_FileType int32

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

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error()
}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error() {}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Err) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error() {}

type _goml_m_Result____string____std_p_fs_p_Error interface {
    is_goml_m_Result____string____std_p_fs_p_Error()
}

type _goml_m_Result____string____std_p_fs_p_Error_Ok struct {
    _0 string
}

func (_ _goml_m_Result____string____std_p_fs_p_Error_Ok) is_goml_m_Result____string____std_p_fs_p_Error() {}

type _goml_m_Result____string____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____string____std_p_fs_p_Error_Err) is_goml_m_Result____string____std_p_fs_p_Error() {}

type _goml_m_Result____unit____std_p_fs_p_Error interface {
    is_goml_m_Result____unit____std_p_fs_p_Error()
}

type _goml_m_Result____unit____std_p_fs_p_Error_Ok struct {
    _0 struct{}
}

func (_ _goml_m_Result____unit____std_p_fs_p_Error_Ok) is_goml_m_Result____unit____std_p_fs_p_Error() {}

type _goml_m_Result____unit____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____unit____std_p_fs_p_Error_Err) is_goml_m_Result____unit____std_p_fs_p_Error() {}

type _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error interface {
    is_goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error()
}

type _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Ok struct {
    _0 _goml_m_std_p_fs_p_Metadata
}

func (_ _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Ok) is_goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error() {}

type _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Err) is_goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error() {}

type _goml_m_Result____bool____std_p_fs_p_Error interface {
    is_goml_m_Result____bool____std_p_fs_p_Error()
}

type _goml_m_Result____bool____std_p_fs_p_Error_Ok struct {
    _0 bool
}

func (_ _goml_m_Result____bool____std_p_fs_p_Error_Ok) is_goml_m_Result____bool____std_p_fs_p_Error() {}

type _goml_m_Result____bool____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____bool____std_p_fs_p_Error_Err) is_goml_m_Result____bool____std_p_fs_p_Error() {}

type _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error interface {
    is_goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error()
}

type _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Ok struct {
    _0 *_goml_vec__goml_m_std_p_fs_p_DirEntry
}

func (_ _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Ok) is_goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error() {}

type _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Err) is_goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error() {}

type _goml_m_Result____Vec_l_string_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec_string
    _v1_0 string
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t814 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t814
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t817 *_goml_vec_uint8
    var inline1826 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t817 = inline1826
    var t818 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t817,
    }
    return t818
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t890 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t890
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t920 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t920
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t932 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t932
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field2342 _goml_m_std_p_bytes_p_Bytes
    var commute_field2344 string
    var inline2158 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline2159 bool = inline2158._0
    var inline2160 *_goml_vec_uint8 = inline2158._1
    var inline2161 string = inline2158._2
    if inline2159 {
        var inline2165 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline2160)
        commute_field2342 = inline2165
        var inline2149 *_goml_vec_uint8 = commute_field2342.values
        var inline2150 Tuple2_4bool_6string = string_from_utf8(inline2149)
        var inline2151 bool = inline2150._0
        var inline2152 string = inline2150._1
        if inline2151 {
            var inline2155 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: inline2152,
            }
            return inline2155
        } else {
            var inline2156 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: "invalid UTF-8",
            }
            return inline2156
        }
    } else {
        commute_field2344 = inline2161
        var t1491 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: commute_field2344,
        }
        return t1491
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1505 *_goml_vec_uint8
    var inline2187 *_goml_vec_uint8 = data__123.values
    t1505 = inline2187
    var mtmp70 Tuple2_4bool_6string
    var inline2185 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1505)
    mtmp70 = inline2185
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1508 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1508
    } else {
        var t1509 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1509
    }
}

func main0() struct{} {
    var inline2251 string = "goml-std-test.txt"
    var inline2252 string = "std-ok"
    var inline2253 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline2252)
    _goml_m_std_p_fs_p_write__bytes(inline2251, inline2253)
    var t1569 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t1570 string
    switch t1569._tag {
    case 0:
        var inline2245 string = t1569._v0_0
        t1570 = inline2245
    case 1:
        var inline2247 string = t1569._v1_0
        var inline2249 string = "err " + inline2247
        t1570 = inline2249
    default:
        panic("non-exhaustive match")
    }
    var inline2242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1570)
    _goml_m_std_p_internal_p_host_p_println(inline2242)
    var t1571 bool
    var inline2239 string = "goml-std-test.txt"
    var inline2240 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline2239)
    t1571 = inline2240
    var t1572 string
    var inline2237 string = _goml_runtime_core_bool_to_string(t1571)
    t1572 = inline2237
    var inline2234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1572)
    _goml_m_std_p_internal_p_host_p_println(inline2234)
    var t1573 _goml_m_Result____Vec_l_string_r_____string
    var inline2223 string = "."
    var inline2224 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline2223)
    var inline2225 bool = inline2224._0
    var inline2226 *_goml_vec_string = inline2224._1
    var inline2227 string = inline2224._2
    if inline2225 {
        var inline2231 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string{
            _tag: 0,
            _v0_0: inline2226,
        }
        t1573 = inline2231
    } else {
        var inline2232 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string{
            _tag: 1,
            _v1_0: inline2227,
        }
        t1573 = inline2232
    }
    var t1574 string
    switch t1573._tag {
    case 0:
        var inline2214 *_goml_vec_string = t1573._v0_0
        var inline2216 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline2214)
        var inline2217 bool = inline2216 > 0
        var inline2218 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline2217)
        t1574 = inline2218
    case 1:
        var inline2219 string = t1573._v1_0
        var inline2221 string = "err " + inline2219
        t1574 = inline2221
    default:
        panic("non-exhaustive match")
    }
    var inline2211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1574)
    _goml_m_std_p_internal_p_host_p_println(inline2211)
    return struct{}{}
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop1617:
    for {
        var t1618 int
        var inline2263 int = _goml_runtime_core_string_len(x397)
        t1618 = inline2263
        var t1619 bool = index__279 < t1618
        if t1619 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t1621 int = compound_old402 + x401
                index__279 = t1621
                continue
            } else {
                var t1623 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1623
            }
        } else {
            break Loop_loop1617
        }
    }
    var t1616 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t1616
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__526 *_goml_vec_string) int {
    var t1649 int = vec_len__Vec_6string(self__526)
    return t1649
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1652 string = _goml_runtime_core_bool_to_string(self__401)
    return t1652
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1658 int = _goml_runtime_core_string_len(self__289)
    return t1658
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1777 bool = index__259 < 0
    var jp1775 bool
    if t1777 {
        jp1775 = true
    } else {
        var t1778 bool = index__259 >= length__260
        jp1775 = t1778
    }
    if jp1775 {
        var inline2272 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2272
    } else {
        var t1662 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1662))
        var t1665 bool = first__261 < 128
        if t1665 {
            var inline2274 int = 1
            var inline2275 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline2275._tag {
            case 0:
                var inline2276 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2276
            case 1:
                var inline2277 rune = inline2275._v1_0
                var inline2279 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2277,
                    _2: inline2274,
                }
                return inline2279
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1669 bool = first__261 < 194
            if t1669 {
                var inline2281 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2281
            } else {
                var t1673 bool = first__261 < 224
                if t1673 {
                    var t1686 int = length__260 - index__259
                    var t1687 bool = t1686 < 2
                    if t1687 {
                        var inline2283 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2283
                    } else {
                        var t1675 int = index__259 + 1
                        var t1676 uint8
                        var inline2297 uint8 = _goml_runtime_core_string_byte_get(value__258, t1675)
                        t1676 = inline2297
                        var second__262 uint32 = uint32(uint8(t1676))
                        var t1679 bool
                        var inline2294 bool = second__262 < 128
                        if inline2294 {
                            t1679 = true
                        } else {
                            var inline2295 bool = second__262 > 191
                            t1679 = inline2295
                        }
                        if t1679 {
                            var inline2285 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2285
                        } else {
                            var t1681_rhs uint32 = 31
                            var t1681 uint32 = first__261 & t1681_rhs
                            var t1682_rhs int = 6
                            var t1682 uint32 = t1681 << t1682_rhs
                            var t1683_rhs uint32 = 63
                            var t1683 uint32 = second__262 & t1683_rhs
                            var t1684 uint32 = t1682 | t1683
                            var inline2287 int = 2
                            var inline2288 Option__char = __goml_builtin_char_from_uint32(t1684)
                            switch inline2288._tag {
                            case 0:
                                var inline2289 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2289
                            case 1:
                                var inline2290 rune = inline2288._v1_0
                                var inline2292 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2290,
                                    _2: inline2287,
                                }
                                return inline2292
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1691 bool = first__261 < 240
                    if t1691 {
                        var t1724 int = length__260 - index__259
                        var t1725 bool = t1724 < 3
                        if t1725 {
                            var inline2299 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2299
                        } else {
                            var t1693 int = index__259 + 1
                            var t1694 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1693)
                            var second__263 uint32 = uint32(uint8(t1694))
                            var t1695 int = index__259 + 2
                            var t1696 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1695)
                            var third__264 uint32 = uint32(uint8(t1696))
                            var t1722 bool = utf8_invalid_continuation(second__263)
                            var jp1717 bool
                            if t1722 {
                                jp1717 = true
                            } else {
                                var inline2301 bool = third__264 < 128
                                if inline2301 {
                                    jp1717 = true
                                } else {
                                    var inline2302 bool = third__264 > 191
                                    jp1717 = inline2302
                                }
                            }
                            var jp1711 bool
                            if jp1717 {
                                jp1711 = true
                            } else {
                                var t1720 bool = first__261 == 224
                                if t1720 {
                                    var t1721 bool = second__263 < 160
                                    jp1711 = t1721
                                } else {
                                    jp1711 = false
                                }
                            }
                            var jp1700 bool
                            if jp1711 {
                                jp1700 = true
                            } else {
                                var t1714 bool = first__261 == 237
                                if t1714 {
                                    var t1715 bool = second__263 >= 160
                                    jp1700 = t1715
                                } else {
                                    jp1700 = false
                                }
                            }
                            if jp1700 {
                                var inline2304 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2304
                            } else {
                                var t1702_rhs uint32 = 15
                                var t1702 uint32 = first__261 & t1702_rhs
                                var t1703_rhs int = 12
                                var t1703 uint32 = t1702 << t1703_rhs
                                var t1704_rhs uint32 = 63
                                var t1704 uint32 = second__263 & t1704_rhs
                                var t1705_rhs int = 6
                                var t1705 uint32 = t1704 << t1705_rhs
                                var t1706 uint32 = t1703 | t1705
                                var t1707_rhs uint32 = 63
                                var t1707 uint32 = third__264 & t1707_rhs
                                var t1708 uint32 = t1706 | t1707
                                var inline2306 int = 3
                                var inline2307 Option__char = __goml_builtin_char_from_uint32(t1708)
                                switch inline2307._tag {
                                case 0:
                                    var inline2308 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2308
                                case 1:
                                    var inline2309 rune = inline2307._v1_0
                                    var inline2311 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2309,
                                        _2: inline2306,
                                    }
                                    return inline2311
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1729 bool = first__261 < 245
                        if t1729 {
                            var t1770 int = length__260 - index__259
                            var t1771 bool = t1770 < 4
                            if t1771 {
                                var t1772 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1772
                            } else {
                                var t1731 int = index__259 + 1
                                var t1732 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1731)
                                var second__265 uint32 = uint32(uint8(t1732))
                                var t1733 int = index__259 + 2
                                var t1734 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1733)
                                var third__266 uint32 = uint32(uint8(t1734))
                                var t1735 int = index__259 + 3
                                var t1736 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1735)
                                var fourth__267 uint32 = uint32(uint8(t1736))
                                var t1768 bool = utf8_invalid_continuation(second__265)
                                var jp1766 bool
                                if t1768 {
                                    jp1766 = true
                                } else {
                                    var t1769 bool = utf8_invalid_continuation(third__266)
                                    jp1766 = t1769
                                }
                                var jp1760 bool
                                if jp1766 {
                                    jp1760 = true
                                } else {
                                    var t1767 bool = utf8_invalid_continuation(fourth__267)
                                    jp1760 = t1767
                                }
                                var jp1754 bool
                                if jp1760 {
                                    jp1754 = true
                                } else {
                                    var t1763 bool = first__261 == 240
                                    if t1763 {
                                        var t1764 bool = second__265 < 144
                                        jp1754 = t1764
                                    } else {
                                        jp1754 = false
                                    }
                                }
                                var jp1740 bool
                                if jp1754 {
                                    jp1740 = true
                                } else {
                                    var t1757 bool = first__261 == 244
                                    if t1757 {
                                        var t1758 bool = second__265 > 143
                                        jp1740 = t1758
                                    } else {
                                        jp1740 = false
                                    }
                                }
                                if jp1740 {
                                    var t1741 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1741
                                } else {
                                    var t1742_rhs uint32 = 7
                                    var t1742 uint32 = first__261 & t1742_rhs
                                    var t1743_rhs int = 18
                                    var t1743 uint32 = t1742 << t1743_rhs
                                    var t1744_rhs uint32 = 63
                                    var t1744 uint32 = second__265 & t1744_rhs
                                    var t1745_rhs int = 12
                                    var t1745 uint32 = t1744 << t1745_rhs
                                    var t1746 uint32 = t1743 | t1745
                                    var t1747_rhs uint32 = 63
                                    var t1747 uint32 = third__266 & t1747_rhs
                                    var t1748_rhs int = 6
                                    var t1748 uint32 = t1747 << t1748_rhs
                                    var t1749 uint32 = t1746 | t1748
                                    var t1750_rhs uint32 = 63
                                    var t1750 uint32 = fourth__267 & t1750_rhs
                                    var t1751 uint32 = t1749 | t1750
                                    var t1752 Tuple3_4bool_4char_3int = utf8_valid_decode(t1751, 4)
                                    return t1752
                                }
                            }
                        } else {
                            var t1773 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1773
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1789 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1789
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1792 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1792
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field2347 rune
    var inline2315 bool = utf8_valid_scalar(value__253)
    if inline2315 {
        var inline2316 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline2317 rune = inline2316._1
        commute_field2347 = inline2317
        var t1798 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2347,
            _2: width__254,
        }
        return t1798
    } else {
        var inline2313 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2313
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1803 bool = value__256 < 128
    if t1803 {
        return true
    } else {
        var t1804 bool = value__256 > 191
        return t1804
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1809 bool
    var inline2321 bool = value__283 <= 1114111
    if inline2321 {
        var inline2322 bool = value__283 >= 55296
        var inline2324 bool
        if inline2322 {
            var inline2326 bool = value__283 <= 57343
            inline2324 = inline2326
        } else {
            inline2324 = false
        }
        var inline2325 bool = !inline2324
        t1809 = inline2325
    } else {
        t1809 = false
    }
    if t1809 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1810 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1810
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1815 bool = value__257 <= 1114111
    if t1815 {
        var t1819 bool = value__257 >= 55296
        var jp1817 bool
        if t1819 {
            var t1820 bool = value__257 <= 57343
            jp1817 = t1820
        } else {
            jp1817 = false
        }
        var t1818 bool = !jp1817
        return t1818
    } else {
        return false
    }
}

func main() {
    main0()
}
