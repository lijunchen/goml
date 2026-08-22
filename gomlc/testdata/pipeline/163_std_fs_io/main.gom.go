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
    var t815 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t815
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t818 *_goml_vec_uint8
    var inline1820 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t818 = inline1820
    var t819 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t818,
    }
    return t819
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t891 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t891
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t921 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t921
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t933 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t933
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field2334 _goml_m_std_p_bytes_p_Bytes
    var commute_field2336 string
    var inline2150 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline2151 bool = inline2150._0
    var inline2152 *_goml_vec_uint8 = inline2150._1
    var inline2153 string = inline2150._2
    if inline2151 {
        var inline2157 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline2152)
        commute_field2334 = inline2157
        var inline2141 *_goml_vec_uint8 = commute_field2334.values
        var inline2142 Tuple2_4bool_6string = string_from_utf8(inline2141)
        var inline2143 bool = inline2142._0
        var inline2144 string = inline2142._1
        if inline2143 {
            var inline2147 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: inline2144,
            }
            return inline2147
        } else {
            var inline2148 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: "invalid UTF-8",
            }
            return inline2148
        }
    } else {
        commute_field2336 = inline2153
        var t1493 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: commute_field2336,
        }
        return t1493
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1507 *_goml_vec_uint8
    var inline2179 *_goml_vec_uint8 = data__123.values
    t1507 = inline2179
    var mtmp70 Tuple2_4bool_6string
    var inline2177 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1507)
    mtmp70 = inline2177
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1510 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1510
    } else {
        var t1511 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1511
    }
}

func main0() struct{} {
    var inline2243 string = "goml-std-test.txt"
    var inline2244 string = "std-ok"
    var inline2245 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline2244)
    _goml_m_std_p_fs_p_write__bytes(inline2243, inline2245)
    var t1571 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t1572 string
    switch t1571._tag {
    case 0:
        var inline2237 string = t1571._v0_0
        t1572 = inline2237
    case 1:
        var inline2239 string = t1571._v1_0
        var inline2241 string = "err " + inline2239
        t1572 = inline2241
    default:
        panic("non-exhaustive match")
    }
    var inline2234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1572)
    _goml_m_std_p_internal_p_host_p_println(inline2234)
    var t1573 bool
    var inline2231 string = "goml-std-test.txt"
    var inline2232 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline2231)
    t1573 = inline2232
    var t1574 string
    var inline2229 string = _goml_runtime_core_bool_to_string(t1573)
    t1574 = inline2229
    var inline2226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1574)
    _goml_m_std_p_internal_p_host_p_println(inline2226)
    var t1575 _goml_m_Result____Vec_l_string_r_____string
    var inline2215 string = "."
    var inline2216 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline2215)
    var inline2217 bool = inline2216._0
    var inline2218 *_goml_vec_string = inline2216._1
    var inline2219 string = inline2216._2
    if inline2217 {
        var inline2223 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string{
            _tag: 0,
            _v0_0: inline2218,
        }
        t1575 = inline2223
    } else {
        var inline2224 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string{
            _tag: 1,
            _v1_0: inline2219,
        }
        t1575 = inline2224
    }
    var t1576 string
    switch t1575._tag {
    case 0:
        var inline2206 *_goml_vec_string = t1575._v0_0
        var inline2208 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline2206)
        var inline2209 bool = inline2208 > 0
        var inline2210 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline2209)
        t1576 = inline2210
    case 1:
        var inline2211 string = t1575._v1_0
        var inline2213 string = "err " + inline2211
        t1576 = inline2213
    default:
        panic("non-exhaustive match")
    }
    var inline2203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1576)
    _goml_m_std_p_internal_p_host_p_println(inline2203)
    return struct{}{}
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop1616:
    for {
        var t1617 int
        var inline2255 int = _goml_runtime_core_string_len(x397)
        t1617 = inline2255
        var t1618 bool = index__279 < t1617
        if t1618 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t1620 int = compound_old402 + x401
                index__279 = t1620
                continue
            } else {
                var t1622 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1622
            }
        } else {
            break Loop_loop1616
        }
    }
    var t1615 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t1615
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__526 *_goml_vec_string) int {
    var t1645 int = vec_len__Vec_6string(self__526)
    return t1645
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1648 string = _goml_runtime_core_bool_to_string(self__401)
    return t1648
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1654 int = _goml_runtime_core_string_len(self__289)
    return t1654
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1773 bool = index__259 < 0
    var jp1771 bool
    if t1773 {
        jp1771 = true
    } else {
        var t1774 bool = index__259 >= length__260
        jp1771 = t1774
    }
    if jp1771 {
        var inline2264 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2264
    } else {
        var t1658 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1658))
        var t1661 bool = first__261 < 128
        if t1661 {
            var inline2266 int = 1
            var inline2267 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline2267._tag {
            case 0:
                var inline2268 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2268
            case 1:
                var inline2269 rune = inline2267._v1_0
                var inline2271 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2269,
                    _2: inline2266,
                }
                return inline2271
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1665 bool = first__261 < 194
            if t1665 {
                var inline2273 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2273
            } else {
                var t1669 bool = first__261 < 224
                if t1669 {
                    var t1682 int = length__260 - index__259
                    var t1683 bool = t1682 < 2
                    if t1683 {
                        var inline2275 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2275
                    } else {
                        var t1671 int = index__259 + 1
                        var t1672 uint8
                        var inline2289 uint8 = _goml_runtime_core_string_byte_get(value__258, t1671)
                        t1672 = inline2289
                        var second__262 uint32 = uint32(uint8(t1672))
                        var t1675 bool
                        var inline2286 bool = second__262 < 128
                        if inline2286 {
                            t1675 = true
                        } else {
                            var inline2287 bool = second__262 > 191
                            t1675 = inline2287
                        }
                        if t1675 {
                            var inline2277 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2277
                        } else {
                            var t1677_rhs uint32 = 31
                            var t1677 uint32 = first__261 & t1677_rhs
                            var t1678_rhs int = 6
                            var t1678 uint32 = t1677 << t1678_rhs
                            var t1679_rhs uint32 = 63
                            var t1679 uint32 = second__262 & t1679_rhs
                            var t1680 uint32 = t1678 | t1679
                            var inline2279 int = 2
                            var inline2280 Option__char = __goml_builtin_char_from_uint32(t1680)
                            switch inline2280._tag {
                            case 0:
                                var inline2281 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2281
                            case 1:
                                var inline2282 rune = inline2280._v1_0
                                var inline2284 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2282,
                                    _2: inline2279,
                                }
                                return inline2284
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1687 bool = first__261 < 240
                    if t1687 {
                        var t1720 int = length__260 - index__259
                        var t1721 bool = t1720 < 3
                        if t1721 {
                            var inline2291 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2291
                        } else {
                            var t1689 int = index__259 + 1
                            var t1690 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1689)
                            var second__263 uint32 = uint32(uint8(t1690))
                            var t1691 int = index__259 + 2
                            var t1692 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1691)
                            var third__264 uint32 = uint32(uint8(t1692))
                            var t1718 bool = utf8_invalid_continuation(second__263)
                            var jp1713 bool
                            if t1718 {
                                jp1713 = true
                            } else {
                                var inline2293 bool = third__264 < 128
                                if inline2293 {
                                    jp1713 = true
                                } else {
                                    var inline2294 bool = third__264 > 191
                                    jp1713 = inline2294
                                }
                            }
                            var jp1707 bool
                            if jp1713 {
                                jp1707 = true
                            } else {
                                var t1716 bool = first__261 == 224
                                if t1716 {
                                    var t1717 bool = second__263 < 160
                                    jp1707 = t1717
                                } else {
                                    jp1707 = false
                                }
                            }
                            var jp1696 bool
                            if jp1707 {
                                jp1696 = true
                            } else {
                                var t1710 bool = first__261 == 237
                                if t1710 {
                                    var t1711 bool = second__263 >= 160
                                    jp1696 = t1711
                                } else {
                                    jp1696 = false
                                }
                            }
                            if jp1696 {
                                var inline2296 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2296
                            } else {
                                var t1698_rhs uint32 = 15
                                var t1698 uint32 = first__261 & t1698_rhs
                                var t1699_rhs int = 12
                                var t1699 uint32 = t1698 << t1699_rhs
                                var t1700_rhs uint32 = 63
                                var t1700 uint32 = second__263 & t1700_rhs
                                var t1701_rhs int = 6
                                var t1701 uint32 = t1700 << t1701_rhs
                                var t1702 uint32 = t1699 | t1701
                                var t1703_rhs uint32 = 63
                                var t1703 uint32 = third__264 & t1703_rhs
                                var t1704 uint32 = t1702 | t1703
                                var inline2298 int = 3
                                var inline2299 Option__char = __goml_builtin_char_from_uint32(t1704)
                                switch inline2299._tag {
                                case 0:
                                    var inline2300 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2300
                                case 1:
                                    var inline2301 rune = inline2299._v1_0
                                    var inline2303 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2301,
                                        _2: inline2298,
                                    }
                                    return inline2303
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1725 bool = first__261 < 245
                        if t1725 {
                            var t1766 int = length__260 - index__259
                            var t1767 bool = t1766 < 4
                            if t1767 {
                                var t1768 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1768
                            } else {
                                var t1727 int = index__259 + 1
                                var t1728 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1727)
                                var second__265 uint32 = uint32(uint8(t1728))
                                var t1729 int = index__259 + 2
                                var t1730 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1729)
                                var third__266 uint32 = uint32(uint8(t1730))
                                var t1731 int = index__259 + 3
                                var t1732 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1731)
                                var fourth__267 uint32 = uint32(uint8(t1732))
                                var t1764 bool = utf8_invalid_continuation(second__265)
                                var jp1762 bool
                                if t1764 {
                                    jp1762 = true
                                } else {
                                    var t1765 bool = utf8_invalid_continuation(third__266)
                                    jp1762 = t1765
                                }
                                var jp1756 bool
                                if jp1762 {
                                    jp1756 = true
                                } else {
                                    var t1763 bool = utf8_invalid_continuation(fourth__267)
                                    jp1756 = t1763
                                }
                                var jp1750 bool
                                if jp1756 {
                                    jp1750 = true
                                } else {
                                    var t1759 bool = first__261 == 240
                                    if t1759 {
                                        var t1760 bool = second__265 < 144
                                        jp1750 = t1760
                                    } else {
                                        jp1750 = false
                                    }
                                }
                                var jp1736 bool
                                if jp1750 {
                                    jp1736 = true
                                } else {
                                    var t1753 bool = first__261 == 244
                                    if t1753 {
                                        var t1754 bool = second__265 > 143
                                        jp1736 = t1754
                                    } else {
                                        jp1736 = false
                                    }
                                }
                                if jp1736 {
                                    var t1737 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1737
                                } else {
                                    var t1738_rhs uint32 = 7
                                    var t1738 uint32 = first__261 & t1738_rhs
                                    var t1739_rhs int = 18
                                    var t1739 uint32 = t1738 << t1739_rhs
                                    var t1740_rhs uint32 = 63
                                    var t1740 uint32 = second__265 & t1740_rhs
                                    var t1741_rhs int = 12
                                    var t1741 uint32 = t1740 << t1741_rhs
                                    var t1742 uint32 = t1739 | t1741
                                    var t1743_rhs uint32 = 63
                                    var t1743 uint32 = third__266 & t1743_rhs
                                    var t1744_rhs int = 6
                                    var t1744 uint32 = t1743 << t1744_rhs
                                    var t1745 uint32 = t1742 | t1744
                                    var t1746_rhs uint32 = 63
                                    var t1746 uint32 = fourth__267 & t1746_rhs
                                    var t1747 uint32 = t1745 | t1746
                                    var t1748 Tuple3_4bool_4char_3int = utf8_valid_decode(t1747, 4)
                                    return t1748
                                }
                            }
                        } else {
                            var t1769 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1769
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
    var t1785 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1785
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1788 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1788
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field2339 rune
    var inline2307 bool = utf8_valid_scalar(value__253)
    if inline2307 {
        var inline2308 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline2309 rune = inline2308._1
        commute_field2339 = inline2309
        var t1794 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2339,
            _2: width__254,
        }
        return t1794
    } else {
        var inline2305 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2305
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1799 bool = value__256 < 128
    if t1799 {
        return true
    } else {
        var t1800 bool = value__256 > 191
        return t1800
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1805 bool
    var inline2313 bool = value__283 <= 1114111
    if inline2313 {
        var inline2314 bool = value__283 >= 55296
        var inline2316 bool
        if inline2314 {
            var inline2318 bool = value__283 <= 57343
            inline2316 = inline2318
        } else {
            inline2316 = false
        }
        var inline2317 bool = !inline2316
        t1805 = inline2317
    } else {
        t1805 = false
    }
    if t1805 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1806 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1806
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1811 bool = value__257 <= 1114111
    if t1811 {
        var t1815 bool = value__257 >= 55296
        var jp1813 bool
        if t1815 {
            var t1816 bool = value__257 <= 57343
            jp1813 = t1816
        } else {
            jp1813 = false
        }
        var t1814 bool = !jp1813
        return t1814
    } else {
        return false
    }
}

func main() {
    main0()
}
