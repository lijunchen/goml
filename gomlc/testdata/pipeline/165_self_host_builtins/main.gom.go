package main

import (
    _goml_context "context"
    _goml_fmt "fmt"
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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
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

func _goml_runtime_std_fs_create_dir_all(path string) Tuple2_4bool_6string {
    var err error = _goml_os.MkdirAll(path, 0755)
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

func _goml_runtime_std_io_println(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
}

func _goml_runtime_std_io_eprint(value string) struct{} {
    _goml_fmt.Fprint(_goml_os.Stderr, value)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
}

type _goml_vec__goml_m_std_p_fs_p_DirEntry struct {
    items []_goml_m_std_p_fs_p_DirEntry
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

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
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

type Option__uint8 struct {
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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t431 *_goml_vec_uint8
    var inline1465 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t431 = inline1465
    var t432 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t431,
    }
    return t432
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t476 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t476)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t479 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: x8,
        }
        return t479
    } else {
        var t480 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "invalid UTF-8",
        }
        return t480
    }
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_internal_p_host_p_eprint(value__29 string) struct{} {
    _goml_runtime_std_io_eprint(value__29)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__bytes(path__118 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp66 Tuple3_4bool_10Vec_5uint8_6string
    var inline1820 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline1820
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t1121 _goml_m_std_p_bytes_p_Bytes
        var inline1818 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t1121 = inline1818
        var t1122 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 0,
            _v0_0: t1121,
        }
        return t1122
    } else {
        var t1123 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 1,
            _v1_0: x69,
        }
        return t1123
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1126 *_goml_vec_uint8
    var inline1824 *_goml_vec_uint8 = data__123.values
    t1126 = inline1824
    var mtmp70 Tuple2_4bool_6string
    var inline1822 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1126)
    mtmp70 = inline1822
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1129 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1129
    } else {
        var t1130 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1130
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline1826 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline1826
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t1135 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1135
    } else {
        var t1136 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x75,
        }
        return t1136
    }
}

func main0() struct{} {
    var t1187 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t1187)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1881 string = ""
    var inline1882 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1881)
    var inline1883 string = inline1882 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline1883)
    var t1188 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t1189 string
    switch t1188._tag {
    case 0:
        t1189 = "ok"
    case 1:
        var inline1877 string = t1188._v1_0
        var inline1879 string = "err " + inline1877
        t1189 = inline1879
    default:
        panic("non-exhaustive match")
    }
    var inline1874 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1189)
    _goml_m_std_p_internal_p_host_p_println(inline1874)
    var t1190 Result__unit__string
    var inline1869 string = "goml-self-host/nested/output.txt"
    var inline1870 string = "boot"
    var inline1871 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1870)
    var inline1872 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1869, inline1871)
    t1190 = inline1872
    var t1191 string
    switch t1190._tag {
    case 0:
        t1191 = "ok"
    case 1:
        var inline1865 string = t1190._v1_0
        var inline1867 string = "err " + inline1865
        t1191 = inline1867
    default:
        panic("non-exhaustive match")
    }
    var inline1862 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1191)
    _goml_m_std_p_internal_p_host_p_println(inline1862)
    var t1192 Result__string__string
    var inline1853 string = "goml-self-host/nested/output.txt"
    var inline1854 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1853)
    switch inline1854._tag {
    case 0:
        var inline1855 _goml_m_std_p_bytes_p_Bytes = inline1854._v0_0
        var inline1857 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1855)
        t1192 = inline1857
    case 1:
        var inline1858 string = inline1854._v1_0
        var inline1860 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline1858,
        }
        t1192 = inline1860
    default:
        panic("non-exhaustive match")
    }
    var t1193 string
    switch t1192._tag {
    case 0:
        var inline1847 string = t1192._v0_0
        t1193 = inline1847
    case 1:
        var inline1849 string = t1192._v1_0
        var inline1851 string = "err " + inline1849
        t1193 = inline1851
    default:
        panic("non-exhaustive match")
    }
    var inline1844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1193)
    _goml_m_std_p_internal_p_host_p_println(inline1844)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1234:
    for {
        var t1235 int
        var inline1893 int = _goml_runtime_core_string_len(x12)
        t1235 = inline1893
        var t1236 bool = index__26 < t1235
        if t1236 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1238 int = compound_old17 + x16
                index__26 = t1238
                continue
            } else {
                var t1240 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1240
            }
        } else {
            break Loop_loop1234
        }
    }
    var t1233 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1233
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1262 string
    t1262 = value__68
    _goml_runtime_std_io_println(t1262)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1272 bool = string_is_char_boundary(value__21, start__22)
    var jp1269 bool
    if t1272 {
        var t1273 bool = string_is_char_boundary(value__21, end__23)
        jp1269 = t1273
    } else {
        jp1269 = false
    }
    if jp1269 {
        var t1270 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1270
    } else {
        var t1271 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1271
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1275 string
    t1275 = value__69
    _goml_runtime_std_io_eprint(t1275)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1283 int = _goml_runtime_core_string_len(self__36)
    return t1283
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1402 bool = index__6 < 0
    var jp1400 bool
    if t1402 {
        jp1400 = true
    } else {
        var t1403 bool = index__6 >= length__7
        jp1400 = t1403
    }
    if jp1400 {
        var inline1908 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1908
    } else {
        var t1287 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1287))
        var t1290 bool = first__8 < 128
        if t1290 {
            var inline1910 int = 1
            var inline1911 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1911._tag {
            case 0:
                var inline1912 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1912
            case 1:
                var inline1913 rune = inline1911._v1_0
                var inline1915 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1913,
                    _2: inline1910,
                }
                return inline1915
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1294 bool = first__8 < 194
            if t1294 {
                var inline1917 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1917
            } else {
                var t1298 bool = first__8 < 224
                if t1298 {
                    var t1311 int = length__7 - index__6
                    var t1312 bool = t1311 < 2
                    if t1312 {
                        var inline1919 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1919
                    } else {
                        var t1300 int = index__6 + 1
                        var t1301 uint8
                        var inline1933 uint8 = _goml_runtime_core_string_byte_get(value__5, t1300)
                        t1301 = inline1933
                        var second__9 uint32 = uint32(uint8(t1301))
                        var t1304 bool
                        var inline1930 bool = second__9 < 128
                        if inline1930 {
                            t1304 = true
                        } else {
                            var inline1931 bool = second__9 > 191
                            t1304 = inline1931
                        }
                        if t1304 {
                            var inline1921 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1921
                        } else {
                            var t1306_rhs uint32 = 31
                            var t1306 uint32 = first__8 & t1306_rhs
                            var t1307_rhs int = 6
                            var t1307 uint32 = t1306 << t1307_rhs
                            var t1308_rhs uint32 = 63
                            var t1308 uint32 = second__9 & t1308_rhs
                            var t1309 uint32 = t1307 | t1308
                            var inline1923 int = 2
                            var inline1924 Option__char = __goml_builtin_char_from_uint32(t1309)
                            switch inline1924._tag {
                            case 0:
                                var inline1925 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1925
                            case 1:
                                var inline1926 rune = inline1924._v1_0
                                var inline1928 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1926,
                                    _2: inline1923,
                                }
                                return inline1928
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1316 bool = first__8 < 240
                    if t1316 {
                        var t1349 int = length__7 - index__6
                        var t1350 bool = t1349 < 3
                        if t1350 {
                            var inline1935 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1935
                        } else {
                            var t1318 int = index__6 + 1
                            var t1319 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1318)
                            var second__10 uint32 = uint32(uint8(t1319))
                            var t1320 int = index__6 + 2
                            var t1321 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1320)
                            var third__11 uint32 = uint32(uint8(t1321))
                            var t1347 bool = utf8_invalid_continuation(second__10)
                            var jp1342 bool
                            if t1347 {
                                jp1342 = true
                            } else {
                                var inline1937 bool = third__11 < 128
                                if inline1937 {
                                    jp1342 = true
                                } else {
                                    var inline1938 bool = third__11 > 191
                                    jp1342 = inline1938
                                }
                            }
                            var jp1336 bool
                            if jp1342 {
                                jp1336 = true
                            } else {
                                var t1345 bool = first__8 == 224
                                if t1345 {
                                    var t1346 bool = second__10 < 160
                                    jp1336 = t1346
                                } else {
                                    jp1336 = false
                                }
                            }
                            var jp1325 bool
                            if jp1336 {
                                jp1325 = true
                            } else {
                                var t1339 bool = first__8 == 237
                                if t1339 {
                                    var t1340 bool = second__10 >= 160
                                    jp1325 = t1340
                                } else {
                                    jp1325 = false
                                }
                            }
                            if jp1325 {
                                var inline1940 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1940
                            } else {
                                var t1327_rhs uint32 = 15
                                var t1327 uint32 = first__8 & t1327_rhs
                                var t1328_rhs int = 12
                                var t1328 uint32 = t1327 << t1328_rhs
                                var t1329_rhs uint32 = 63
                                var t1329 uint32 = second__10 & t1329_rhs
                                var t1330_rhs int = 6
                                var t1330 uint32 = t1329 << t1330_rhs
                                var t1331 uint32 = t1328 | t1330
                                var t1332_rhs uint32 = 63
                                var t1332 uint32 = third__11 & t1332_rhs
                                var t1333 uint32 = t1331 | t1332
                                var inline1942 int = 3
                                var inline1943 Option__char = __goml_builtin_char_from_uint32(t1333)
                                switch inline1943._tag {
                                case 0:
                                    var inline1944 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1944
                                case 1:
                                    var inline1945 rune = inline1943._v1_0
                                    var inline1947 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1945,
                                        _2: inline1942,
                                    }
                                    return inline1947
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1354 bool = first__8 < 245
                        if t1354 {
                            var t1395 int = length__7 - index__6
                            var t1396 bool = t1395 < 4
                            if t1396 {
                                var t1397 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1397
                            } else {
                                var t1356 int = index__6 + 1
                                var t1357 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1356)
                                var second__12 uint32 = uint32(uint8(t1357))
                                var t1358 int = index__6 + 2
                                var t1359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1358)
                                var third__13 uint32 = uint32(uint8(t1359))
                                var t1360 int = index__6 + 3
                                var t1361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1360)
                                var fourth__14 uint32 = uint32(uint8(t1361))
                                var t1393 bool = utf8_invalid_continuation(second__12)
                                var jp1391 bool
                                if t1393 {
                                    jp1391 = true
                                } else {
                                    var t1394 bool = utf8_invalid_continuation(third__13)
                                    jp1391 = t1394
                                }
                                var jp1385 bool
                                if jp1391 {
                                    jp1385 = true
                                } else {
                                    var t1392 bool = utf8_invalid_continuation(fourth__14)
                                    jp1385 = t1392
                                }
                                var jp1379 bool
                                if jp1385 {
                                    jp1379 = true
                                } else {
                                    var t1388 bool = first__8 == 240
                                    if t1388 {
                                        var t1389 bool = second__12 < 144
                                        jp1379 = t1389
                                    } else {
                                        jp1379 = false
                                    }
                                }
                                var jp1365 bool
                                if jp1379 {
                                    jp1365 = true
                                } else {
                                    var t1382 bool = first__8 == 244
                                    if t1382 {
                                        var t1383 bool = second__12 > 143
                                        jp1365 = t1383
                                    } else {
                                        jp1365 = false
                                    }
                                }
                                if jp1365 {
                                    var t1366 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1366
                                } else {
                                    var t1367_rhs uint32 = 7
                                    var t1367 uint32 = first__8 & t1367_rhs
                                    var t1368_rhs int = 18
                                    var t1368 uint32 = t1367 << t1368_rhs
                                    var t1369_rhs uint32 = 63
                                    var t1369 uint32 = second__12 & t1369_rhs
                                    var t1370_rhs int = 12
                                    var t1370 uint32 = t1369 << t1370_rhs
                                    var t1371 uint32 = t1368 | t1370
                                    var t1372_rhs uint32 = 63
                                    var t1372 uint32 = third__13 & t1372_rhs
                                    var t1373_rhs int = 6
                                    var t1373 uint32 = t1372 << t1373_rhs
                                    var t1374 uint32 = t1371 | t1373
                                    var t1375_rhs uint32 = 63
                                    var t1375 uint32 = fourth__14 & t1375_rhs
                                    var t1376 uint32 = t1374 | t1375
                                    var t1377 Tuple3_4bool_4char_3int = utf8_valid_decode(t1376, 4)
                                    return t1377
                                }
                            }
                        } else {
                            var t1398 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1398
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1425 bool = index__16 < 0
    var jp1417 bool
    if t1425 {
        jp1417 = true
    } else {
        var t1426 int
        var inline1949 int = _goml_runtime_core_string_len(value__15)
        t1426 = inline1949
        var t1427 bool = index__16 > t1426
        jp1417 = t1427
    }
    if jp1417 {
        return false
    } else {
        var t1420 int
        var inline1953 int = _goml_runtime_core_string_len(value__15)
        t1420 = inline1953
        var t1421 bool = index__16 == t1420
        if t1421 {
            return true
        } else {
            var t1422 uint8
            var inline1951 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1422 = inline1951
            var t1423_rhs uint8 = 192
            var t1423 uint8 = t1422 & t1423_rhs
            var t1424 bool = t1423 != 128
            return t1424
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1430 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1430
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1433 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1433
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1989 rune
    var inline1957 bool = utf8_valid_scalar(value__0)
    if inline1957 {
        var inline1958 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1959 rune = inline1958._1
        commute_field1989 = inline1959
        var t1439 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1989,
            _2: width__1,
        }
        return t1439
    } else {
        var inline1955 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1955
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1444 bool = value__3 < 128
    if t1444 {
        return true
    } else {
        var t1445 bool = value__3 > 191
        return t1445
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1450 bool
    var inline1963 bool = value__30 <= 1114111
    if inline1963 {
        var inline1964 bool = value__30 >= 55296
        var inline1966 bool
        if inline1964 {
            var inline1968 bool = value__30 <= 57343
            inline1966 = inline1968
        } else {
            inline1966 = false
        }
        var inline1967 bool = !inline1966
        t1450 = inline1967
    } else {
        t1450 = false
    }
    if t1450 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1451 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1451
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1456 bool = value__4 <= 1114111
    if t1456 {
        var t1460 bool = value__4 >= 55296
        var jp1458 bool
        if t1460 {
            var t1461 bool = value__4 <= 57343
            jp1458 = t1461
        } else {
            jp1458 = false
        }
        var t1459 bool = !jp1458
        return t1459
    } else {
        return false
    }
}

func main() {
    main0()
}
