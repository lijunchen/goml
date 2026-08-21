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
    var t434 *_goml_vec_uint8
    var inline1474 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t434 = inline1474
    var t435 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t434,
    }
    return t435
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t479 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t479)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t482 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: x8,
        }
        return t482
    } else {
        var t483 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "invalid UTF-8",
        }
        return t483
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
    var inline1829 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline1829
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t1130 _goml_m_std_p_bytes_p_Bytes
        var inline1827 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t1130 = inline1827
        var t1131 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 0,
            _v0_0: t1130,
        }
        return t1131
    } else {
        var t1132 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 1,
            _v1_0: x69,
        }
        return t1132
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1135 *_goml_vec_uint8
    var inline1833 *_goml_vec_uint8 = data__123.values
    t1135 = inline1833
    var mtmp70 Tuple2_4bool_6string
    var inline1831 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1135)
    mtmp70 = inline1831
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1138 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1138
    } else {
        var t1139 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1139
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline1835 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline1835
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t1144 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1144
    } else {
        var t1145 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x75,
        }
        return t1145
    }
}

func main0() struct{} {
    var t1196 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t1196)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1890 string = ""
    var inline1891 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1890)
    var inline1892 string = inline1891 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline1892)
    var t1197 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t1198 string
    switch t1197._tag {
    case 0:
        t1198 = "ok"
    case 1:
        var inline1886 string = t1197._v1_0
        var inline1888 string = "err " + inline1886
        t1198 = inline1888
    default:
        panic("non-exhaustive match")
    }
    var inline1883 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1198)
    _goml_m_std_p_internal_p_host_p_println(inline1883)
    var t1199 Result__unit__string
    var inline1878 string = "goml-self-host/nested/output.txt"
    var inline1879 string = "boot"
    var inline1880 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1879)
    var inline1881 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1878, inline1880)
    t1199 = inline1881
    var t1200 string
    switch t1199._tag {
    case 0:
        t1200 = "ok"
    case 1:
        var inline1874 string = t1199._v1_0
        var inline1876 string = "err " + inline1874
        t1200 = inline1876
    default:
        panic("non-exhaustive match")
    }
    var inline1871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1200)
    _goml_m_std_p_internal_p_host_p_println(inline1871)
    var t1201 Result__string__string
    var inline1862 string = "goml-self-host/nested/output.txt"
    var inline1863 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1862)
    switch inline1863._tag {
    case 0:
        var inline1864 _goml_m_std_p_bytes_p_Bytes = inline1863._v0_0
        var inline1866 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1864)
        t1201 = inline1866
    case 1:
        var inline1867 string = inline1863._v1_0
        var inline1869 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline1867,
        }
        t1201 = inline1869
    default:
        panic("non-exhaustive match")
    }
    var t1202 string
    switch t1201._tag {
    case 0:
        var inline1856 string = t1201._v0_0
        t1202 = inline1856
    case 1:
        var inline1858 string = t1201._v1_0
        var inline1860 string = "err " + inline1858
        t1202 = inline1860
    default:
        panic("non-exhaustive match")
    }
    var inline1853 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1202)
    _goml_m_std_p_internal_p_host_p_println(inline1853)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1243:
    for {
        var t1244 int
        var inline1902 int = _goml_runtime_core_string_len(x12)
        t1244 = inline1902
        var t1245 bool = index__26 < t1244
        if t1245 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1247 int = compound_old17 + x16
                index__26 = t1247
                continue
            } else {
                var t1249 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1249
            }
        } else {
            break Loop_loop1243
        }
    }
    var t1242 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1242
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1271 string
    t1271 = value__68
    _goml_runtime_std_io_println(t1271)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1281 bool = string_is_char_boundary(value__21, start__22)
    var jp1278 bool
    if t1281 {
        var t1282 bool = string_is_char_boundary(value__21, end__23)
        jp1278 = t1282
    } else {
        jp1278 = false
    }
    if jp1278 {
        var t1279 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1279
    } else {
        var t1280 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1280
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1284 string
    t1284 = value__69
    _goml_runtime_std_io_eprint(t1284)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1292 int = _goml_runtime_core_string_len(self__36)
    return t1292
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1411 bool = index__6 < 0
    var jp1409 bool
    if t1411 {
        jp1409 = true
    } else {
        var t1412 bool = index__6 >= length__7
        jp1409 = t1412
    }
    if jp1409 {
        var inline1917 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1917
    } else {
        var t1296 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1296))
        var t1299 bool = first__8 < 128
        if t1299 {
            var inline1919 int = 1
            var inline1920 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1920._tag {
            case 0:
                var inline1921 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1921
            case 1:
                var inline1922 rune = inline1920._v1_0
                var inline1924 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1922,
                    _2: inline1919,
                }
                return inline1924
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1303 bool = first__8 < 194
            if t1303 {
                var inline1926 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1926
            } else {
                var t1307 bool = first__8 < 224
                if t1307 {
                    var t1320 int = length__7 - index__6
                    var t1321 bool = t1320 < 2
                    if t1321 {
                        var inline1928 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1928
                    } else {
                        var t1309 int = index__6 + 1
                        var t1310 uint8
                        var inline1942 uint8 = _goml_runtime_core_string_byte_get(value__5, t1309)
                        t1310 = inline1942
                        var second__9 uint32 = uint32(uint8(t1310))
                        var t1313 bool
                        var inline1939 bool = second__9 < 128
                        if inline1939 {
                            t1313 = true
                        } else {
                            var inline1940 bool = second__9 > 191
                            t1313 = inline1940
                        }
                        if t1313 {
                            var inline1930 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1930
                        } else {
                            var t1315_rhs uint32 = 31
                            var t1315 uint32 = first__8 & t1315_rhs
                            var t1316_rhs int = 6
                            var t1316 uint32 = t1315 << t1316_rhs
                            var t1317_rhs uint32 = 63
                            var t1317 uint32 = second__9 & t1317_rhs
                            var t1318 uint32 = t1316 | t1317
                            var inline1932 int = 2
                            var inline1933 Option__char = __goml_builtin_char_from_uint32(t1318)
                            switch inline1933._tag {
                            case 0:
                                var inline1934 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1934
                            case 1:
                                var inline1935 rune = inline1933._v1_0
                                var inline1937 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1935,
                                    _2: inline1932,
                                }
                                return inline1937
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1325 bool = first__8 < 240
                    if t1325 {
                        var t1358 int = length__7 - index__6
                        var t1359 bool = t1358 < 3
                        if t1359 {
                            var inline1944 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1944
                        } else {
                            var t1327 int = index__6 + 1
                            var t1328 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1327)
                            var second__10 uint32 = uint32(uint8(t1328))
                            var t1329 int = index__6 + 2
                            var t1330 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1329)
                            var third__11 uint32 = uint32(uint8(t1330))
                            var t1356 bool = utf8_invalid_continuation(second__10)
                            var jp1351 bool
                            if t1356 {
                                jp1351 = true
                            } else {
                                var inline1946 bool = third__11 < 128
                                if inline1946 {
                                    jp1351 = true
                                } else {
                                    var inline1947 bool = third__11 > 191
                                    jp1351 = inline1947
                                }
                            }
                            var jp1345 bool
                            if jp1351 {
                                jp1345 = true
                            } else {
                                var t1354 bool = first__8 == 224
                                if t1354 {
                                    var t1355 bool = second__10 < 160
                                    jp1345 = t1355
                                } else {
                                    jp1345 = false
                                }
                            }
                            var jp1334 bool
                            if jp1345 {
                                jp1334 = true
                            } else {
                                var t1348 bool = first__8 == 237
                                if t1348 {
                                    var t1349 bool = second__10 >= 160
                                    jp1334 = t1349
                                } else {
                                    jp1334 = false
                                }
                            }
                            if jp1334 {
                                var inline1949 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1949
                            } else {
                                var t1336_rhs uint32 = 15
                                var t1336 uint32 = first__8 & t1336_rhs
                                var t1337_rhs int = 12
                                var t1337 uint32 = t1336 << t1337_rhs
                                var t1338_rhs uint32 = 63
                                var t1338 uint32 = second__10 & t1338_rhs
                                var t1339_rhs int = 6
                                var t1339 uint32 = t1338 << t1339_rhs
                                var t1340 uint32 = t1337 | t1339
                                var t1341_rhs uint32 = 63
                                var t1341 uint32 = third__11 & t1341_rhs
                                var t1342 uint32 = t1340 | t1341
                                var inline1951 int = 3
                                var inline1952 Option__char = __goml_builtin_char_from_uint32(t1342)
                                switch inline1952._tag {
                                case 0:
                                    var inline1953 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1953
                                case 1:
                                    var inline1954 rune = inline1952._v1_0
                                    var inline1956 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1954,
                                        _2: inline1951,
                                    }
                                    return inline1956
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1363 bool = first__8 < 245
                        if t1363 {
                            var t1404 int = length__7 - index__6
                            var t1405 bool = t1404 < 4
                            if t1405 {
                                var t1406 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1406
                            } else {
                                var t1365 int = index__6 + 1
                                var t1366 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1365)
                                var second__12 uint32 = uint32(uint8(t1366))
                                var t1367 int = index__6 + 2
                                var t1368 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1367)
                                var third__13 uint32 = uint32(uint8(t1368))
                                var t1369 int = index__6 + 3
                                var t1370 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1369)
                                var fourth__14 uint32 = uint32(uint8(t1370))
                                var t1402 bool = utf8_invalid_continuation(second__12)
                                var jp1400 bool
                                if t1402 {
                                    jp1400 = true
                                } else {
                                    var t1403 bool = utf8_invalid_continuation(third__13)
                                    jp1400 = t1403
                                }
                                var jp1394 bool
                                if jp1400 {
                                    jp1394 = true
                                } else {
                                    var t1401 bool = utf8_invalid_continuation(fourth__14)
                                    jp1394 = t1401
                                }
                                var jp1388 bool
                                if jp1394 {
                                    jp1388 = true
                                } else {
                                    var t1397 bool = first__8 == 240
                                    if t1397 {
                                        var t1398 bool = second__12 < 144
                                        jp1388 = t1398
                                    } else {
                                        jp1388 = false
                                    }
                                }
                                var jp1374 bool
                                if jp1388 {
                                    jp1374 = true
                                } else {
                                    var t1391 bool = first__8 == 244
                                    if t1391 {
                                        var t1392 bool = second__12 > 143
                                        jp1374 = t1392
                                    } else {
                                        jp1374 = false
                                    }
                                }
                                if jp1374 {
                                    var t1375 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1375
                                } else {
                                    var t1376_rhs uint32 = 7
                                    var t1376 uint32 = first__8 & t1376_rhs
                                    var t1377_rhs int = 18
                                    var t1377 uint32 = t1376 << t1377_rhs
                                    var t1378_rhs uint32 = 63
                                    var t1378 uint32 = second__12 & t1378_rhs
                                    var t1379_rhs int = 12
                                    var t1379 uint32 = t1378 << t1379_rhs
                                    var t1380 uint32 = t1377 | t1379
                                    var t1381_rhs uint32 = 63
                                    var t1381 uint32 = third__13 & t1381_rhs
                                    var t1382_rhs int = 6
                                    var t1382 uint32 = t1381 << t1382_rhs
                                    var t1383 uint32 = t1380 | t1382
                                    var t1384_rhs uint32 = 63
                                    var t1384 uint32 = fourth__14 & t1384_rhs
                                    var t1385 uint32 = t1383 | t1384
                                    var t1386 Tuple3_4bool_4char_3int = utf8_valid_decode(t1385, 4)
                                    return t1386
                                }
                            }
                        } else {
                            var t1407 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1407
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
    var t1434 bool = index__16 < 0
    var jp1426 bool
    if t1434 {
        jp1426 = true
    } else {
        var t1435 int
        var inline1958 int = _goml_runtime_core_string_len(value__15)
        t1435 = inline1958
        var t1436 bool = index__16 > t1435
        jp1426 = t1436
    }
    if jp1426 {
        return false
    } else {
        var t1429 int
        var inline1962 int = _goml_runtime_core_string_len(value__15)
        t1429 = inline1962
        var t1430 bool = index__16 == t1429
        if t1430 {
            return true
        } else {
            var t1431 uint8
            var inline1960 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1431 = inline1960
            var t1432_rhs uint8 = 192
            var t1432 uint8 = t1431 & t1432_rhs
            var t1433 bool = t1432 != 128
            return t1433
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1439 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1439
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1442 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1442
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1998 rune
    var inline1966 bool = utf8_valid_scalar(value__0)
    if inline1966 {
        var inline1967 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1968 rune = inline1967._1
        commute_field1998 = inline1968
        var t1448 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1998,
            _2: width__1,
        }
        return t1448
    } else {
        var inline1964 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1964
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1453 bool = value__3 < 128
    if t1453 {
        return true
    } else {
        var t1454 bool = value__3 > 191
        return t1454
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1459 bool
    var inline1972 bool = value__30 <= 1114111
    if inline1972 {
        var inline1973 bool = value__30 >= 55296
        var inline1975 bool
        if inline1973 {
            var inline1977 bool = value__30 <= 57343
            inline1975 = inline1977
        } else {
            inline1975 = false
        }
        var inline1976 bool = !inline1975
        t1459 = inline1976
    } else {
        t1459 = false
    }
    if t1459 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1460 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1460
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1465 bool = value__4 <= 1114111
    if t1465 {
        var t1469 bool = value__4 >= 55296
        var jp1467 bool
        if t1469 {
            var t1470 bool = value__4 <= 57343
            jp1467 = t1470
        } else {
            jp1467 = false
        }
        var t1468 bool = !jp1467
        return t1468
    } else {
        return false
    }
}

func main() {
    main0()
}
