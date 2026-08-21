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
    _goml_fmt.Println(value)
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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t430 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t430
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t433 *_goml_vec_uint8
    var inline1447 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t433 = inline1447
    var t434 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t433,
    }
    return t434
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t506 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t506
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t536 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t536
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t548 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t548
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1961 _goml_m_std_p_bytes_p_Bytes
    var commute_field1963 string
    var inline1777 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline1778 bool = inline1777._0
    var inline1779 *_goml_vec_uint8 = inline1777._1
    var inline1780 string = inline1777._2
    if inline1778 {
        var inline1784 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1779)
        commute_field1961 = inline1784
        var inline1768 *_goml_vec_uint8 = commute_field1961.values
        var inline1769 Tuple2_4bool_6string = string_from_utf8(inline1768)
        var inline1770 bool = inline1769._0
        var inline1771 string = inline1769._1
        if inline1770 {
            var inline1774 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: inline1771,
            }
            return inline1774
        } else {
            var inline1775 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: "invalid UTF-8",
            }
            return inline1775
        }
    } else {
        commute_field1963 = inline1780
        var t1120 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: commute_field1963,
        }
        return t1120
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1134 *_goml_vec_uint8
    var inline1806 *_goml_vec_uint8 = data__123.values
    t1134 = inline1806
    var mtmp70 Tuple2_4bool_6string
    var inline1804 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1134)
    mtmp70 = inline1804
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1137 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1137
    } else {
        var t1138 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1138
    }
}

func main0() struct{} {
    var inline1870 string = "goml-std-test.txt"
    var inline1871 string = "std-ok"
    var inline1872 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1871)
    _goml_m_std_p_fs_p_write__bytes(inline1870, inline1872)
    var t1198 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t1199 string
    switch t1198._tag {
    case 0:
        var inline1864 string = t1198._v0_0
        t1199 = inline1864
    case 1:
        var inline1866 string = t1198._v1_0
        var inline1868 string = "err " + inline1866
        t1199 = inline1868
    default:
        panic("non-exhaustive match")
    }
    var inline1861 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1199)
    _goml_m_std_p_internal_p_host_p_println(inline1861)
    var t1200 bool
    var inline1858 string = "goml-std-test.txt"
    var inline1859 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline1858)
    t1200 = inline1859
    var t1201 string
    var inline1856 string = _goml_runtime_core_bool_to_string(t1200)
    t1201 = inline1856
    var inline1853 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1201)
    _goml_m_std_p_internal_p_host_p_println(inline1853)
    var t1202 _goml_m_Result____Vec_l_string_r_____string
    var inline1842 string = "."
    var inline1843 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline1842)
    var inline1844 bool = inline1843._0
    var inline1845 *_goml_vec_string = inline1843._1
    var inline1846 string = inline1843._2
    if inline1844 {
        var inline1850 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string{
            _tag: 0,
            _v0_0: inline1845,
        }
        t1202 = inline1850
    } else {
        var inline1851 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string{
            _tag: 1,
            _v1_0: inline1846,
        }
        t1202 = inline1851
    }
    var t1203 string
    switch t1202._tag {
    case 0:
        var inline1833 *_goml_vec_string = t1202._v0_0
        var inline1835 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1833)
        var inline1836 bool = inline1835 > 0
        var inline1837 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1836)
        t1203 = inline1837
    case 1:
        var inline1838 string = t1202._v1_0
        var inline1840 string = "err " + inline1838
        t1203 = inline1840
    default:
        panic("non-exhaustive match")
    }
    var inline1830 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1203)
    _goml_m_std_p_internal_p_host_p_println(inline1830)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1243:
    for {
        var t1244 int
        var inline1882 int = _goml_runtime_core_string_len(x12)
        t1244 = inline1882
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__273 *_goml_vec_string) int {
    var t1272 int = vec_len__Vec_6string(self__273)
    return t1272
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1275 string = _goml_runtime_core_bool_to_string(self__148)
    return t1275
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1281 int = _goml_runtime_core_string_len(self__36)
    return t1281
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1400 bool = index__6 < 0
    var jp1398 bool
    if t1400 {
        jp1398 = true
    } else {
        var t1401 bool = index__6 >= length__7
        jp1398 = t1401
    }
    if jp1398 {
        var inline1891 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1891
    } else {
        var t1285 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1285))
        var t1288 bool = first__8 < 128
        if t1288 {
            var inline1893 int = 1
            var inline1894 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1894._tag {
            case 0:
                var inline1895 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1895
            case 1:
                var inline1896 rune = inline1894._v1_0
                var inline1898 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1896,
                    _2: inline1893,
                }
                return inline1898
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1292 bool = first__8 < 194
            if t1292 {
                var inline1900 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1900
            } else {
                var t1296 bool = first__8 < 224
                if t1296 {
                    var t1309 int = length__7 - index__6
                    var t1310 bool = t1309 < 2
                    if t1310 {
                        var inline1902 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1902
                    } else {
                        var t1298 int = index__6 + 1
                        var t1299 uint8
                        var inline1916 uint8 = _goml_runtime_core_string_byte_get(value__5, t1298)
                        t1299 = inline1916
                        var second__9 uint32 = uint32(uint8(t1299))
                        var t1302 bool
                        var inline1913 bool = second__9 < 128
                        if inline1913 {
                            t1302 = true
                        } else {
                            var inline1914 bool = second__9 > 191
                            t1302 = inline1914
                        }
                        if t1302 {
                            var inline1904 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1904
                        } else {
                            var t1304_rhs uint32 = 31
                            var t1304 uint32 = first__8 & t1304_rhs
                            var t1305_rhs int = 6
                            var t1305 uint32 = t1304 << t1305_rhs
                            var t1306_rhs uint32 = 63
                            var t1306 uint32 = second__9 & t1306_rhs
                            var t1307 uint32 = t1305 | t1306
                            var inline1906 int = 2
                            var inline1907 Option__char = __goml_builtin_char_from_uint32(t1307)
                            switch inline1907._tag {
                            case 0:
                                var inline1908 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1908
                            case 1:
                                var inline1909 rune = inline1907._v1_0
                                var inline1911 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1909,
                                    _2: inline1906,
                                }
                                return inline1911
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1314 bool = first__8 < 240
                    if t1314 {
                        var t1347 int = length__7 - index__6
                        var t1348 bool = t1347 < 3
                        if t1348 {
                            var inline1918 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1918
                        } else {
                            var t1316 int = index__6 + 1
                            var t1317 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1316)
                            var second__10 uint32 = uint32(uint8(t1317))
                            var t1318 int = index__6 + 2
                            var t1319 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1318)
                            var third__11 uint32 = uint32(uint8(t1319))
                            var t1345 bool = utf8_invalid_continuation(second__10)
                            var jp1340 bool
                            if t1345 {
                                jp1340 = true
                            } else {
                                var inline1920 bool = third__11 < 128
                                if inline1920 {
                                    jp1340 = true
                                } else {
                                    var inline1921 bool = third__11 > 191
                                    jp1340 = inline1921
                                }
                            }
                            var jp1334 bool
                            if jp1340 {
                                jp1334 = true
                            } else {
                                var t1343 bool = first__8 == 224
                                if t1343 {
                                    var t1344 bool = second__10 < 160
                                    jp1334 = t1344
                                } else {
                                    jp1334 = false
                                }
                            }
                            var jp1323 bool
                            if jp1334 {
                                jp1323 = true
                            } else {
                                var t1337 bool = first__8 == 237
                                if t1337 {
                                    var t1338 bool = second__10 >= 160
                                    jp1323 = t1338
                                } else {
                                    jp1323 = false
                                }
                            }
                            if jp1323 {
                                var inline1923 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1923
                            } else {
                                var t1325_rhs uint32 = 15
                                var t1325 uint32 = first__8 & t1325_rhs
                                var t1326_rhs int = 12
                                var t1326 uint32 = t1325 << t1326_rhs
                                var t1327_rhs uint32 = 63
                                var t1327 uint32 = second__10 & t1327_rhs
                                var t1328_rhs int = 6
                                var t1328 uint32 = t1327 << t1328_rhs
                                var t1329 uint32 = t1326 | t1328
                                var t1330_rhs uint32 = 63
                                var t1330 uint32 = third__11 & t1330_rhs
                                var t1331 uint32 = t1329 | t1330
                                var inline1925 int = 3
                                var inline1926 Option__char = __goml_builtin_char_from_uint32(t1331)
                                switch inline1926._tag {
                                case 0:
                                    var inline1927 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1927
                                case 1:
                                    var inline1928 rune = inline1926._v1_0
                                    var inline1930 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1928,
                                        _2: inline1925,
                                    }
                                    return inline1930
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1352 bool = first__8 < 245
                        if t1352 {
                            var t1393 int = length__7 - index__6
                            var t1394 bool = t1393 < 4
                            if t1394 {
                                var t1395 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1395
                            } else {
                                var t1354 int = index__6 + 1
                                var t1355 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1354)
                                var second__12 uint32 = uint32(uint8(t1355))
                                var t1356 int = index__6 + 2
                                var t1357 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1356)
                                var third__13 uint32 = uint32(uint8(t1357))
                                var t1358 int = index__6 + 3
                                var t1359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1358)
                                var fourth__14 uint32 = uint32(uint8(t1359))
                                var t1391 bool = utf8_invalid_continuation(second__12)
                                var jp1389 bool
                                if t1391 {
                                    jp1389 = true
                                } else {
                                    var t1392 bool = utf8_invalid_continuation(third__13)
                                    jp1389 = t1392
                                }
                                var jp1383 bool
                                if jp1389 {
                                    jp1383 = true
                                } else {
                                    var t1390 bool = utf8_invalid_continuation(fourth__14)
                                    jp1383 = t1390
                                }
                                var jp1377 bool
                                if jp1383 {
                                    jp1377 = true
                                } else {
                                    var t1386 bool = first__8 == 240
                                    if t1386 {
                                        var t1387 bool = second__12 < 144
                                        jp1377 = t1387
                                    } else {
                                        jp1377 = false
                                    }
                                }
                                var jp1363 bool
                                if jp1377 {
                                    jp1363 = true
                                } else {
                                    var t1380 bool = first__8 == 244
                                    if t1380 {
                                        var t1381 bool = second__12 > 143
                                        jp1363 = t1381
                                    } else {
                                        jp1363 = false
                                    }
                                }
                                if jp1363 {
                                    var t1364 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1364
                                } else {
                                    var t1365_rhs uint32 = 7
                                    var t1365 uint32 = first__8 & t1365_rhs
                                    var t1366_rhs int = 18
                                    var t1366 uint32 = t1365 << t1366_rhs
                                    var t1367_rhs uint32 = 63
                                    var t1367 uint32 = second__12 & t1367_rhs
                                    var t1368_rhs int = 12
                                    var t1368 uint32 = t1367 << t1368_rhs
                                    var t1369 uint32 = t1366 | t1368
                                    var t1370_rhs uint32 = 63
                                    var t1370 uint32 = third__13 & t1370_rhs
                                    var t1371_rhs int = 6
                                    var t1371 uint32 = t1370 << t1371_rhs
                                    var t1372 uint32 = t1369 | t1371
                                    var t1373_rhs uint32 = 63
                                    var t1373 uint32 = fourth__14 & t1373_rhs
                                    var t1374 uint32 = t1372 | t1373
                                    var t1375 Tuple3_4bool_4char_3int = utf8_valid_decode(t1374, 4)
                                    return t1375
                                }
                            }
                        } else {
                            var t1396 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1396
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

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1412 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1412
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1415 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1415
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1966 rune
    var inline1934 bool = utf8_valid_scalar(value__0)
    if inline1934 {
        var inline1935 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1936 rune = inline1935._1
        commute_field1966 = inline1936
        var t1421 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1966,
            _2: width__1,
        }
        return t1421
    } else {
        var inline1932 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1932
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1426 bool = value__3 < 128
    if t1426 {
        return true
    } else {
        var t1427 bool = value__3 > 191
        return t1427
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1432 bool
    var inline1940 bool = value__30 <= 1114111
    if inline1940 {
        var inline1941 bool = value__30 >= 55296
        var inline1943 bool
        if inline1941 {
            var inline1945 bool = value__30 <= 57343
            inline1943 = inline1945
        } else {
            inline1943 = false
        }
        var inline1944 bool = !inline1943
        t1432 = inline1944
    } else {
        t1432 = false
    }
    if t1432 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1433 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1433
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1438 bool = value__4 <= 1114111
    if t1438 {
        var t1442 bool = value__4 >= 55296
        var jp1440 bool
        if t1442 {
            var t1443 bool = value__4 <= 57343
            jp1440 = t1443
        } else {
            jp1440 = false
        }
        var t1441 bool = !jp1440
        return t1441
    } else {
        return false
    }
}

func main() {
    main0()
}
