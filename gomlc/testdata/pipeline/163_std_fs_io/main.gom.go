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

type _goml_m_Result____Vec_l_string_r_____string interface {
    is_goml_m_Result____Vec_l_string_r_____string()
}

type _goml_m_Result____Vec_l_string_r_____string_Ok struct {
    _0 *_goml_vec_string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Ok) is_goml_m_Result____Vec_l_string_r_____string() {}

type _goml_m_Result____Vec_l_string_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Err) is_goml_m_Result____Vec_l_string_r_____string() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t425 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t425
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t428 *_goml_vec_uint8
    var inline1443 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t428 = inline1443
    var t429 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t428,
    }
    return t429
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t501 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t501
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t531 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t531
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t543 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t543
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1959 _goml_m_std_p_bytes_p_Bytes
    var commute_field1961 string
    var inline1775 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline1776 bool = inline1775._0
    var inline1777 *_goml_vec_uint8 = inline1775._1
    var inline1778 string = inline1775._2
    if inline1776 {
        var inline1782 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1777)
        commute_field1959 = inline1782
        var inline1766 *_goml_vec_uint8 = commute_field1959.values
        var inline1767 Tuple2_4bool_6string = string_from_utf8(inline1766)
        var inline1768 bool = inline1767._0
        var inline1769 string = inline1767._1
        if inline1768 {
            var inline1772 Result__string__string = Result__string__string_Ok{
                _0: inline1769,
            }
            return inline1772
        } else {
            var inline1773 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1773
        }
    } else {
        commute_field1961 = inline1778
        var t1108 Result__string__string = Result__string__string_Err{
            _0: commute_field1961,
        }
        return t1108
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1122 *_goml_vec_uint8
    var inline1804 *_goml_vec_uint8 = data__123.values
    t1122 = inline1804
    var mtmp70 Tuple2_4bool_6string
    var inline1802 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1122)
    mtmp70 = inline1802
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1125 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1125
    } else {
        var t1126 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t1126
    }
}

func main0() struct{} {
    var inline1868 string = "goml-std-test.txt"
    var inline1869 string = "std-ok"
    var inline1870 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1869)
    _goml_m_std_p_fs_p_write__bytes(inline1868, inline1870)
    var t1186 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t1187 string
    switch t1186.(type) {
    case Result__string__string_Ok:
        var inline1862 string = t1186.(Result__string__string_Ok)._0
        t1187 = inline1862
    case Result__string__string_Err:
        var inline1864 string = t1186.(Result__string__string_Err)._0
        var inline1866 string = "err " + inline1864
        t1187 = inline1866
    default:
        panic("non-exhaustive match")
    }
    var inline1859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1187)
    _goml_m_std_p_internal_p_host_p_println(inline1859)
    var t1188 bool
    var inline1856 string = "goml-std-test.txt"
    var inline1857 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline1856)
    t1188 = inline1857
    var t1189 string
    var inline1854 string = _goml_runtime_core_bool_to_string(t1188)
    t1189 = inline1854
    var inline1851 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1189)
    _goml_m_std_p_internal_p_host_p_println(inline1851)
    var t1190 _goml_m_Result____Vec_l_string_r_____string
    var inline1840 string = "."
    var inline1841 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline1840)
    var inline1842 bool = inline1841._0
    var inline1843 *_goml_vec_string = inline1841._1
    var inline1844 string = inline1841._2
    if inline1842 {
        var inline1848 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1843,
        }
        t1190 = inline1848
    } else {
        var inline1849 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1844,
        }
        t1190 = inline1849
    }
    var t1191 string
    switch t1190.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1831 *_goml_vec_string = t1190.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1833 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1831)
        var inline1834 bool = inline1833 > 0
        var inline1835 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1834)
        t1191 = inline1835
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1836 string = t1190.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1838 string = "err " + inline1836
        t1191 = inline1838
    default:
        panic("non-exhaustive match")
    }
    var inline1828 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1191)
    _goml_m_std_p_internal_p_host_p_println(inline1828)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1234:
    for {
        var t1235 int
        var inline1880 int = _goml_runtime_core_string_len(x12)
        t1235 = inline1880
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__273 *_goml_vec_string) int {
    var t1266 int = vec_len__Vec_6string(self__273)
    return t1266
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1269 string = _goml_runtime_core_bool_to_string(self__148)
    return t1269
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1275 int = _goml_runtime_core_string_len(self__36)
    return t1275
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1394 bool = index__6 < 0
    var jp1392 bool
    if t1394 {
        jp1392 = true
    } else {
        var t1395 bool = index__6 >= length__7
        jp1392 = t1395
    }
    if jp1392 {
        var inline1889 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1889
    } else {
        var t1279 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1279))
        var t1282 bool = first__8 < 128
        if t1282 {
            var inline1891 int = 1
            var inline1892 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1892.(type) {
            case Option__char_None:
                var inline1893 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1893
            case Option__char_Some:
                var inline1894 rune = inline1892.(Option__char_Some)._0
                var inline1896 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1894,
                    _2: inline1891,
                }
                return inline1896
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1286 bool = first__8 < 194
            if t1286 {
                var inline1898 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1898
            } else {
                var t1290 bool = first__8 < 224
                if t1290 {
                    var t1303 int = length__7 - index__6
                    var t1304 bool = t1303 < 2
                    if t1304 {
                        var inline1900 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1900
                    } else {
                        var t1292 int = index__6 + 1
                        var t1293 uint8
                        var inline1914 uint8 = _goml_runtime_core_string_byte_get(value__5, t1292)
                        t1293 = inline1914
                        var second__9 uint32 = uint32(uint8(t1293))
                        var t1296 bool
                        var inline1911 bool = second__9 < 128
                        if inline1911 {
                            t1296 = true
                        } else {
                            var inline1912 bool = second__9 > 191
                            t1296 = inline1912
                        }
                        if t1296 {
                            var inline1902 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1902
                        } else {
                            var t1298_rhs uint32 = 31
                            var t1298 uint32 = first__8 & t1298_rhs
                            var t1299_rhs int = 6
                            var t1299 uint32 = t1298 << t1299_rhs
                            var t1300_rhs uint32 = 63
                            var t1300 uint32 = second__9 & t1300_rhs
                            var t1301 uint32 = t1299 | t1300
                            var inline1904 int = 2
                            var inline1905 Option__char = __goml_builtin_char_from_uint32(t1301)
                            switch inline1905.(type) {
                            case Option__char_None:
                                var inline1906 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1906
                            case Option__char_Some:
                                var inline1907 rune = inline1905.(Option__char_Some)._0
                                var inline1909 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1907,
                                    _2: inline1904,
                                }
                                return inline1909
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1308 bool = first__8 < 240
                    if t1308 {
                        var t1341 int = length__7 - index__6
                        var t1342 bool = t1341 < 3
                        if t1342 {
                            var inline1916 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1916
                        } else {
                            var t1310 int = index__6 + 1
                            var t1311 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1310)
                            var second__10 uint32 = uint32(uint8(t1311))
                            var t1312 int = index__6 + 2
                            var t1313 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1312)
                            var third__11 uint32 = uint32(uint8(t1313))
                            var t1339 bool = utf8_invalid_continuation(second__10)
                            var jp1334 bool
                            if t1339 {
                                jp1334 = true
                            } else {
                                var inline1918 bool = third__11 < 128
                                if inline1918 {
                                    jp1334 = true
                                } else {
                                    var inline1919 bool = third__11 > 191
                                    jp1334 = inline1919
                                }
                            }
                            var jp1328 bool
                            if jp1334 {
                                jp1328 = true
                            } else {
                                var t1337 bool = first__8 == 224
                                if t1337 {
                                    var t1338 bool = second__10 < 160
                                    jp1328 = t1338
                                } else {
                                    jp1328 = false
                                }
                            }
                            var jp1317 bool
                            if jp1328 {
                                jp1317 = true
                            } else {
                                var t1331 bool = first__8 == 237
                                if t1331 {
                                    var t1332 bool = second__10 >= 160
                                    jp1317 = t1332
                                } else {
                                    jp1317 = false
                                }
                            }
                            if jp1317 {
                                var inline1921 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1921
                            } else {
                                var t1319_rhs uint32 = 15
                                var t1319 uint32 = first__8 & t1319_rhs
                                var t1320_rhs int = 12
                                var t1320 uint32 = t1319 << t1320_rhs
                                var t1321_rhs uint32 = 63
                                var t1321 uint32 = second__10 & t1321_rhs
                                var t1322_rhs int = 6
                                var t1322 uint32 = t1321 << t1322_rhs
                                var t1323 uint32 = t1320 | t1322
                                var t1324_rhs uint32 = 63
                                var t1324 uint32 = third__11 & t1324_rhs
                                var t1325 uint32 = t1323 | t1324
                                var inline1923 int = 3
                                var inline1924 Option__char = __goml_builtin_char_from_uint32(t1325)
                                switch inline1924.(type) {
                                case Option__char_None:
                                    var inline1925 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1925
                                case Option__char_Some:
                                    var inline1926 rune = inline1924.(Option__char_Some)._0
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
                        var t1346 bool = first__8 < 245
                        if t1346 {
                            var t1387 int = length__7 - index__6
                            var t1388 bool = t1387 < 4
                            if t1388 {
                                var t1389 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1389
                            } else {
                                var t1348 int = index__6 + 1
                                var t1349 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1348)
                                var second__12 uint32 = uint32(uint8(t1349))
                                var t1350 int = index__6 + 2
                                var t1351 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1350)
                                var third__13 uint32 = uint32(uint8(t1351))
                                var t1352 int = index__6 + 3
                                var t1353 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1352)
                                var fourth__14 uint32 = uint32(uint8(t1353))
                                var t1385 bool = utf8_invalid_continuation(second__12)
                                var jp1383 bool
                                if t1385 {
                                    jp1383 = true
                                } else {
                                    var t1386 bool = utf8_invalid_continuation(third__13)
                                    jp1383 = t1386
                                }
                                var jp1377 bool
                                if jp1383 {
                                    jp1377 = true
                                } else {
                                    var t1384 bool = utf8_invalid_continuation(fourth__14)
                                    jp1377 = t1384
                                }
                                var jp1371 bool
                                if jp1377 {
                                    jp1371 = true
                                } else {
                                    var t1380 bool = first__8 == 240
                                    if t1380 {
                                        var t1381 bool = second__12 < 144
                                        jp1371 = t1381
                                    } else {
                                        jp1371 = false
                                    }
                                }
                                var jp1357 bool
                                if jp1371 {
                                    jp1357 = true
                                } else {
                                    var t1374 bool = first__8 == 244
                                    if t1374 {
                                        var t1375 bool = second__12 > 143
                                        jp1357 = t1375
                                    } else {
                                        jp1357 = false
                                    }
                                }
                                if jp1357 {
                                    var t1358 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1358
                                } else {
                                    var t1359_rhs uint32 = 7
                                    var t1359 uint32 = first__8 & t1359_rhs
                                    var t1360_rhs int = 18
                                    var t1360 uint32 = t1359 << t1360_rhs
                                    var t1361_rhs uint32 = 63
                                    var t1361 uint32 = second__12 & t1361_rhs
                                    var t1362_rhs int = 12
                                    var t1362 uint32 = t1361 << t1362_rhs
                                    var t1363 uint32 = t1360 | t1362
                                    var t1364_rhs uint32 = 63
                                    var t1364 uint32 = third__13 & t1364_rhs
                                    var t1365_rhs int = 6
                                    var t1365 uint32 = t1364 << t1365_rhs
                                    var t1366 uint32 = t1363 | t1365
                                    var t1367_rhs uint32 = 63
                                    var t1367 uint32 = fourth__14 & t1367_rhs
                                    var t1368 uint32 = t1366 | t1367
                                    var t1369 Tuple3_4bool_4char_3int = utf8_valid_decode(t1368, 4)
                                    return t1369
                                }
                            }
                        } else {
                            var t1390 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1390
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
    var t1406 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1406
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1409 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1409
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1964 rune
    var inline1932 bool = utf8_valid_scalar(value__0)
    if inline1932 {
        var inline1933 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1934 rune = inline1933._1
        commute_field1964 = inline1934
        var t1415 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1964,
            _2: width__1,
        }
        return t1415
    } else {
        var inline1930 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1930
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1420 bool = value__3 < 128
    if t1420 {
        return true
    } else {
        var t1421 bool = value__3 > 191
        return t1421
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1426 bool
    var inline1938 bool = value__30 <= 1114111
    if inline1938 {
        var inline1939 bool = value__30 >= 55296
        var inline1941 bool
        if inline1939 {
            var inline1943 bool = value__30 <= 57343
            inline1941 = inline1943
        } else {
            inline1941 = false
        }
        var inline1942 bool = !inline1941
        t1426 = inline1942
    } else {
        t1426 = false
    }
    if t1426 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1427 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1427
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1432 bool = value__4 <= 1114111
    if t1432 {
        var t1436 bool = value__4 >= 55296
        var jp1434 bool
        if t1436 {
            var t1437 bool = value__4 <= 57343
            jp1434 = t1437
        } else {
            jp1434 = false
        }
        var t1435 bool = !jp1434
        return t1435
    } else {
        return false
    }
}

func main() {
    main0()
}
