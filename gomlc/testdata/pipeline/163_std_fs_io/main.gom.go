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
    var t427 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t427
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t430 *_goml_vec_uint8
    var inline1438 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t430 = inline1438
    var t431 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t430,
    }
    return t431
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t503 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t503
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t533 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t533
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t545 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t545
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1952 _goml_m_std_p_bytes_p_Bytes
    var commute_field1954 string
    var inline1768 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline1769 bool = inline1768._0
    var inline1770 *_goml_vec_uint8 = inline1768._1
    var inline1771 string = inline1768._2
    if inline1769 {
        var inline1775 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1770)
        commute_field1952 = inline1775
        var inline1759 *_goml_vec_uint8 = commute_field1952.values
        var inline1760 Tuple2_4bool_6string = string_from_utf8(inline1759)
        var inline1761 bool = inline1760._0
        var inline1762 string = inline1760._1
        if inline1761 {
            var inline1765 Result__string__string = Result__string__string_Ok{
                _0: inline1762,
            }
            return inline1765
        } else {
            var inline1766 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1766
        }
    } else {
        commute_field1954 = inline1771
        var t1111 Result__string__string = Result__string__string_Err{
            _0: commute_field1954,
        }
        return t1111
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1125 *_goml_vec_uint8
    var inline1797 *_goml_vec_uint8 = data__123.values
    t1125 = inline1797
    var mtmp70 Tuple2_4bool_6string
    var inline1795 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1125)
    mtmp70 = inline1795
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1128 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1128
    } else {
        var t1129 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t1129
    }
}

func main0() struct{} {
    var inline1861 string = "goml-std-test.txt"
    var inline1862 string = "std-ok"
    var inline1863 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1862)
    _goml_m_std_p_fs_p_write__bytes(inline1861, inline1863)
    var t1189 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t1190 string
    switch t1189.(type) {
    case Result__string__string_Ok:
        var inline1855 string = t1189.(Result__string__string_Ok)._0
        t1190 = inline1855
    case Result__string__string_Err:
        var inline1857 string = t1189.(Result__string__string_Err)._0
        var inline1859 string = "err " + inline1857
        t1190 = inline1859
    default:
        panic("non-exhaustive match")
    }
    var inline1852 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1190)
    _goml_m_std_p_internal_p_host_p_println(inline1852)
    var t1191 bool
    var inline1849 string = "goml-std-test.txt"
    var inline1850 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline1849)
    t1191 = inline1850
    var t1192 string
    var inline1847 string = _goml_runtime_core_bool_to_string(t1191)
    t1192 = inline1847
    var inline1844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1192)
    _goml_m_std_p_internal_p_host_p_println(inline1844)
    var t1193 _goml_m_Result____Vec_l_string_r_____string
    var inline1833 string = "."
    var inline1834 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline1833)
    var inline1835 bool = inline1834._0
    var inline1836 *_goml_vec_string = inline1834._1
    var inline1837 string = inline1834._2
    if inline1835 {
        var inline1841 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1836,
        }
        t1193 = inline1841
    } else {
        var inline1842 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1837,
        }
        t1193 = inline1842
    }
    var t1194 string
    switch t1193.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1824 *_goml_vec_string = t1193.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1826 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1824)
        var inline1827 bool = inline1826 > 0
        var inline1828 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1827)
        t1194 = inline1828
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1829 string = t1193.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1831 string = "err " + inline1829
        t1194 = inline1831
    default:
        panic("non-exhaustive match")
    }
    var inline1821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1194)
    _goml_m_std_p_internal_p_host_p_println(inline1821)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1234:
    for {
        var t1235 int
        var inline1873 int = _goml_runtime_core_string_len(x12)
        t1235 = inline1873
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
    var t1263 int = vec_len__Vec_6string(self__273)
    return t1263
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1266 string = _goml_runtime_core_bool_to_string(self__148)
    return t1266
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1272 int = _goml_runtime_core_string_len(self__36)
    return t1272
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1391 bool = index__6 < 0
    var jp1389 bool
    if t1391 {
        jp1389 = true
    } else {
        var t1392 bool = index__6 >= length__7
        jp1389 = t1392
    }
    if jp1389 {
        var inline1882 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1882
    } else {
        var t1276 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1276))
        var t1279 bool = first__8 < 128
        if t1279 {
            var inline1884 int = 1
            var inline1885 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1885.(type) {
            case Option__char_None:
                var inline1886 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1886
            case Option__char_Some:
                var inline1887 rune = inline1885.(Option__char_Some)._0
                var inline1889 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1887,
                    _2: inline1884,
                }
                return inline1889
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1283 bool = first__8 < 194
            if t1283 {
                var inline1891 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1891
            } else {
                var t1287 bool = first__8 < 224
                if t1287 {
                    var t1300 int = length__7 - index__6
                    var t1301 bool = t1300 < 2
                    if t1301 {
                        var inline1893 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1893
                    } else {
                        var t1289 int = index__6 + 1
                        var t1290 uint8
                        var inline1907 uint8 = _goml_runtime_core_string_byte_get(value__5, t1289)
                        t1290 = inline1907
                        var second__9 uint32 = uint32(uint8(t1290))
                        var t1293 bool
                        var inline1904 bool = second__9 < 128
                        if inline1904 {
                            t1293 = true
                        } else {
                            var inline1905 bool = second__9 > 191
                            t1293 = inline1905
                        }
                        if t1293 {
                            var inline1895 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1895
                        } else {
                            var t1295_rhs uint32 = 31
                            var t1295 uint32 = first__8 & t1295_rhs
                            var t1296_rhs int = 6
                            var t1296 uint32 = t1295 << t1296_rhs
                            var t1297_rhs uint32 = 63
                            var t1297 uint32 = second__9 & t1297_rhs
                            var t1298 uint32 = t1296 | t1297
                            var inline1897 int = 2
                            var inline1898 Option__char = __goml_builtin_char_from_uint32(t1298)
                            switch inline1898.(type) {
                            case Option__char_None:
                                var inline1899 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1899
                            case Option__char_Some:
                                var inline1900 rune = inline1898.(Option__char_Some)._0
                                var inline1902 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1900,
                                    _2: inline1897,
                                }
                                return inline1902
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1305 bool = first__8 < 240
                    if t1305 {
                        var t1338 int = length__7 - index__6
                        var t1339 bool = t1338 < 3
                        if t1339 {
                            var inline1909 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1909
                        } else {
                            var t1307 int = index__6 + 1
                            var t1308 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1307)
                            var second__10 uint32 = uint32(uint8(t1308))
                            var t1309 int = index__6 + 2
                            var t1310 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1309)
                            var third__11 uint32 = uint32(uint8(t1310))
                            var t1336 bool = utf8_invalid_continuation(second__10)
                            var jp1331 bool
                            if t1336 {
                                jp1331 = true
                            } else {
                                var inline1911 bool = third__11 < 128
                                if inline1911 {
                                    jp1331 = true
                                } else {
                                    var inline1912 bool = third__11 > 191
                                    jp1331 = inline1912
                                }
                            }
                            var jp1325 bool
                            if jp1331 {
                                jp1325 = true
                            } else {
                                var t1334 bool = first__8 == 224
                                if t1334 {
                                    var t1335 bool = second__10 < 160
                                    jp1325 = t1335
                                } else {
                                    jp1325 = false
                                }
                            }
                            var jp1314 bool
                            if jp1325 {
                                jp1314 = true
                            } else {
                                var t1328 bool = first__8 == 237
                                if t1328 {
                                    var t1329 bool = second__10 >= 160
                                    jp1314 = t1329
                                } else {
                                    jp1314 = false
                                }
                            }
                            if jp1314 {
                                var inline1914 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1914
                            } else {
                                var t1316_rhs uint32 = 15
                                var t1316 uint32 = first__8 & t1316_rhs
                                var t1317_rhs int = 12
                                var t1317 uint32 = t1316 << t1317_rhs
                                var t1318_rhs uint32 = 63
                                var t1318 uint32 = second__10 & t1318_rhs
                                var t1319_rhs int = 6
                                var t1319 uint32 = t1318 << t1319_rhs
                                var t1320 uint32 = t1317 | t1319
                                var t1321_rhs uint32 = 63
                                var t1321 uint32 = third__11 & t1321_rhs
                                var t1322 uint32 = t1320 | t1321
                                var inline1916 int = 3
                                var inline1917 Option__char = __goml_builtin_char_from_uint32(t1322)
                                switch inline1917.(type) {
                                case Option__char_None:
                                    var inline1918 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1918
                                case Option__char_Some:
                                    var inline1919 rune = inline1917.(Option__char_Some)._0
                                    var inline1921 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1919,
                                        _2: inline1916,
                                    }
                                    return inline1921
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1343 bool = first__8 < 245
                        if t1343 {
                            var t1384 int = length__7 - index__6
                            var t1385 bool = t1384 < 4
                            if t1385 {
                                var t1386 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1386
                            } else {
                                var t1345 int = index__6 + 1
                                var t1346 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1345)
                                var second__12 uint32 = uint32(uint8(t1346))
                                var t1347 int = index__6 + 2
                                var t1348 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1347)
                                var third__13 uint32 = uint32(uint8(t1348))
                                var t1349 int = index__6 + 3
                                var t1350 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1349)
                                var fourth__14 uint32 = uint32(uint8(t1350))
                                var t1382 bool = utf8_invalid_continuation(second__12)
                                var jp1380 bool
                                if t1382 {
                                    jp1380 = true
                                } else {
                                    var t1383 bool = utf8_invalid_continuation(third__13)
                                    jp1380 = t1383
                                }
                                var jp1374 bool
                                if jp1380 {
                                    jp1374 = true
                                } else {
                                    var t1381 bool = utf8_invalid_continuation(fourth__14)
                                    jp1374 = t1381
                                }
                                var jp1368 bool
                                if jp1374 {
                                    jp1368 = true
                                } else {
                                    var t1377 bool = first__8 == 240
                                    if t1377 {
                                        var t1378 bool = second__12 < 144
                                        jp1368 = t1378
                                    } else {
                                        jp1368 = false
                                    }
                                }
                                var jp1354 bool
                                if jp1368 {
                                    jp1354 = true
                                } else {
                                    var t1371 bool = first__8 == 244
                                    if t1371 {
                                        var t1372 bool = second__12 > 143
                                        jp1354 = t1372
                                    } else {
                                        jp1354 = false
                                    }
                                }
                                if jp1354 {
                                    var t1355 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1355
                                } else {
                                    var t1356_rhs uint32 = 7
                                    var t1356 uint32 = first__8 & t1356_rhs
                                    var t1357_rhs int = 18
                                    var t1357 uint32 = t1356 << t1357_rhs
                                    var t1358_rhs uint32 = 63
                                    var t1358 uint32 = second__12 & t1358_rhs
                                    var t1359_rhs int = 12
                                    var t1359 uint32 = t1358 << t1359_rhs
                                    var t1360 uint32 = t1357 | t1359
                                    var t1361_rhs uint32 = 63
                                    var t1361 uint32 = third__13 & t1361_rhs
                                    var t1362_rhs int = 6
                                    var t1362 uint32 = t1361 << t1362_rhs
                                    var t1363 uint32 = t1360 | t1362
                                    var t1364_rhs uint32 = 63
                                    var t1364 uint32 = fourth__14 & t1364_rhs
                                    var t1365 uint32 = t1363 | t1364
                                    var t1366 Tuple3_4bool_4char_3int = utf8_valid_decode(t1365, 4)
                                    return t1366
                                }
                            }
                        } else {
                            var t1387 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1387
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
    var t1403 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1403
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1406 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1406
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1957 rune
    var inline1925 bool = utf8_valid_scalar(value__0)
    if inline1925 {
        var inline1926 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1927 rune = inline1926._1
        commute_field1957 = inline1927
        var t1412 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1957,
            _2: width__1,
        }
        return t1412
    } else {
        var inline1923 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1923
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1417 bool = value__3 < 128
    if t1417 {
        return true
    } else {
        var t1418 bool = value__3 > 191
        return t1418
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1423 bool
    var inline1931 bool = value__30 <= 1114111
    if inline1931 {
        var inline1932 bool = value__30 >= 55296
        var inline1934 bool
        if inline1932 {
            var inline1936 bool = value__30 <= 57343
            inline1934 = inline1936
        } else {
            inline1934 = false
        }
        var inline1935 bool = !inline1934
        t1423 = inline1935
    } else {
        t1423 = false
    }
    if t1423 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1424 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1424
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1429 bool = value__4 <= 1114111
    if t1429 {
        var t1433 bool = value__4 >= 55296
        var jp1431 bool
        if t1433 {
            var t1434 bool = value__4 <= 57343
            jp1431 = t1434
        } else {
            jp1431 = false
        }
        var t1432 bool = !jp1431
        return t1432
    } else {
        return false
    }
}

func main() {
    main0()
}
