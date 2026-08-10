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
    var t189 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t189
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t192 *_goml_vec_uint8
    var inline1207 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t192 = inline1207
    var t193 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t192,
    }
    return t193
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t265 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t265
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t295 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t295
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t307 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t307
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1723 _goml_m_std_p_bytes_p_Bytes
    var commute_field1725 string
    var inline1539 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline1540 bool = inline1539._0
    var inline1541 *_goml_vec_uint8 = inline1539._1
    var inline1542 string = inline1539._2
    if inline1540 {
        var inline1546 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1541)
        commute_field1723 = inline1546
        var inline1530 *_goml_vec_uint8 = commute_field1723.values
        var inline1531 Tuple2_4bool_6string = string_from_utf8(inline1530)
        var inline1532 bool = inline1531._0
        var inline1533 string = inline1531._1
        if inline1532 {
            var inline1536 Result__string__string = Result__string__string_Ok{
                _0: inline1533,
            }
            return inline1536
        } else {
            var inline1537 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1537
        }
    } else {
        commute_field1725 = inline1542
        var t872 Result__string__string = Result__string__string_Err{
            _0: commute_field1725,
        }
        return t872
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t886 *_goml_vec_uint8
    var inline1568 *_goml_vec_uint8 = data__123.values
    t886 = inline1568
    var mtmp70 Tuple2_4bool_6string
    var inline1566 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t886)
    mtmp70 = inline1566
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t889 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t889
    } else {
        var t890 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t890
    }
}

func main0() struct{} {
    var inline1632 string = "goml-std-test.txt"
    var inline1633 string = "std-ok"
    var inline1634 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1633)
    _goml_m_std_p_fs_p_write__bytes(inline1632, inline1634)
    var t950 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t951 string
    switch t950.(type) {
    case Result__string__string_Ok:
        var inline1626 string = t950.(Result__string__string_Ok)._0
        t951 = inline1626
    case Result__string__string_Err:
        var inline1628 string = t950.(Result__string__string_Err)._0
        var inline1630 string = "err " + inline1628
        t951 = inline1630
    default:
        panic("non-exhaustive match")
    }
    var inline1623 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t951)
    _goml_m_std_p_internal_p_host_p_println(inline1623)
    var t952 bool
    var inline1620 string = "goml-std-test.txt"
    var inline1621 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline1620)
    t952 = inline1621
    var t953 string
    var inline1618 string = _goml_runtime_core_bool_to_string(t952)
    t953 = inline1618
    var inline1615 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t953)
    _goml_m_std_p_internal_p_host_p_println(inline1615)
    var t954 _goml_m_Result____Vec_l_string_r_____string
    var inline1604 string = "."
    var inline1605 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline1604)
    var inline1606 bool = inline1605._0
    var inline1607 *_goml_vec_string = inline1605._1
    var inline1608 string = inline1605._2
    if inline1606 {
        var inline1612 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1607,
        }
        t954 = inline1612
    } else {
        var inline1613 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1608,
        }
        t954 = inline1613
    }
    var t955 string
    switch t954.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1595 *_goml_vec_string = t954.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1597 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1595)
        var inline1598 bool = inline1597 > 0
        var inline1599 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1598)
        t955 = inline1599
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1600 string = t954.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1602 string = "err " + inline1600
        t955 = inline1602
    default:
        panic("non-exhaustive match")
    }
    var inline1592 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t955)
    _goml_m_std_p_internal_p_host_p_println(inline1592)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop998:
    for {
        var t999 int
        var inline1644 int = _goml_runtime_core_string_len(x12)
        t999 = inline1644
        var t1000 bool = index__26 < t999
        if t1000 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1002 int = compound_old17 + x16
                index__26 = t1002
                continue
            } else {
                var t1004 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1004
            }
        } else {
            break Loop_loop998
        }
    }
    var t997 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t997
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__185 *_goml_vec_string) int {
    var t1030 int = vec_len__Vec_6string(self__185)
    return t1030
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1033 string = _goml_runtime_core_bool_to_string(self__64)
    return t1033
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1039 int = _goml_runtime_core_string_len(self__36)
    return t1039
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1158 bool = index__6 < 0
    var jp1156 bool
    if t1158 {
        jp1156 = true
    } else {
        var t1159 bool = index__6 >= length__7
        jp1156 = t1159
    }
    if jp1156 {
        var inline1653 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1653
    } else {
        var t1043 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1043))
        var t1046 bool = first__8 < 128
        if t1046 {
            var inline1655 int = 1
            var inline1656 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1656.(type) {
            case Option__char_None:
                var inline1657 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1657
            case Option__char_Some:
                var inline1658 rune = inline1656.(Option__char_Some)._0
                var inline1660 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1658,
                    _2: inline1655,
                }
                return inline1660
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1050 bool = first__8 < 194
            if t1050 {
                var inline1662 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1662
            } else {
                var t1054 bool = first__8 < 224
                if t1054 {
                    var t1067 int = length__7 - index__6
                    var t1068 bool = t1067 < 2
                    if t1068 {
                        var inline1664 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1664
                    } else {
                        var t1056 int = index__6 + 1
                        var t1057 uint8
                        var inline1678 uint8 = _goml_runtime_core_string_byte_get(value__5, t1056)
                        t1057 = inline1678
                        var second__9 uint32 = uint32(uint8(t1057))
                        var t1060 bool
                        var inline1675 bool = second__9 < 128
                        if inline1675 {
                            t1060 = true
                        } else {
                            var inline1676 bool = second__9 > 191
                            t1060 = inline1676
                        }
                        if t1060 {
                            var inline1666 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1666
                        } else {
                            var t1062_rhs uint32 = 31
                            var t1062 uint32 = first__8 & t1062_rhs
                            var t1063_rhs int = 6
                            var t1063 uint32 = t1062 << t1063_rhs
                            var t1064_rhs uint32 = 63
                            var t1064 uint32 = second__9 & t1064_rhs
                            var t1065 uint32 = t1063 | t1064
                            var inline1668 int = 2
                            var inline1669 Option__char = __goml_builtin_char_from_uint32(t1065)
                            switch inline1669.(type) {
                            case Option__char_None:
                                var inline1670 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1670
                            case Option__char_Some:
                                var inline1671 rune = inline1669.(Option__char_Some)._0
                                var inline1673 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1671,
                                    _2: inline1668,
                                }
                                return inline1673
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1072 bool = first__8 < 240
                    if t1072 {
                        var t1105 int = length__7 - index__6
                        var t1106 bool = t1105 < 3
                        if t1106 {
                            var inline1680 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1680
                        } else {
                            var t1074 int = index__6 + 1
                            var t1075 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1074)
                            var second__10 uint32 = uint32(uint8(t1075))
                            var t1076 int = index__6 + 2
                            var t1077 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1076)
                            var third__11 uint32 = uint32(uint8(t1077))
                            var t1103 bool = utf8_invalid_continuation(second__10)
                            var jp1098 bool
                            if t1103 {
                                jp1098 = true
                            } else {
                                var inline1682 bool = third__11 < 128
                                if inline1682 {
                                    jp1098 = true
                                } else {
                                    var inline1683 bool = third__11 > 191
                                    jp1098 = inline1683
                                }
                            }
                            var jp1092 bool
                            if jp1098 {
                                jp1092 = true
                            } else {
                                var t1101 bool = first__8 == 224
                                if t1101 {
                                    var t1102 bool = second__10 < 160
                                    jp1092 = t1102
                                } else {
                                    jp1092 = false
                                }
                            }
                            var jp1081 bool
                            if jp1092 {
                                jp1081 = true
                            } else {
                                var t1095 bool = first__8 == 237
                                if t1095 {
                                    var t1096 bool = second__10 >= 160
                                    jp1081 = t1096
                                } else {
                                    jp1081 = false
                                }
                            }
                            if jp1081 {
                                var inline1685 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1685
                            } else {
                                var t1083_rhs uint32 = 15
                                var t1083 uint32 = first__8 & t1083_rhs
                                var t1084_rhs int = 12
                                var t1084 uint32 = t1083 << t1084_rhs
                                var t1085_rhs uint32 = 63
                                var t1085 uint32 = second__10 & t1085_rhs
                                var t1086_rhs int = 6
                                var t1086 uint32 = t1085 << t1086_rhs
                                var t1087 uint32 = t1084 | t1086
                                var t1088_rhs uint32 = 63
                                var t1088 uint32 = third__11 & t1088_rhs
                                var t1089 uint32 = t1087 | t1088
                                var inline1687 int = 3
                                var inline1688 Option__char = __goml_builtin_char_from_uint32(t1089)
                                switch inline1688.(type) {
                                case Option__char_None:
                                    var inline1689 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1689
                                case Option__char_Some:
                                    var inline1690 rune = inline1688.(Option__char_Some)._0
                                    var inline1692 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1690,
                                        _2: inline1687,
                                    }
                                    return inline1692
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1110 bool = first__8 < 245
                        if t1110 {
                            var t1151 int = length__7 - index__6
                            var t1152 bool = t1151 < 4
                            if t1152 {
                                var t1153 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1153
                            } else {
                                var t1112 int = index__6 + 1
                                var t1113 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1112)
                                var second__12 uint32 = uint32(uint8(t1113))
                                var t1114 int = index__6 + 2
                                var t1115 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1114)
                                var third__13 uint32 = uint32(uint8(t1115))
                                var t1116 int = index__6 + 3
                                var t1117 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1116)
                                var fourth__14 uint32 = uint32(uint8(t1117))
                                var t1149 bool = utf8_invalid_continuation(second__12)
                                var jp1147 bool
                                if t1149 {
                                    jp1147 = true
                                } else {
                                    var t1150 bool = utf8_invalid_continuation(third__13)
                                    jp1147 = t1150
                                }
                                var jp1141 bool
                                if jp1147 {
                                    jp1141 = true
                                } else {
                                    var t1148 bool = utf8_invalid_continuation(fourth__14)
                                    jp1141 = t1148
                                }
                                var jp1135 bool
                                if jp1141 {
                                    jp1135 = true
                                } else {
                                    var t1144 bool = first__8 == 240
                                    if t1144 {
                                        var t1145 bool = second__12 < 144
                                        jp1135 = t1145
                                    } else {
                                        jp1135 = false
                                    }
                                }
                                var jp1121 bool
                                if jp1135 {
                                    jp1121 = true
                                } else {
                                    var t1138 bool = first__8 == 244
                                    if t1138 {
                                        var t1139 bool = second__12 > 143
                                        jp1121 = t1139
                                    } else {
                                        jp1121 = false
                                    }
                                }
                                if jp1121 {
                                    var t1122 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1122
                                } else {
                                    var t1123_rhs uint32 = 7
                                    var t1123 uint32 = first__8 & t1123_rhs
                                    var t1124_rhs int = 18
                                    var t1124 uint32 = t1123 << t1124_rhs
                                    var t1125_rhs uint32 = 63
                                    var t1125 uint32 = second__12 & t1125_rhs
                                    var t1126_rhs int = 12
                                    var t1126 uint32 = t1125 << t1126_rhs
                                    var t1127 uint32 = t1124 | t1126
                                    var t1128_rhs uint32 = 63
                                    var t1128 uint32 = third__13 & t1128_rhs
                                    var t1129_rhs int = 6
                                    var t1129 uint32 = t1128 << t1129_rhs
                                    var t1130 uint32 = t1127 | t1129
                                    var t1131_rhs uint32 = 63
                                    var t1131 uint32 = fourth__14 & t1131_rhs
                                    var t1132 uint32 = t1130 | t1131
                                    var t1133 Tuple3_4bool_4char_3int = utf8_valid_decode(t1132, 4)
                                    return t1133
                                }
                            }
                        } else {
                            var t1154 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1154
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1170 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1170
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1173 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1173
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1728 rune
    var inline1696 bool = utf8_valid_scalar(value__0)
    if inline1696 {
        var inline1697 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1698 rune = inline1697._1
        commute_field1728 = inline1698
        var t1179 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1728,
            _2: width__1,
        }
        return t1179
    } else {
        var inline1694 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1694
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1184 bool = value__3 < 128
    if t1184 {
        return true
    } else {
        var t1185 bool = value__3 > 191
        return t1185
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1190 bool
    var inline1702 bool = value__30 <= 1114111
    if inline1702 {
        var inline1703 bool = value__30 >= 55296
        var inline1705 bool
        if inline1703 {
            var inline1707 bool = value__30 <= 57343
            inline1705 = inline1707
        } else {
            inline1705 = false
        }
        var inline1706 bool = !inline1705
        t1190 = inline1706
    } else {
        t1190 = false
    }
    if t1190 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1191 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1191
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1196 bool = value__4 <= 1114111
    if t1196 {
        var t1200 bool = value__4 >= 55296
        var jp1198 bool
        if t1200 {
            var t1201 bool = value__4 <= 57343
            jp1198 = t1201
        } else {
            jp1198 = false
        }
        var t1199 bool = !jp1198
        return t1199
    } else {
        return false
    }
}

func main() {
    main0()
}
