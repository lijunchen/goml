package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
)

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

type _goml_vec__goml_m_std_p_fs_p_DirEntry struct {
    items []_goml_m_std_p_fs_p_DirEntry
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
}

type Tuple2_14Option__string_14Option__string struct {
    _0 Option__string
    _1 Option__string
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

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
}

type Tuple2_27_goml_m_std_p_fs_p_FileType_27_goml_m_std_p_fs_p_FileType struct {
    _0 _goml_m_std_p_fs_p_FileType
    _1 _goml_m_std_p_fs_p_FileType
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

type Tuple6_4bool_11Vec_6string_3int_4bool_3int_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 int
    _3 bool
    _4 int
    _5 string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t193 *_goml_vec_uint8
    var inline1052 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t193 = inline1052
    var t194 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t193,
    }
    return t194
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t238 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t238)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t241 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t241
    } else {
        var t242 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t242
    }
}

func _goml_m_std_p_fs_p_read__bytes(path__118 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp66 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t693 _goml_m_std_p_bytes_p_Bytes
        var inline1398 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t693 = inline1398
        var t694 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t693,
        }
        return t694
    } else {
        var t695 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x69,
        }
        return t695
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t698 *_goml_vec_uint8
    var inline1400 *_goml_vec_uint8 = data__123.values
    t698 = inline1400
    var mtmp70 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t698)
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t701 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t701
    } else {
        var t702 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t702
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t707 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t707
    } else {
        var t708 Result__unit__string = Result__unit__string_Err{
            _0: x75,
        }
        return t708
    }
}

func main0() struct{} {
    var t759 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t759)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1441 string = ""
    var inline1442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1441)
    var inline1443 string = inline1442 + "\n"
    _goml_runtime_std_io_eprint(inline1443)
    var t760 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t761 string
    switch t760.(type) {
    case Result__unit__string_Ok:
        t761 = "ok"
    case Result__unit__string_Err:
        var inline1437 string = t760.(Result__unit__string_Err)._0
        var inline1439 string = "err " + inline1437
        t761 = inline1439
    default:
        panic("non-exhaustive match")
    }
    var inline1433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t761)
    _goml_runtime_std_io_println(inline1433)
    var t762 Result__unit__string
    var inline1428 string = "goml-self-host/nested/output.txt"
    var inline1429 string = "boot"
    var inline1430 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1429)
    var inline1431 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1428, inline1430)
    t762 = inline1431
    var t763 string
    switch t762.(type) {
    case Result__unit__string_Ok:
        t763 = "ok"
    case Result__unit__string_Err:
        var inline1424 string = t762.(Result__unit__string_Err)._0
        var inline1426 string = "err " + inline1424
        t763 = inline1426
    default:
        panic("non-exhaustive match")
    }
    var inline1420 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t763)
    _goml_runtime_std_io_println(inline1420)
    var t764 Result__string__string
    var inline1411 string = "goml-self-host/nested/output.txt"
    var inline1412 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1411)
    switch inline1412.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline1413 _goml_m_std_p_bytes_p_Bytes = inline1412.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline1415 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1413)
        t764 = inline1415
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline1416 string = inline1412.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline1418 Result__string__string = Result__string__string_Err{
            _0: inline1416,
        }
        t764 = inline1418
    default:
        panic("non-exhaustive match")
    }
    var t765 string
    switch t764.(type) {
    case Result__string__string_Ok:
        var inline1405 string = t764.(Result__string__string_Ok)._0
        t765 = inline1405
    case Result__string__string_Err:
        var inline1407 string = t764.(Result__string__string_Err)._0
        var inline1409 string = "err " + inline1407
        t765 = inline1409
    default:
        panic("non-exhaustive match")
    }
    var inline1402 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t765)
    _goml_runtime_std_io_println(inline1402)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop809:
    for {
        var t810 int
        var inline1459 int = _goml_runtime_core_string_len(x12)
        t810 = inline1459
        var t811 bool = index__26 < t810
        if t811 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t813 int = compound_old17 + x16
                index__26 = t813
                continue
            } else {
                var t815 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t815
            }
        } else {
            break Loop_loop809
        }
    }
    var t808 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t808
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t842 bool = self__117 == other__118
    return t842
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t849 string
    t849 = value__68
    _goml_runtime_std_io_println(t849)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t859 bool = string_is_char_boundary(value__21, start__22)
    var jp856 bool
    if t859 {
        var t860 bool = string_is_char_boundary(value__21, end__23)
        jp856 = t860
    } else {
        jp856 = false
    }
    if jp856 {
        var t857 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t857
    } else {
        var t858 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t858
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t862 string
    t862 = value__69
    _goml_runtime_std_io_eprint(t862)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t870 int = _goml_runtime_core_string_len(self__38)
    return t870
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t989 bool = index__6 < 0
    var jp987 bool
    if t989 {
        jp987 = true
    } else {
        var t990 bool = index__6 >= length__7
        jp987 = t990
    }
    if jp987 {
        var inline1468 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1468
    } else {
        var t874 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t874))
        var t877 bool = first__8 < 128
        if t877 {
            var inline1470 int = 1
            var inline1471 Option__char = char_from_uint32(first__8)
            switch inline1471.(type) {
            case Option__char_None:
                var inline1472 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1472
            case Option__char_Some:
                var inline1473 rune = inline1471.(Option__char_Some)._0
                var inline1475 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1473,
                    _2: inline1470,
                }
                return inline1475
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t881 bool = first__8 < 194
            if t881 {
                var inline1477 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1477
            } else {
                var t885 bool = first__8 < 224
                if t885 {
                    var t898 int = length__7 - index__6
                    var t899 bool = t898 < 2
                    if t899 {
                        var inline1479 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1479
                    } else {
                        var t887 int = index__6 + 1
                        var t888 uint8
                        var inline1493 uint8 = _goml_runtime_core_string_byte_get(value__5, t887)
                        t888 = inline1493
                        var second__9 uint32 = uint32(uint8(t888))
                        var t891 bool
                        var inline1490 bool = second__9 < 128
                        if inline1490 {
                            t891 = true
                        } else {
                            var inline1491 bool = second__9 > 191
                            t891 = inline1491
                        }
                        if t891 {
                            var inline1481 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1481
                        } else {
                            var t893_rhs uint32 = 31
                            var t893 uint32 = first__8 & t893_rhs
                            var t894_rhs int = 6
                            var t894 uint32 = t893 << t894_rhs
                            var t895_rhs uint32 = 63
                            var t895 uint32 = second__9 & t895_rhs
                            var t896 uint32 = t894 | t895
                            var inline1483 int = 2
                            var inline1484 Option__char = char_from_uint32(t896)
                            switch inline1484.(type) {
                            case Option__char_None:
                                var inline1485 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1485
                            case Option__char_Some:
                                var inline1486 rune = inline1484.(Option__char_Some)._0
                                var inline1488 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1486,
                                    _2: inline1483,
                                }
                                return inline1488
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t903 bool = first__8 < 240
                    if t903 {
                        var t936 int = length__7 - index__6
                        var t937 bool = t936 < 3
                        if t937 {
                            var inline1495 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1495
                        } else {
                            var t905 int = index__6 + 1
                            var t906 uint8
                            var inline1510 uint8 = _goml_runtime_core_string_byte_get(value__5, t905)
                            t906 = inline1510
                            var second__10 uint32 = uint32(uint8(t906))
                            var t907 int = index__6 + 2
                            var t908 uint8
                            var inline1508 uint8 = _goml_runtime_core_string_byte_get(value__5, t907)
                            t908 = inline1508
                            var third__11 uint32 = uint32(uint8(t908))
                            var t934 bool = utf8_invalid_continuation(second__10)
                            var jp929 bool
                            if t934 {
                                jp929 = true
                            } else {
                                var inline1497 bool = third__11 < 128
                                if inline1497 {
                                    jp929 = true
                                } else {
                                    var inline1498 bool = third__11 > 191
                                    jp929 = inline1498
                                }
                            }
                            var jp923 bool
                            if jp929 {
                                jp923 = true
                            } else {
                                var t932 bool
                                var inline1500 uint32 = 224
                                var inline1501 bool = first__8 == inline1500
                                t932 = inline1501
                                if t932 {
                                    var t933 bool = second__10 < 160
                                    jp923 = t933
                                } else {
                                    jp923 = false
                                }
                            }
                            var jp912 bool
                            if jp923 {
                                jp912 = true
                            } else {
                                var t926 bool
                                var inline1503 uint32 = 237
                                var inline1504 bool = first__8 == inline1503
                                t926 = inline1504
                                if t926 {
                                    var t927 bool = second__10 >= 160
                                    jp912 = t927
                                } else {
                                    jp912 = false
                                }
                            }
                            if jp912 {
                                var inline1506 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1506
                            } else {
                                var t914_rhs uint32 = 15
                                var t914 uint32 = first__8 & t914_rhs
                                var t915_rhs int = 12
                                var t915 uint32 = t914 << t915_rhs
                                var t916_rhs uint32 = 63
                                var t916 uint32 = second__10 & t916_rhs
                                var t917_rhs int = 6
                                var t917 uint32 = t916 << t917_rhs
                                var t918 uint32 = t915 | t917
                                var t919_rhs uint32 = 63
                                var t919 uint32 = third__11 & t919_rhs
                                var t920 uint32 = t918 | t919
                                var t921 Tuple3_4bool_4char_3int = utf8_valid_decode(t920, 3)
                                return t921
                            }
                        }
                    } else {
                        var t941 bool = first__8 < 245
                        if t941 {
                            var t982 int = length__7 - index__6
                            var t983 bool = t982 < 4
                            if t983 {
                                var t984 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t984
                            } else {
                                var t943 int = index__6 + 1
                                var t944 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t943)
                                var second__12 uint32 = uint32(uint8(t944))
                                var t945 int = index__6 + 2
                                var t946 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t945)
                                var third__13 uint32 = uint32(uint8(t946))
                                var t947 int = index__6 + 3
                                var t948 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t947)
                                var fourth__14 uint32 = uint32(uint8(t948))
                                var t980 bool = utf8_invalid_continuation(second__12)
                                var jp978 bool
                                if t980 {
                                    jp978 = true
                                } else {
                                    var t981 bool = utf8_invalid_continuation(third__13)
                                    jp978 = t981
                                }
                                var jp972 bool
                                if jp978 {
                                    jp972 = true
                                } else {
                                    var t979 bool = utf8_invalid_continuation(fourth__14)
                                    jp972 = t979
                                }
                                var jp966 bool
                                if jp972 {
                                    jp966 = true
                                } else {
                                    var t975 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t975 {
                                        var t976 bool = second__12 < 144
                                        jp966 = t976
                                    } else {
                                        jp966 = false
                                    }
                                }
                                var jp952 bool
                                if jp966 {
                                    jp952 = true
                                } else {
                                    var t969 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t969 {
                                        var t970 bool = second__12 > 143
                                        jp952 = t970
                                    } else {
                                        jp952 = false
                                    }
                                }
                                if jp952 {
                                    var t953 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t953
                                } else {
                                    var t954_rhs uint32 = 7
                                    var t954 uint32 = first__8 & t954_rhs
                                    var t955_rhs int = 18
                                    var t955 uint32 = t954 << t955_rhs
                                    var t956_rhs uint32 = 63
                                    var t956 uint32 = second__12 & t956_rhs
                                    var t957_rhs int = 12
                                    var t957 uint32 = t956 << t957_rhs
                                    var t958 uint32 = t955 | t957
                                    var t959_rhs uint32 = 63
                                    var t959 uint32 = third__13 & t959_rhs
                                    var t960_rhs int = 6
                                    var t960 uint32 = t959 << t960_rhs
                                    var t961 uint32 = t958 | t960
                                    var t962_rhs uint32 = 63
                                    var t962 uint32 = fourth__14 & t962_rhs
                                    var t963 uint32 = t961 | t962
                                    var t964 Tuple3_4bool_4char_3int = utf8_valid_decode(t963, 4)
                                    return t964
                                }
                            }
                        } else {
                            var t985 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t985
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1007 bool = index__16 < 0
    var jp998 bool
    if t1007 {
        jp998 = true
    } else {
        var t1008 int
        var inline1512 int = _goml_runtime_core_string_len(value__15)
        t1008 = inline1512
        var t1009 bool = index__16 > t1008
        jp998 = t1009
    }
    if jp998 {
        return false
    } else {
        var t1001 int
        var inline1521 int = _goml_runtime_core_string_len(value__15)
        t1001 = inline1521
        var t1002 bool
        var inline1519 bool = index__16 == t1001
        t1002 = inline1519
        if t1002 {
            return true
        } else {
            var t1003 uint8
            var inline1517 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1003 = inline1517
            var t1004_rhs uint8 = 192
            var t1004 uint8 = t1003 & t1004_rhs
            var t1005 bool
            var inline1514 uint8 = 128
            var inline1515 bool = t1004 == inline1514
            t1005 = inline1515
            var t1006 bool = !t1005
            return t1006
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1012 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1012
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1015 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1015
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1559 rune
    var inline1525 bool = utf8_valid_scalar(value__0)
    if inline1525 {
        var inline1526 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1528 rune = inline1526._1
        commute_field1559 = inline1528
        var t1021 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1559,
            _2: width__1,
        }
        return t1021
    } else {
        var inline1523 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1523
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1026 bool = value__3 < 128
    if t1026 {
        return true
    } else {
        var t1027 bool = value__3 > 191
        return t1027
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1035 bool
    var inline1532 bool = value__32 <= 1114111
    if inline1532 {
        var inline1533 bool = value__32 >= 55296
        var inline1535 bool
        if inline1533 {
            var inline1537 bool = value__32 <= 57343
            inline1535 = inline1537
        } else {
            inline1535 = false
        }
        var inline1536 bool = !inline1535
        t1035 = inline1536
    } else {
        t1035 = false
    }
    if t1035 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1036 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1036
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1041 bool = value__4 <= 1114111
    if t1041 {
        var t1045 bool = value__4 >= 55296
        var jp1043 bool
        if t1045 {
            var t1046 bool = value__4 <= 57343
            jp1043 = t1046
        } else {
            jp1043 = false
        }
        var t1044 bool = !jp1043
        return t1044
    } else {
        return false
    }
}

func main() {
    main0()
}
