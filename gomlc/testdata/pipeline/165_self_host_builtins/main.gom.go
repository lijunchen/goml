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
    var inline1045 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t193 = inline1045
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
        var inline1370 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t693 = inline1370
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
    var inline1372 *_goml_vec_uint8 = data__123.values
    t698 = inline1372
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
    var inline1411 string = ""
    var inline1412 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1411)
    var inline1413 string = inline1412 + "\n"
    _goml_runtime_std_io_eprint(inline1413)
    var t760 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t761 string
    switch t760.(type) {
    case Result__unit__string_Ok:
        t761 = "ok"
    case Result__unit__string_Err:
        var inline1407 string = t760.(Result__unit__string_Err)._0
        var inline1409 string = "err " + inline1407
        t761 = inline1409
    default:
        panic("non-exhaustive match")
    }
    var inline1404 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t761)
    _goml_runtime_std_io_println(inline1404)
    var t762 Result__unit__string
    var inline1399 string = "goml-self-host/nested/output.txt"
    var inline1400 string = "boot"
    var inline1401 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1400)
    var inline1402 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1399, inline1401)
    t762 = inline1402
    var t763 string
    switch t762.(type) {
    case Result__unit__string_Ok:
        t763 = "ok"
    case Result__unit__string_Err:
        var inline1395 string = t762.(Result__unit__string_Err)._0
        var inline1397 string = "err " + inline1395
        t763 = inline1397
    default:
        panic("non-exhaustive match")
    }
    var inline1392 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t763)
    _goml_runtime_std_io_println(inline1392)
    var t764 Result__string__string
    var inline1383 string = "goml-self-host/nested/output.txt"
    var inline1384 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1383)
    switch inline1384.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline1385 _goml_m_std_p_bytes_p_Bytes = inline1384.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline1387 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1385)
        t764 = inline1387
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline1388 string = inline1384.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline1390 Result__string__string = Result__string__string_Err{
            _0: inline1388,
        }
        t764 = inline1390
    default:
        panic("non-exhaustive match")
    }
    var t765 string
    switch t764.(type) {
    case Result__string__string_Ok:
        var inline1377 string = t764.(Result__string__string_Ok)._0
        t765 = inline1377
    case Result__string__string_Err:
        var inline1379 string = t764.(Result__string__string_Err)._0
        var inline1381 string = "err " + inline1379
        t765 = inline1381
    default:
        panic("non-exhaustive match")
    }
    var inline1374 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t765)
    _goml_runtime_std_io_println(inline1374)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop809:
    for {
        var t810 int
        var inline1423 int = _goml_runtime_core_string_len(x12)
        t810 = inline1423
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

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t840 string
    t840 = value__68
    _goml_runtime_std_io_println(t840)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t850 bool = string_is_char_boundary(value__21, start__22)
    var jp847 bool
    if t850 {
        var t851 bool = string_is_char_boundary(value__21, end__23)
        jp847 = t851
    } else {
        jp847 = false
    }
    if jp847 {
        var t848 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t848
    } else {
        var t849 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t849
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t853 string
    t853 = value__69
    _goml_runtime_std_io_eprint(t853)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t861 int = _goml_runtime_core_string_len(self__38)
    return t861
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t980 bool = index__6 < 0
    var jp978 bool
    if t980 {
        jp978 = true
    } else {
        var t981 bool = index__6 >= length__7
        jp978 = t981
    }
    if jp978 {
        var inline1432 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1432
    } else {
        var t865 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t865))
        var t868 bool = first__8 < 128
        if t868 {
            var inline1434 int = 1
            var inline1435 Option__char = char_from_uint32(first__8)
            switch inline1435.(type) {
            case Option__char_None:
                var inline1436 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1436
            case Option__char_Some:
                var inline1437 rune = inline1435.(Option__char_Some)._0
                var inline1439 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1437,
                    _2: inline1434,
                }
                return inline1439
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t872 bool = first__8 < 194
            if t872 {
                var inline1441 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1441
            } else {
                var t876 bool = first__8 < 224
                if t876 {
                    var t889 int = length__7 - index__6
                    var t890 bool = t889 < 2
                    if t890 {
                        var inline1443 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1443
                    } else {
                        var t878 int = index__6 + 1
                        var t879 uint8
                        var inline1457 uint8 = _goml_runtime_core_string_byte_get(value__5, t878)
                        t879 = inline1457
                        var second__9 uint32 = uint32(uint8(t879))
                        var t882 bool
                        var inline1454 bool = second__9 < 128
                        if inline1454 {
                            t882 = true
                        } else {
                            var inline1455 bool = second__9 > 191
                            t882 = inline1455
                        }
                        if t882 {
                            var inline1445 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1445
                        } else {
                            var t884_rhs uint32 = 31
                            var t884 uint32 = first__8 & t884_rhs
                            var t885_rhs int = 6
                            var t885 uint32 = t884 << t885_rhs
                            var t886_rhs uint32 = 63
                            var t886 uint32 = second__9 & t886_rhs
                            var t887 uint32 = t885 | t886
                            var inline1447 int = 2
                            var inline1448 Option__char = char_from_uint32(t887)
                            switch inline1448.(type) {
                            case Option__char_None:
                                var inline1449 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1449
                            case Option__char_Some:
                                var inline1450 rune = inline1448.(Option__char_Some)._0
                                var inline1452 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1450,
                                    _2: inline1447,
                                }
                                return inline1452
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t894 bool = first__8 < 240
                    if t894 {
                        var t927 int = length__7 - index__6
                        var t928 bool = t927 < 3
                        if t928 {
                            var inline1459 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1459
                        } else {
                            var t896 int = index__6 + 1
                            var t897 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t896)
                            var second__10 uint32 = uint32(uint8(t897))
                            var t898 int = index__6 + 2
                            var t899 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t898)
                            var third__11 uint32 = uint32(uint8(t899))
                            var t925 bool = utf8_invalid_continuation(second__10)
                            var jp920 bool
                            if t925 {
                                jp920 = true
                            } else {
                                var inline1461 bool = third__11 < 128
                                if inline1461 {
                                    jp920 = true
                                } else {
                                    var inline1462 bool = third__11 > 191
                                    jp920 = inline1462
                                }
                            }
                            var jp914 bool
                            if jp920 {
                                jp914 = true
                            } else {
                                var t923 bool = first__8 == 224
                                if t923 {
                                    var t924 bool = second__10 < 160
                                    jp914 = t924
                                } else {
                                    jp914 = false
                                }
                            }
                            var jp903 bool
                            if jp914 {
                                jp903 = true
                            } else {
                                var t917 bool = first__8 == 237
                                if t917 {
                                    var t918 bool = second__10 >= 160
                                    jp903 = t918
                                } else {
                                    jp903 = false
                                }
                            }
                            if jp903 {
                                var inline1464 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1464
                            } else {
                                var t905_rhs uint32 = 15
                                var t905 uint32 = first__8 & t905_rhs
                                var t906_rhs int = 12
                                var t906 uint32 = t905 << t906_rhs
                                var t907_rhs uint32 = 63
                                var t907 uint32 = second__10 & t907_rhs
                                var t908_rhs int = 6
                                var t908 uint32 = t907 << t908_rhs
                                var t909 uint32 = t906 | t908
                                var t910_rhs uint32 = 63
                                var t910 uint32 = third__11 & t910_rhs
                                var t911 uint32 = t909 | t910
                                var inline1466 int = 3
                                var inline1467 Option__char = char_from_uint32(t911)
                                switch inline1467.(type) {
                                case Option__char_None:
                                    var inline1468 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1468
                                case Option__char_Some:
                                    var inline1469 rune = inline1467.(Option__char_Some)._0
                                    var inline1471 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1469,
                                        _2: inline1466,
                                    }
                                    return inline1471
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t932 bool = first__8 < 245
                        if t932 {
                            var t973 int = length__7 - index__6
                            var t974 bool = t973 < 4
                            if t974 {
                                var t975 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t975
                            } else {
                                var t934 int = index__6 + 1
                                var t935 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t934)
                                var second__12 uint32 = uint32(uint8(t935))
                                var t936 int = index__6 + 2
                                var t937 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t936)
                                var third__13 uint32 = uint32(uint8(t937))
                                var t938 int = index__6 + 3
                                var t939 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t938)
                                var fourth__14 uint32 = uint32(uint8(t939))
                                var t971 bool = utf8_invalid_continuation(second__12)
                                var jp969 bool
                                if t971 {
                                    jp969 = true
                                } else {
                                    var t972 bool = utf8_invalid_continuation(third__13)
                                    jp969 = t972
                                }
                                var jp963 bool
                                if jp969 {
                                    jp963 = true
                                } else {
                                    var t970 bool = utf8_invalid_continuation(fourth__14)
                                    jp963 = t970
                                }
                                var jp957 bool
                                if jp963 {
                                    jp957 = true
                                } else {
                                    var t966 bool = first__8 == 240
                                    if t966 {
                                        var t967 bool = second__12 < 144
                                        jp957 = t967
                                    } else {
                                        jp957 = false
                                    }
                                }
                                var jp943 bool
                                if jp957 {
                                    jp943 = true
                                } else {
                                    var t960 bool = first__8 == 244
                                    if t960 {
                                        var t961 bool = second__12 > 143
                                        jp943 = t961
                                    } else {
                                        jp943 = false
                                    }
                                }
                                if jp943 {
                                    var t944 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t944
                                } else {
                                    var t945_rhs uint32 = 7
                                    var t945 uint32 = first__8 & t945_rhs
                                    var t946_rhs int = 18
                                    var t946 uint32 = t945 << t946_rhs
                                    var t947_rhs uint32 = 63
                                    var t947 uint32 = second__12 & t947_rhs
                                    var t948_rhs int = 12
                                    var t948 uint32 = t947 << t948_rhs
                                    var t949 uint32 = t946 | t948
                                    var t950_rhs uint32 = 63
                                    var t950 uint32 = third__13 & t950_rhs
                                    var t951_rhs int = 6
                                    var t951 uint32 = t950 << t951_rhs
                                    var t952 uint32 = t949 | t951
                                    var t953_rhs uint32 = 63
                                    var t953 uint32 = fourth__14 & t953_rhs
                                    var t954 uint32 = t952 | t953
                                    var t955 Tuple3_4bool_4char_3int = utf8_valid_decode(t954, 4)
                                    return t955
                                }
                            }
                        } else {
                            var t976 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t976
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
    var t1003 bool = index__16 < 0
    var jp995 bool
    if t1003 {
        jp995 = true
    } else {
        var t1004 int
        var inline1473 int = _goml_runtime_core_string_len(value__15)
        t1004 = inline1473
        var t1005 bool = index__16 > t1004
        jp995 = t1005
    }
    if jp995 {
        return false
    } else {
        var t998 int
        var inline1477 int = _goml_runtime_core_string_len(value__15)
        t998 = inline1477
        var t999 bool = index__16 == t998
        if t999 {
            return true
        } else {
            var t1000 uint8
            var inline1475 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1000 = inline1475
            var t1001_rhs uint8 = 192
            var t1001 uint8 = t1000 & t1001_rhs
            var t1002 bool = t1001 != 128
            return t1002
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1008 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1008
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1011 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1011
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1513 rune
    var inline1481 bool = utf8_valid_scalar(value__0)
    if inline1481 {
        var inline1482 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1483 rune = inline1482._1
        commute_field1513 = inline1483
        var t1017 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1513,
            _2: width__1,
        }
        return t1017
    } else {
        var inline1479 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1479
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1022 bool = value__3 < 128
    if t1022 {
        return true
    } else {
        var t1023 bool = value__3 > 191
        return t1023
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1028 bool
    var inline1487 bool = value__32 <= 1114111
    if inline1487 {
        var inline1488 bool = value__32 >= 55296
        var inline1490 bool
        if inline1488 {
            var inline1492 bool = value__32 <= 57343
            inline1490 = inline1492
        } else {
            inline1490 = false
        }
        var inline1491 bool = !inline1490
        t1028 = inline1491
    } else {
        t1028 = false
    }
    if t1028 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1029 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1029
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1034 bool = value__4 <= 1114111
    if t1034 {
        var t1038 bool = value__4 >= 55296
        var jp1036 bool
        if t1038 {
            var t1039 bool = value__4 <= 57343
            jp1036 = t1039
        } else {
            jp1036 = false
        }
        var t1037 bool = !jp1036
        return t1037
    } else {
        return false
    }
}

func main() {
    main0()
}
