package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int) bool {
    if i < 0 || i > int(len(s)) {
        return false
    }
    if i == int(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    if !_goml_utf8.Valid(bytes.items) {
        return Tuple2_4bool_6string{
            _0: false,
            _1: "",
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
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

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type Option__uint8 interface {
    isOption__uint8()
}

type None struct {}

func (_ None) isOption__uint8() {}

type Some struct {
    _0 uint8
}

func (_ Some) isOption__uint8() {}

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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t176 *_goml_vec_uint8
    var inline404 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t176 = inline404
    var t177 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t176,
    }
    return t177
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t221 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t221)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t224 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t224
    } else {
        var t225 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t225
    }
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    if x4 {
        var t240 _goml_m_std_p_bytes_p_Bytes
        var inline463 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x5,
        }
        t240 = inline463
        var t241 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t240,
        }
        return t241
    } else {
        var t242 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x6,
        }
        return t242
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t245 *_goml_vec_uint8
    var inline465 *_goml_vec_uint8 = data__10.values
    t245 = inline465
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t245)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t248 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t248
    } else {
        var t249 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t249
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__13 string) Result__unit__string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__13)
    var x11 bool = mtmp10._0
    var x12 string = mtmp10._1
    if x11 {
        var t254 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t254
    } else {
        var t255 Result__unit__string = Result__unit__string_Err{
            _0: x12,
        }
        return t255
    }
}

func main0() struct{} {
    var t338 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t338)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline514 string = ""
    var inline515 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline514)
    var inline516 string = inline515 + "\n"
    _goml_runtime_std_io_eprint(inline516)
    var t339 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t340 string
    switch t339.(type) {
    case Result__unit__string_Ok:
        t340 = "ok"
    case Result__unit__string_Err:
        var inline510 string = t339.(Result__unit__string_Err)._0
        var inline512 string = "err " + inline510
        t340 = inline512
    default:
        panic("non-exhaustive match")
    }
    var inline506 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t340)
    _goml_runtime_std_io_println(inline506)
    var t341 Result__unit__string
    var inline501 string = "goml-self-host/nested/output.txt"
    var inline502 string = "boot"
    var inline503 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline502)
    var inline504 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline501, inline503)
    t341 = inline504
    var t342 string
    switch t341.(type) {
    case Result__unit__string_Ok:
        t342 = "ok"
    case Result__unit__string_Err:
        var inline497 string = t341.(Result__unit__string_Err)._0
        var inline499 string = "err " + inline497
        t342 = inline499
    default:
        panic("non-exhaustive match")
    }
    var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t342)
    _goml_runtime_std_io_println(inline493)
    var t343 Result__string__string
    var inline484 string = "goml-self-host/nested/output.txt"
    var inline485 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline484)
    switch inline485.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline486 _goml_m_std_p_bytes_p_Bytes = inline485.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline488 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline486)
        t343 = inline488
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline489 string = inline485.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline491 Result__string__string = Result__string__string_Err{
            _0: inline489,
        }
        t343 = inline491
    default:
        panic("non-exhaustive match")
    }
    var t344 string
    switch t343.(type) {
    case Result__string__string_Ok:
        var inline478 string = t343.(Result__string__string_Ok)._0
        t344 = inline478
    case Result__string__string_Err:
        var inline480 string = t343.(Result__string__string_Err)._0
        var inline482 string = "err " + inline480
        t344 = inline482
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t344)
    _goml_runtime_std_io_println(inline475)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t385 string
    t385 = value__1
    _goml_runtime_std_io_println(t385)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t388 string
    t388 = value__2
    _goml_runtime_std_io_eprint(t388)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
