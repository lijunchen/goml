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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t173 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t173
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t176 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t177 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t176,
    }
    return t177
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var t218 *_goml_vec_uint8 = self__22.values
    return t218
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

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var t230 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(x1)
        return t230
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var t231 Result__string__string = Result__string__string_Err{
            _0: x2,
        }
        return t231
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var t234 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t235 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t234)
    return t235
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    if x4 {
        var t240 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(x5)
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
    var t245 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
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

func show_unit(result__0 Result__unit__string) string {
    switch result__0.(type) {
    case Result__unit__string_Ok:
        return "ok"
    case Result__unit__string_Err:
        var x156 string = result__0.(Result__unit__string_Err)._0
        var t331 string = "err " + x156
        return t331
    default:
        panic("non-exhaustive match")
    }
}

func show_string(result__2 Result__string__string) string {
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x157 string = result__2.(Result__string__string_Ok)._0
        return x157
    case Result__string__string_Err:
        var x158 string = result__2.(Result__string__string_Err)._0
        var t336 string = "err " + x158
        return t336
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t338 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t338)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t339 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t340 string = show_unit(t339)
    _goml_m_std_p_io_p_println____T__string(t340)
    var t341 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t342 string = show_unit(t341)
    _goml_m_std_p_io_p_println____T__string(t342)
    var t343 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t344 string = show_string(t343)
    _goml_m_std_p_io_p_println____T__string(t344)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var t354 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    return t354
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t385 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t385)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t388 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__2)
    _goml_runtime_std_io_eprint(t388)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t391 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__3)
    var t392 string = t391 + "\n"
    _goml_runtime_std_io_eprint(t392)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
