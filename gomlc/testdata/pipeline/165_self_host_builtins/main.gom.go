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
    var retv170 _goml_m_std_p_bytes_p_Bytes
    var t171 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    retv170 = t171
    return retv170
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var retv173 _goml_m_std_p_bytes_p_Bytes
    var t174 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t175 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t174,
    }
    retv173 = t175
    return retv173
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var retv215 *_goml_vec_uint8
    var t216 *_goml_vec_uint8 = self__22.values
    retv215 = t216
    return retv215
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var retv218 Result__string__string
    var t219 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t219)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    var value__25 string = x8
    var valid__24 bool = x7
    var jp221 Result__string__string
    if valid__24 {
        var t222 Result__string__string = Result__string__string_Ok{
            _0: value__25,
        }
        jp221 = t222
    } else {
        var t223 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        jp221 = t223
    }
    retv218 = jp221
    return retv218
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv225 Result__string__string
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    var jp227 Result__string__string
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var data__1 _goml_m_std_p_bytes_p_Bytes = x1
        var t228 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(data__1)
        jp227 = t228
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var error__2 string = x2
        var t229 Result__string__string = Result__string__string_Err{
            _0: error__2,
        }
        jp227 = t229
    default:
        panic("non-exhaustive match")
    }
    retv225 = jp227
    return retv225
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var retv231 Result__unit__string
    var t232 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t233 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t232)
    retv231 = t233
    return retv231
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var retv235 _goml_m_Result____std_p_bytes_p_Bytes____string
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    var err__8 string = x6
    var data__7 *_goml_vec_uint8 = x5
    var ok__6 bool = x4
    var jp237 _goml_m_Result____std_p_bytes_p_Bytes____string
    if ok__6 {
        var t238 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(data__7)
        var t239 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t238,
        }
        jp237 = t239
    } else {
        var t240 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: err__8,
        }
        jp237 = t240
    }
    retv235 = jp237
    return retv235
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var retv242 Result__unit__string
    var t243 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t243)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__12 string = x9
    var ok__11 bool = x8
    var jp245 Result__unit__string
    if ok__11 {
        var t246 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp245 = t246
    } else {
        var t247 Result__unit__string = Result__unit__string_Err{
            _0: err__12,
        }
        jp245 = t247
    }
    retv242 = jp245
    return retv242
}

func _goml_m_std_p_fs_p_create__dir__all(path__13 string) Result__unit__string {
    var retv249 Result__unit__string
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__13)
    var x11 bool = mtmp10._0
    var x12 string = mtmp10._1
    var err__15 string = x12
    var ok__14 bool = x11
    var jp251 Result__unit__string
    if ok__14 {
        var t252 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp251 = t252
    } else {
        var t253 Result__unit__string = Result__unit__string_Err{
            _0: err__15,
        }
        jp251 = t253
    }
    retv249 = jp251
    return retv249
}

func show_unit(result__0 Result__unit__string) string {
    var retv326 string
    var jp328 string
    switch result__0.(type) {
    case Result__unit__string_Ok:
        jp328 = "ok"
    case Result__unit__string_Err:
        var x153 string = result__0.(Result__unit__string_Err)._0
        var err__1 string = x153
        var t329 string = "err " + err__1
        jp328 = t329
    default:
        panic("non-exhaustive match")
    }
    retv326 = jp328
    return retv326
}

func show_string(result__2 Result__string__string) string {
    var retv331 string
    var jp333 string
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x154 string = result__2.(Result__string__string_Ok)._0
        var value__3 string = x154
        jp333 = value__3
    case Result__string__string_Err:
        var x155 string = result__2.(Result__string__string_Err)._0
        var err__4 string = x155
        var t334 string = "err " + err__4
        jp333 = t334
    default:
        panic("non-exhaustive match")
    }
    retv331 = jp333
    return retv331
}

func main0() struct{} {
    var t336 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t336)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t337 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t338 string = show_unit(t337)
    _goml_m_std_p_io_p_println____T__string(t338)
    var t339 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t340 string = show_unit(t339)
    _goml_m_std_p_io_p_println____T__string(t340)
    var t341 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t342 string = show_string(t341)
    _goml_m_std_p_io_p_println____T__string(t342)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv351 *_goml_vec_uint8
    var t352 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv351 = t352
    return retv351
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t383 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t383)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t386 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__2)
    _goml_runtime_std_io_eprint(t386)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t389 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__3)
    var t390 string = t389 + "\n"
    _goml_runtime_std_io_eprint(t390)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv396 string
    retv396 = self__38
    return retv396
}

func main() {
    main0()
}
