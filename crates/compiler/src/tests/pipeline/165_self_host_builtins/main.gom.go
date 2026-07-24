package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_string_byte_slice(s string, start int32, end int32) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int32) bool {
    if i < 0 || i > int32(len(s)) {
        return false
    }
    if i == int32(len(s)) {
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
    var retv79 _goml_m_std_p_bytes_p_Bytes
    var t80 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    retv79 = t80
    return retv79
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var retv82 _goml_m_std_p_bytes_p_Bytes
    var t83 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t84 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t83,
    }
    retv82 = t84
    return retv82
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var retv125 *_goml_vec_uint8
    var t126 *_goml_vec_uint8 = self__22.values
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var retv128 Result__string__string
    var t129 *_goml_vec_uint8 = self__23.values
    var mtmp0 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t129)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var value__25 string = x2
    var valid__24 bool = x1
    var jp131 Result__string__string
    if valid__24 {
        var t132 Result__string__string = Result__string__string_Ok{
            _0: value__25,
        }
        jp131 = t132
    } else {
        var t133 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        jp131 = t133
    }
    retv128 = jp131
    return retv128
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv135 Result__string__string
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    var jp137 Result__string__string
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var data__1 _goml_m_std_p_bytes_p_Bytes = x1
        var t138 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(data__1)
        jp137 = t138
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var error__2 string = x2
        var t139 Result__string__string = Result__string__string_Err{
            _0: error__2,
        }
        jp137 = t139
    default:
        panic("non-exhaustive match")
    }
    retv135 = jp137
    return retv135
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var retv141 Result__unit__string
    var t142 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t143 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t142)
    retv141 = t143
    return retv141
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var retv145 _goml_m_Result____std_p_bytes_p_Bytes____string
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    var err__8 string = x6
    var data__7 *_goml_vec_uint8 = x5
    var ok__6 bool = x4
    var jp147 _goml_m_Result____std_p_bytes_p_Bytes____string
    if ok__6 {
        var t148 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(data__7)
        var t149 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t148,
        }
        jp147 = t149
    } else {
        var t150 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: err__8,
        }
        jp147 = t150
    }
    retv145 = jp147
    return retv145
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var retv152 Result__unit__string
    var t153 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t153)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__12 string = x9
    var ok__11 bool = x8
    var jp155 Result__unit__string
    if ok__11 {
        var t156 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp155 = t156
    } else {
        var t157 Result__unit__string = Result__unit__string_Err{
            _0: err__12,
        }
        jp155 = t157
    }
    retv152 = jp155
    return retv152
}

func _goml_m_std_p_fs_p_create__dir__all(path__13 string) Result__unit__string {
    var retv159 Result__unit__string
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__13)
    var x11 bool = mtmp10._0
    var x12 string = mtmp10._1
    var err__15 string = x12
    var ok__14 bool = x11
    var jp161 Result__unit__string
    if ok__14 {
        var t162 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp161 = t162
    } else {
        var t163 Result__unit__string = Result__unit__string_Err{
            _0: err__15,
        }
        jp161 = t163
    }
    retv159 = jp161
    return retv159
}

func show_unit(result__0 Result__unit__string) string {
    var retv207 string
    var jp209 string
    switch result__0.(type) {
    case Result__unit__string_Ok:
        jp209 = "ok"
    case Result__unit__string_Err:
        var x62 string = result__0.(Result__unit__string_Err)._0
        var err__1 string = x62
        var t210 string = "err " + err__1
        jp209 = t210
    default:
        panic("non-exhaustive match")
    }
    retv207 = jp209
    return retv207
}

func show_string(result__2 Result__string__string) string {
    var retv212 string
    var jp214 string
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x63 string = result__2.(Result__string__string_Ok)._0
        var value__3 string = x63
        jp214 = value__3
    case Result__string__string_Err:
        var x64 string = result__2.(Result__string__string_Err)._0
        var err__4 string = x64
        var t215 string = "err " + err__4
        jp214 = t215
    default:
        panic("non-exhaustive match")
    }
    retv212 = jp214
    return retv212
}

func main0() struct{} {
    var t217 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t217)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t218 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t219 string = show_unit(t218)
    _goml_m_std_p_io_p_println____T__string(t219)
    var t220 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t221 string = show_unit(t220)
    _goml_m_std_p_io_p_println____T__string(t221)
    var t222 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t223 string = show_string(t222)
    _goml_m_std_p_io_p_println____T__string(t223)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__23 string) *_goml_vec_uint8 {
    var retv232 *_goml_vec_uint8
    var t233 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__23)
    retv232 = t233
    return retv232
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t269)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__2)
    _goml_runtime_std_io_eprint(t272)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__3)
    var t276 string = t275 + "\n"
    _goml_runtime_std_io_eprint(t276)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv282 string
    retv282 = self__37
    return retv282
}

func main() {
    main0()
}
