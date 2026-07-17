package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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
    var i int32 = 0
    for {
        if i >= int32(len(entries)) {
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

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
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
    var retv75 _goml_m_std_p_bytes_p_Bytes
    var t76 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    retv75 = t76
    return retv75
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var retv78 _goml_m_std_p_bytes_p_Bytes
    var t79 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t80 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t79,
    }
    retv78 = t80
    return retv78
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var retv121 *_goml_vec_uint8
    var t122 *_goml_vec_uint8 = self__22.values
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var retv124 Result__string__string
    var t125 *_goml_vec_uint8 = self__23.values
    var mtmp0 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t125)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var value__25 string = x2
    var valid__24 bool = x1
    var jp127 Result__string__string
    if valid__24 {
        var t128 Result__string__string = Result__string__string_Ok{
            _0: value__25,
        }
        jp127 = t128
    } else {
        var t129 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        jp127 = t129
    }
    retv124 = jp127
    return retv124
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv131 Result__string__string
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    var jp133 Result__string__string
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var data__1 _goml_m_std_p_bytes_p_Bytes = x1
        var t134 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(data__1)
        jp133 = t134
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var error__2 string = x2
        var t135 Result__string__string = Result__string__string_Err{
            _0: error__2,
        }
        jp133 = t135
    default:
        panic("non-exhaustive match")
    }
    retv131 = jp133
    return retv131
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var retv137 Result__unit__string
    var t138 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t139 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t138)
    retv137 = t139
    return retv137
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var retv141 _goml_m_Result____std_p_bytes_p_Bytes____string
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    var err__8 string = x6
    var data__7 *_goml_vec_uint8 = x5
    var ok__6 bool = x4
    var jp143 _goml_m_Result____std_p_bytes_p_Bytes____string
    if ok__6 {
        var t144 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(data__7)
        var t145 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t144,
        }
        jp143 = t145
    } else {
        var t146 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: err__8,
        }
        jp143 = t146
    }
    retv141 = jp143
    return retv141
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var retv148 Result__unit__string
    var t149 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t149)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__12 string = x9
    var ok__11 bool = x8
    var jp151 Result__unit__string
    if ok__11 {
        var t152 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp151 = t152
    } else {
        var t153 Result__unit__string = Result__unit__string_Err{
            _0: err__12,
        }
        jp151 = t153
    }
    retv148 = jp151
    return retv148
}

func _goml_m_std_p_fs_p_exists(path__16 string) bool {
    var retv161 bool
    var t162 bool = _goml_runtime_std_fs_file_exists(path__16)
    retv161 = t162
    return retv161
}

func _goml_m_std_p_fs_p_read__dir(path__23 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv176 _goml_m_Result____Vec_l_string_r_____string
    var mtmp17 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__23)
    var x18 bool = mtmp17._0
    var x19 *_goml_vec_string = mtmp17._1
    var x20 string = mtmp17._2
    var err__26 string = x20
    var names__25 *_goml_vec_string = x19
    var ok__24 bool = x18
    var jp178 _goml_m_Result____Vec_l_string_r_____string
    if ok__24 {
        var t179 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__25,
        }
        jp178 = t179
    } else {
        var t180 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__26,
        }
        jp178 = t180
    }
    retv176 = jp178
    return retv176
}

func show_read(res__0 Result__string__string) string {
    var retv203 string
    var jp205 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x58 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x58
        jp205 = value__1
    case Result__string__string_Err:
        var x59 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x59
        var t206 string = "err " + err__2
        jp205 = t206
    default:
        panic("non-exhaustive match")
    }
    retv203 = jp205
    return retv203
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv208 string
    var jp210 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x60 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 *_goml_vec_string = x60
        var t211 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t212 bool = t211 > 0
        var t213 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t212)
        jp210 = t213
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x61 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x61
        var t214 string = "err " + err__5
        jp210 = t214
    default:
        panic("non-exhaustive match")
    }
    retv208 = jp210
    return retv208
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t216 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t217 string = show_read(t216)
    _goml_m_std_p_io_p_println____T__string(t217)
    var t218 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t219 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t218)
    _goml_m_std_p_io_p_println____T__string(t219)
    var t220 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t221 string = show_dir(t220)
    _goml_m_std_p_io_p_println____T__string(t221)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__20 string) *_goml_vec_uint8 {
    var retv229 *_goml_vec_uint8
    var t230 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__20)
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__131 *_goml_vec_string) int32 {
    var retv266 int32
    var t267 int32 = vec_len__Vec_6string(self__131)
    retv266 = t267
    return retv266
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv269 string
    var t270 string = _goml_runtime_core_bool_to_string(self__33)
    retv269 = t270
    return retv269
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t272)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv275 string
    retv275 = self__34
    return retv275
}

func main() {
    main0()
}
