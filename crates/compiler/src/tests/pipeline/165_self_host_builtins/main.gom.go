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

func _goml_runtime_std_fs_read_file(path string) Tuple3_4bool_6string_6string {
    var data []uint8
    var err error
    data, err = _goml_os.ReadFile(path)
    if err != nil {
        return Tuple3_4bool_6string_6string{
            _0: false,
            _1: "",
            _2: err.Error(),
        }
    }
    return Tuple3_4bool_6string_6string{
        _0: true,
        _1: string(data),
        _2: "",
    }
}

func _goml_runtime_std_fs_write_file(path string, content string) Tuple2_4bool_6string {
    var err error = _goml_os.WriteFile(path, []byte(content), 0644)
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

type _goml_vec_string struct {
    items []string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 string
}

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

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv68 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_runtime_std_fs_read_file(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp70 Result__string__string
    if ok__1 {
        var t71 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp70 = t71
    } else {
        var t72 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp70 = t72
    }
    retv68 = jp70
    return retv68
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv74 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_runtime_std_fs_write_file(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp76 Result__unit__string
    if ok__6 {
        var t77 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp76 = t77
    } else {
        var t78 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp76 = t78
    }
    retv74 = jp76
    return retv74
}

func _goml_m_std_p_fs_p_create__dir__all(path__8 string) Result__unit__string {
    var retv80 Result__unit__string
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__8)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__10 string = x9
    var ok__9 bool = x8
    var jp82 Result__unit__string
    if ok__9 {
        var t83 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp82 = t83
    } else {
        var t84 Result__unit__string = Result__unit__string_Err{
            _0: err__10,
        }
        jp82 = t84
    }
    retv80 = jp82
    return retv80
}

func show_unit(result__0 Result__unit__string) string {
    var retv95 string
    var jp97 string
    switch result__0.(type) {
    case Result__unit__string_Ok:
        jp97 = "ok"
    case Result__unit__string_Err:
        var x59 string = result__0.(Result__unit__string_Err)._0
        var err__1 string = x59
        var t98 string = "err " + err__1
        jp97 = t98
    default:
        panic("non-exhaustive match")
    }
    retv95 = jp97
    return retv95
}

func show_string(result__2 Result__string__string) string {
    var retv100 string
    var jp102 string
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x60 string = result__2.(Result__string__string_Ok)._0
        var value__3 string = x60
        jp102 = value__3
    case Result__string__string_Err:
        var x61 string = result__2.(Result__string__string_Err)._0
        var err__4 string = x61
        var t103 string = "err " + err__4
        jp102 = t103
    default:
        panic("non-exhaustive match")
    }
    retv100 = jp102
    return retv100
}

func main0() struct{} {
    var t105 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t105)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t106 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t107 string = show_unit(t106)
    _goml_m_std_p_io_p_println____T__string(t107)
    var t108 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t109 string = show_unit(t108)
    _goml_m_std_p_io_p_println____T__string(t109)
    var t110 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t111 string = show_string(t110)
    _goml_m_std_p_io_p_println____T__string(t111)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t114)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__2)
    _goml_runtime_std_io_eprint(t117)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__3)
    var t121 string = t120 + "\n"
    _goml_runtime_std_io_eprint(t121)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv124 string
    retv124 = self__34
    return retv124
}

func main() {
    main0()
}
