package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
)

func string_byte_slice(s string, start int32, end int32) string {
    return s[start:end]
}

func _goml_m_std_p_fs_p_read__file__raw(path string) Tuple3_4bool_6string_6string {
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

func _goml_m_std_p_fs_p_write__file__raw(path string, content string) Tuple2_4bool_6string {
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

func _goml_m_std_p_fs_p_create__dir__all__raw(path string) Tuple2_4bool_6string {
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

func _goml_m_std_p_io_p_println__raw(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint__raw(value string) struct{} {
    _goml_fmt.Fprint(_goml_os.Stderr, value)
    return struct{}{}
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
    _1 []string
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
    _0 []string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Ok) is_goml_m_Result____Vec_l_string_r_____string() {}

type _goml_m_Result____Vec_l_string_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Err) is_goml_m_Result____Vec_l_string_r_____string() {}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv10 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_m_std_p_fs_p_read__file__raw(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp12 Result__string__string
    if ok__1 {
        var t13 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp12 = t13
    } else {
        var t14 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp12 = t14
    }
    retv10 = jp12
    return retv10
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv16 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_m_std_p_fs_p_write__file__raw(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp18 Result__unit__string
    if ok__6 {
        var t19 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp18 = t19
    } else {
        var t20 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp18 = t20
    }
    retv16 = jp18
    return retv16
}

func _goml_m_std_p_fs_p_create__dir__all(path__8 string) Result__unit__string {
    var retv22 Result__unit__string
    var mtmp7 Tuple2_4bool_6string = _goml_m_std_p_fs_p_create__dir__all__raw(path__8)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__10 string = x9
    var ok__9 bool = x8
    var jp24 Result__unit__string
    if ok__9 {
        var t25 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp24 = t25
    } else {
        var t26 Result__unit__string = Result__unit__string_Err{
            _0: err__10,
        }
        jp24 = t26
    }
    retv22 = jp24
    return retv22
}

func show_unit(result__0 Result__unit__string) string {
    var retv37 string
    var jp39 string
    switch result__0.(type) {
    case Result__unit__string_Ok:
        jp39 = "ok"
    case Result__unit__string_Err:
        var x1 string = result__0.(Result__unit__string_Err)._0
        var err__1 string = x1
        var t40 string = "err " + err__1
        jp39 = t40
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func show_string(result__2 Result__string__string) string {
    var retv42 string
    var jp44 string
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x2 string = result__2.(Result__string__string_Ok)._0
        var value__3 string = x2
        jp44 = value__3
    case Result__string__string_Err:
        var x3 string = result__2.(Result__string__string_Err)._0
        var err__4 string = x3
        var t45 string = "err " + err__4
        jp44 = t45
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func main0() struct{} {
    var t47 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t47)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t48 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t49 string = show_unit(t48)
    _goml_m_std_p_io_p_println____T__string(t49)
    var t50 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t51 string = show_unit(t50)
    _goml_m_std_p_io_p_println____T__string(t51)
    var t52 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t53 string = show_string(t52)
    _goml_m_std_p_io_p_println____T__string(t53)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    _goml_m_std_p_io_p_println__raw(value__1)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    _goml_m_std_p_io_p_eprint__raw(value__2)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t60 string = value__3 + "\n"
    _goml_m_std_p_io_p_eprint__raw(t60)
    return struct{}{}
}

func main() {
    main0()
}
