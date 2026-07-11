package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
)

func _goml_runtime_core_string_byte_slice(s string, start int32, end int32) string {
    return s[start:end]
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
    var retv14 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_runtime_std_fs_read_file(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp16 Result__string__string
    if ok__1 {
        var t17 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp16 = t17
    } else {
        var t18 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp16 = t18
    }
    retv14 = jp16
    return retv14
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv20 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_runtime_std_fs_write_file(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp22 Result__unit__string
    if ok__6 {
        var t23 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp22 = t23
    } else {
        var t24 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp22 = t24
    }
    retv20 = jp22
    return retv20
}

func _goml_m_std_p_fs_p_create__dir__all(path__8 string) Result__unit__string {
    var retv26 Result__unit__string
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__8)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__10 string = x9
    var ok__9 bool = x8
    var jp28 Result__unit__string
    if ok__9 {
        var t29 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp28 = t29
    } else {
        var t30 Result__unit__string = Result__unit__string_Err{
            _0: err__10,
        }
        jp28 = t30
    }
    retv26 = jp28
    return retv26
}

func show_unit(result__0 Result__unit__string) string {
    var retv41 string
    var jp43 string
    switch result__0.(type) {
    case Result__unit__string_Ok:
        jp43 = "ok"
    case Result__unit__string_Err:
        var x5 string = result__0.(Result__unit__string_Err)._0
        var err__1 string = x5
        var t44 string = "err " + err__1
        jp43 = t44
    default:
        panic("non-exhaustive match")
    }
    retv41 = jp43
    return retv41
}

func show_string(result__2 Result__string__string) string {
    var retv46 string
    var jp48 string
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x6 string = result__2.(Result__string__string_Ok)._0
        var value__3 string = x6
        jp48 = value__3
    case Result__string__string_Err:
        var x7 string = result__2.(Result__string__string_Err)._0
        var err__4 string = x7
        var t49 string = "err " + err__4
        jp48 = t49
    default:
        panic("non-exhaustive match")
    }
    retv46 = jp48
    return retv46
}

func main0() struct{} {
    var t51 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t51)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t52 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t53 string = show_unit(t52)
    _goml_m_std_p_io_p_println____T__string(t53)
    var t54 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t55 string = show_unit(t54)
    _goml_m_std_p_io_p_println____T__string(t55)
    var t56 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t57 string = show_string(t56)
    _goml_m_std_p_io_p_println____T__string(t57)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t60 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t60)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t63 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__2)
    _goml_runtime_std_io_eprint(t63)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t66 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__3)
    var t67 string = t66 + "\n"
    _goml_runtime_std_io_eprint(t67)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv70 string
    retv70 = self__9
    return retv70
}

func main() {
    main0()
}
