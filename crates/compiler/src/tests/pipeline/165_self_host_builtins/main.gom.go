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
    var retv32 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_runtime_std_fs_read_file(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp34 Result__string__string
    if ok__1 {
        var t35 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp34 = t35
    } else {
        var t36 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp34 = t36
    }
    retv32 = jp34
    return retv32
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv38 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_runtime_std_fs_write_file(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp40 Result__unit__string
    if ok__6 {
        var t41 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp40 = t41
    } else {
        var t42 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp40 = t42
    }
    retv38 = jp40
    return retv38
}

func _goml_m_std_p_fs_p_create__dir__all(path__8 string) Result__unit__string {
    var retv44 Result__unit__string
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__8)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__10 string = x9
    var ok__9 bool = x8
    var jp46 Result__unit__string
    if ok__9 {
        var t47 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp46 = t47
    } else {
        var t48 Result__unit__string = Result__unit__string_Err{
            _0: err__10,
        }
        jp46 = t48
    }
    retv44 = jp46
    return retv44
}

func show_unit(result__0 Result__unit__string) string {
    var retv59 string
    var jp61 string
    switch result__0.(type) {
    case Result__unit__string_Ok:
        jp61 = "ok"
    case Result__unit__string_Err:
        var x23 string = result__0.(Result__unit__string_Err)._0
        var err__1 string = x23
        var t62 string = "err " + err__1
        jp61 = t62
    default:
        panic("non-exhaustive match")
    }
    retv59 = jp61
    return retv59
}

func show_string(result__2 Result__string__string) string {
    var retv64 string
    var jp66 string
    switch result__2.(type) {
    case Result__string__string_Ok:
        var x24 string = result__2.(Result__string__string_Ok)._0
        var value__3 string = x24
        jp66 = value__3
    case Result__string__string_Err:
        var x25 string = result__2.(Result__string__string_Err)._0
        var err__4 string = x25
        var t67 string = "err " + err__4
        jp66 = t67
    default:
        panic("non-exhaustive match")
    }
    retv64 = jp66
    return retv64
}

func main0() struct{} {
    var t69 string = _goml_runtime_core_string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t69)
    _goml_m_std_p_io_p_eprint____T__string("")
    _goml_m_std_p_io_p_eprintln____T__string("")
    var t70 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t71 string = show_unit(t70)
    _goml_m_std_p_io_p_println____T__string(t71)
    var t72 Result__unit__string = _goml_m_std_p_fs_p_write__file("goml-self-host/nested/output.txt", "boot")
    var t73 string = show_unit(t72)
    _goml_m_std_p_io_p_println____T__string(t73)
    var t74 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-self-host/nested/output.txt")
    var t75 string = show_string(t74)
    _goml_m_std_p_io_p_println____T__string(t75)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t78)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__2)
    _goml_runtime_std_io_eprint(t81)
    return struct{}{}
}

func _goml_m_std_p_io_p_eprintln____T__string(value__3 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__3)
    var t85 string = t84 + "\n"
    _goml_runtime_std_io_eprint(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv88 string
    retv88 = self__9
    return retv88
}

func main() {
    main0()
}
