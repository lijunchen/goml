package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

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

func configure(ok__0 bool) Result__unit__string {
    var retv20 Result__unit__string
    var jp22 Result__unit__string
    if ok__0 {
        var t23 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp22 = t23
    } else {
        var t24 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp22 = t24
    }
    retv20 = jp22
    return retv20
}

func read_duration(ok__1 bool) Result__string__string {
    var retv26 Result__string__string
    var jp28 Result__string__string
    if ok__1 {
        var t29 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp28 = t29
    } else {
        var t30 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp28 = t30
    }
    retv26 = jp28
    return retv26
}

func format_duration(value__2 string) string {
    var retv32 string
    var t33 string = "duration=" + value__2
    retv32 = t33
    return retv32
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv35 Result__string__string
    var mtmp7 Result__unit__string = configure(config_ok__3)
    switch mtmp7.(type) {
    case Result__unit__string_Ok:
        var mtmp11 Result__string__string = read_duration(read_ok__4)
        var jp38 string
        switch mtmp11.(type) {
        case Result__string__string_Ok:
            var x12 string = mtmp11.(Result__string__string_Ok)._0
            var try_value__27 string = x12
            jp38 = try_value__27
            var value__5 string = jp38
            var t39 string = format_duration(value__5)
            var t40 Result__string__string = Result__string__string_Ok{
                _0: t39,
            }
            retv35 = t40
            return retv35
        case Result__string__string_Err:
            var x13 string = mtmp11.(Result__string__string_Err)._0
            var try_residual__27 string = x13
            var t41 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv35 = t41
            return retv35
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x9 string = mtmp7.(Result__unit__string_Err)._0
        var try_residual__23 string = x9
        var t42 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv35 = t42
        return retv35
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv44 string
    var jp46 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x14 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x14
        var t47 string = "ok " + value__7
        jp46 = t47
    case Result__string__string_Err:
        var x15 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x15
        var t48 string = "err " + err__8
        jp46 = t48
    default:
        panic("non-exhaustive match")
    }
    retv44 = jp46
    return retv44
}

func main0() struct{} {
    var t50 Result__string__string = configure_and_format(true, true)
    var t51 string = show(t50)
    println__T_string(t51)
    var t52 Result__string__string = configure_and_format(true, false)
    var t53 string = show(t52)
    println__T_string(t53)
    var t54 Result__string__string = configure_and_format(false, true)
    var t55 string = show(t54)
    println__T_string(t55)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t57 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t57)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv60 string
    retv60 = self__9
    return retv60
}

func main() {
    main0()
}
