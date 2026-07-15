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
    var retv35 Result__unit__string
    var jp37 Result__unit__string
    if ok__0 {
        var t38 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp37 = t38
    } else {
        var t39 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp37 = t39
    }
    retv35 = jp37
    return retv35
}

func read_duration(ok__1 bool) Result__string__string {
    var retv41 Result__string__string
    var jp43 Result__string__string
    if ok__1 {
        var t44 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp43 = t44
    } else {
        var t45 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp43 = t45
    }
    retv41 = jp43
    return retv41
}

func format_duration(value__2 string) string {
    var retv47 string
    var t48 string = "duration=" + value__2
    retv47 = t48
    return retv47
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv50 Result__string__string
    var mtmp22 Result__unit__string = configure(config_ok__3)
    switch mtmp22.(type) {
    case Result__unit__string_Ok:
        var mtmp26 Result__string__string = read_duration(read_ok__4)
        var jp53 string
        switch mtmp26.(type) {
        case Result__string__string_Ok:
            var x27 string = mtmp26.(Result__string__string_Ok)._0
            var try_value__27 string = x27
            jp53 = try_value__27
            var value__5 string = jp53
            var t54 string = format_duration(value__5)
            var t55 Result__string__string = Result__string__string_Ok{
                _0: t54,
            }
            retv50 = t55
            return retv50
        case Result__string__string_Err:
            var x28 string = mtmp26.(Result__string__string_Err)._0
            var try_residual__27 string = x28
            var t56 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv50 = t56
            return retv50
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x24 string = mtmp22.(Result__unit__string_Err)._0
        var try_residual__23 string = x24
        var t57 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv50 = t57
        return retv50
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv59 string
    var jp61 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x29 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x29
        var t62 string = "ok " + value__7
        jp61 = t62
    case Result__string__string_Err:
        var x30 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x30
        var t63 string = "err " + err__8
        jp61 = t63
    default:
        panic("non-exhaustive match")
    }
    retv59 = jp61
    return retv59
}

func main0() struct{} {
    var t65 Result__string__string = configure_and_format(true, true)
    var t66 string = show(t65)
    println__T_string(t66)
    var t67 Result__string__string = configure_and_format(true, false)
    var t68 string = show(t67)
    println__T_string(t68)
    var t69 Result__string__string = configure_and_format(false, true)
    var t70 string = show(t69)
    println__T_string(t70)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv75 string
    retv75 = self__9
    return retv75
}

func main() {
    main0()
}
