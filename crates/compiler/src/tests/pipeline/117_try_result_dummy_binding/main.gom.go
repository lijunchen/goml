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
    var retv17 Result__unit__string
    var jp19 Result__unit__string
    if ok__0 {
        var t20 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp19 = t20
    } else {
        var t21 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp19 = t21
    }
    retv17 = jp19
    return retv17
}

func read_duration(ok__1 bool) Result__string__string {
    var retv23 Result__string__string
    var jp25 Result__string__string
    if ok__1 {
        var t26 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp25 = t26
    } else {
        var t27 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp25 = t27
    }
    retv23 = jp25
    return retv23
}

func format_duration(value__2 string) string {
    var retv29 string
    var t30 string = "duration=" + value__2
    retv29 = t30
    return retv29
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv32 Result__string__string
    var mtmp4 Result__unit__string = configure(config_ok__3)
    switch mtmp4.(type) {
    case Result__unit__string_Ok:
        var mtmp8 Result__string__string = read_duration(read_ok__4)
        var jp35 string
        switch mtmp8.(type) {
        case Result__string__string_Ok:
            var x9 string = mtmp8.(Result__string__string_Ok)._0
            var try_value__27 string = x9
            jp35 = try_value__27
            var value__5 string = jp35
            var t36 string = format_duration(value__5)
            var t37 Result__string__string = Result__string__string_Ok{
                _0: t36,
            }
            retv32 = t37
            return retv32
        case Result__string__string_Err:
            var x10 string = mtmp8.(Result__string__string_Err)._0
            var try_residual__27 string = x10
            var t38 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv32 = t38
            return retv32
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x6 string = mtmp4.(Result__unit__string_Err)._0
        var try_residual__23 string = x6
        var t39 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv32 = t39
        return retv32
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv41 string
    var jp43 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x11 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x11
        var t44 string = "ok " + value__7
        jp43 = t44
    case Result__string__string_Err:
        var x12 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x12
        var t45 string = "err " + err__8
        jp43 = t45
    default:
        panic("non-exhaustive match")
    }
    retv41 = jp43
    return retv41
}

func main0() struct{} {
    var t47 Result__string__string = configure_and_format(true, true)
    var t48 string = show(t47)
    println__T_string(t48)
    var t49 Result__string__string = configure_and_format(true, false)
    var t50 string = show(t49)
    println__T_string(t50)
    var t51 Result__string__string = configure_and_format(false, true)
    var t52 string = show(t51)
    println__T_string(t52)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func main() {
    main0()
}
