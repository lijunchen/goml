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
    var retv74 Result__unit__string
    var jp76 Result__unit__string
    if ok__0 {
        var t77 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp76 = t77
    } else {
        var t78 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp76 = t78
    }
    retv74 = jp76
    return retv74
}

func read_duration(ok__1 bool) Result__string__string {
    var retv80 Result__string__string
    var jp82 Result__string__string
    if ok__1 {
        var t83 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp82 = t83
    } else {
        var t84 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp82 = t84
    }
    retv80 = jp82
    return retv80
}

func format_duration(value__2 string) string {
    var retv86 string
    var t87 string = "duration=" + value__2
    retv86 = t87
    return retv86
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv89 Result__string__string
    var mtmp61 Result__unit__string = configure(config_ok__3)
    switch mtmp61.(type) {
    case Result__unit__string_Ok:
        var mtmp65 Result__string__string = read_duration(read_ok__4)
        var jp92 string
        switch mtmp65.(type) {
        case Result__string__string_Ok:
            var x66 string = mtmp65.(Result__string__string_Ok)._0
            var try_value__27 string = x66
            jp92 = try_value__27
            var value__5 string = jp92
            var t93 string = format_duration(value__5)
            var t94 Result__string__string = Result__string__string_Ok{
                _0: t93,
            }
            retv89 = t94
            return retv89
        case Result__string__string_Err:
            var x67 string = mtmp65.(Result__string__string_Err)._0
            var try_residual__27 string = x67
            var t95 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv89 = t95
            return retv89
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x63 string = mtmp61.(Result__unit__string_Err)._0
        var try_residual__23 string = x63
        var t96 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv89 = t96
        return retv89
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv98 string
    var jp100 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x68 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x68
        var t101 string = "ok " + value__7
        jp100 = t101
    case Result__string__string_Err:
        var x69 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x69
        var t102 string = "err " + err__8
        jp100 = t102
    default:
        panic("non-exhaustive match")
    }
    retv98 = jp100
    return retv98
}

func main0() struct{} {
    var t104 Result__string__string = configure_and_format(true, true)
    var t105 string = show(t104)
    println__T_string(t105)
    var t106 Result__string__string = configure_and_format(true, false)
    var t107 string = show(t106)
    println__T_string(t107)
    var t108 Result__string__string = configure_and_format(false, true)
    var t109 string = show(t108)
    println__T_string(t109)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv114 string
    retv114 = self__37
    return retv114
}

func main() {
    main0()
}
