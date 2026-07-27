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
    var retv77 Result__unit__string
    var jp79 Result__unit__string
    if ok__0 {
        var t80 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp79 = t80
    } else {
        var t81 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func read_duration(ok__1 bool) Result__string__string {
    var retv83 Result__string__string
    var jp85 Result__string__string
    if ok__1 {
        var t86 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp85 = t86
    } else {
        var t87 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp85 = t87
    }
    retv83 = jp85
    return retv83
}

func format_duration(value__2 string) string {
    var retv89 string
    var t90 string = "duration=" + value__2
    retv89 = t90
    return retv89
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv92 Result__string__string
    var mtmp64 Result__unit__string = configure(config_ok__3)
    switch mtmp64.(type) {
    case Result__unit__string_Ok:
        var mtmp68 Result__string__string = read_duration(read_ok__4)
        var jp95 string
        switch mtmp68.(type) {
        case Result__string__string_Ok:
            var x69 string = mtmp68.(Result__string__string_Ok)._0
            var try_value__27 string = x69
            jp95 = try_value__27
            var value__5 string = jp95
            var t96 string = format_duration(value__5)
            var t97 Result__string__string = Result__string__string_Ok{
                _0: t96,
            }
            retv92 = t97
            return retv92
        case Result__string__string_Err:
            var x70 string = mtmp68.(Result__string__string_Err)._0
            var try_residual__27 string = x70
            var t98 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv92 = t98
            return retv92
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x66 string = mtmp64.(Result__unit__string_Err)._0
        var try_residual__23 string = x66
        var t99 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv92 = t99
        return retv92
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv101 string
    var jp103 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x71 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x71
        var t104 string = "ok " + value__7
        jp103 = t104
    case Result__string__string_Err:
        var x72 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x72
        var t105 string = "err " + err__8
        jp103 = t105
    default:
        panic("non-exhaustive match")
    }
    retv101 = jp103
    return retv101
}

func main0() struct{} {
    var t107 Result__string__string = configure_and_format(true, true)
    var t108 string = show(t107)
    println__T_string(t108)
    var t109 Result__string__string = configure_and_format(true, false)
    var t110 string = show(t109)
    println__T_string(t110)
    var t111 Result__string__string = configure_and_format(false, true)
    var t112 string = show(t111)
    println__T_string(t112)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv117 string
    retv117 = self__38
    return retv117
}

func main() {
    main0()
}
