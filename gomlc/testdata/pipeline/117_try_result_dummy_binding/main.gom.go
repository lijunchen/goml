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
    var retv81 Result__unit__string
    var jp83 Result__unit__string
    if ok__0 {
        var t84 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp83 = t84
    } else {
        var t85 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp83 = t85
    }
    retv81 = jp83
    return retv81
}

func read_duration(ok__1 bool) Result__string__string {
    var retv87 Result__string__string
    var jp89 Result__string__string
    if ok__1 {
        var t90 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp89 = t90
    } else {
        var t91 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp89 = t91
    }
    retv87 = jp89
    return retv87
}

func format_duration(value__2 string) string {
    var retv93 string
    var t94 string = "duration=" + value__2
    retv93 = t94
    return retv93
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv96 Result__string__string
    var mtmp68 Result__unit__string = configure(config_ok__3)
    switch mtmp68.(type) {
    case Result__unit__string_Ok:
        var mtmp72 Result__string__string = read_duration(read_ok__4)
        var jp99 string
        switch mtmp72.(type) {
        case Result__string__string_Ok:
            var x73 string = mtmp72.(Result__string__string_Ok)._0
            var try_value__27 string = x73
            jp99 = try_value__27
            var value__5 string = jp99
            var t100 string = format_duration(value__5)
            var t101 Result__string__string = Result__string__string_Ok{
                _0: t100,
            }
            retv96 = t101
            return retv96
        case Result__string__string_Err:
            var x74 string = mtmp72.(Result__string__string_Err)._0
            var try_residual__27 string = x74
            var t102 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv96 = t102
            return retv96
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x70 string = mtmp68.(Result__unit__string_Err)._0
        var try_residual__23 string = x70
        var t103 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv96 = t103
        return retv96
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv105 string
    var jp107 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x75 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x75
        var t108 string = "ok " + value__7
        jp107 = t108
    case Result__string__string_Err:
        var x76 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x76
        var t109 string = "err " + err__8
        jp107 = t109
    default:
        panic("non-exhaustive match")
    }
    retv105 = jp107
    return retv105
}

func main0() struct{} {
    var t111 Result__string__string = configure_and_format(true, true)
    var t112 string = show(t111)
    println__T_string(t112)
    var t113 Result__string__string = configure_and_format(true, false)
    var t114 string = show(t113)
    println__T_string(t114)
    var t115 Result__string__string = configure_and_format(false, true)
    var t116 string = show(t115)
    println__T_string(t116)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv121 string
    retv121 = self__38
    return retv121
}

func main() {
    main0()
}
