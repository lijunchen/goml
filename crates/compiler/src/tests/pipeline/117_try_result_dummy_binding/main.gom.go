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
    var retv71 Result__unit__string
    var jp73 Result__unit__string
    if ok__0 {
        var t74 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp73 = t74
    } else {
        var t75 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp73 = t75
    }
    retv71 = jp73
    return retv71
}

func read_duration(ok__1 bool) Result__string__string {
    var retv77 Result__string__string
    var jp79 Result__string__string
    if ok__1 {
        var t80 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp79 = t80
    } else {
        var t81 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func format_duration(value__2 string) string {
    var retv83 string
    var t84 string = "duration=" + value__2
    retv83 = t84
    return retv83
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv86 Result__string__string
    var mtmp58 Result__unit__string = configure(config_ok__3)
    switch mtmp58.(type) {
    case Result__unit__string_Ok:
        var mtmp62 Result__string__string = read_duration(read_ok__4)
        var jp89 string
        switch mtmp62.(type) {
        case Result__string__string_Ok:
            var x63 string = mtmp62.(Result__string__string_Ok)._0
            var try_value__27 string = x63
            jp89 = try_value__27
            var value__5 string = jp89
            var t90 string = format_duration(value__5)
            var t91 Result__string__string = Result__string__string_Ok{
                _0: t90,
            }
            retv86 = t91
            return retv86
        case Result__string__string_Err:
            var x64 string = mtmp62.(Result__string__string_Err)._0
            var try_residual__27 string = x64
            var t92 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv86 = t92
            return retv86
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x60 string = mtmp58.(Result__unit__string_Err)._0
        var try_residual__23 string = x60
        var t93 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv86 = t93
        return retv86
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv95 string
    var jp97 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x65 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x65
        var t98 string = "ok " + value__7
        jp97 = t98
    case Result__string__string_Err:
        var x66 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x66
        var t99 string = "err " + err__8
        jp97 = t99
    default:
        panic("non-exhaustive match")
    }
    retv95 = jp97
    return retv95
}

func main0() struct{} {
    var t101 Result__string__string = configure_and_format(true, true)
    var t102 string = show(t101)
    println__T_string(t102)
    var t103 Result__string__string = configure_and_format(true, false)
    var t104 string = show(t103)
    println__T_string(t104)
    var t105 Result__string__string = configure_and_format(false, true)
    var t106 string = show(t105)
    println__T_string(t106)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv111 string
    retv111 = self__34
    return retv111
}

func main() {
    main0()
}
