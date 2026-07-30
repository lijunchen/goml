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
    var retv121 Result__unit__string
    var jp123 Result__unit__string
    if ok__0 {
        var t124 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp123 = t124
    } else {
        var t125 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp123 = t125
    }
    retv121 = jp123
    return retv121
}

func read_duration(ok__1 bool) Result__string__string {
    var retv127 Result__string__string
    var jp129 Result__string__string
    if ok__1 {
        var t130 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp129 = t130
    } else {
        var t131 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp129 = t131
    }
    retv127 = jp129
    return retv127
}

func format_duration(value__2 string) string {
    var retv133 string
    var t134 string = "duration=" + value__2
    retv133 = t134
    return retv133
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv136 Result__string__string
    var mtmp108 Result__unit__string = configure(config_ok__3)
    switch mtmp108.(type) {
    case Result__unit__string_Ok:
        var mtmp112 Result__string__string = read_duration(read_ok__4)
        var jp139 string
        switch mtmp112.(type) {
        case Result__string__string_Ok:
            var x113 string = mtmp112.(Result__string__string_Ok)._0
            var try_value__27 string = x113
            jp139 = try_value__27
            var value__5 string = jp139
            var t140 string = format_duration(value__5)
            var t141 Result__string__string = Result__string__string_Ok{
                _0: t140,
            }
            retv136 = t141
            return retv136
        case Result__string__string_Err:
            var x114 string = mtmp112.(Result__string__string_Err)._0
            var try_residual__27 string = x114
            var t142 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv136 = t142
            return retv136
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x110 string = mtmp108.(Result__unit__string_Err)._0
        var try_residual__23 string = x110
        var t143 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv136 = t143
        return retv136
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv145 string
    var jp147 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x115 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x115
        var t148 string = "ok " + value__7
        jp147 = t148
    case Result__string__string_Err:
        var x116 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x116
        var t149 string = "err " + err__8
        jp147 = t149
    default:
        panic("non-exhaustive match")
    }
    retv145 = jp147
    return retv145
}

func main0() struct{} {
    var t151 Result__string__string = configure_and_format(true, true)
    var t152 string = show(t151)
    println__T_string(t152)
    var t153 Result__string__string = configure_and_format(true, false)
    var t154 string = show(t153)
    println__T_string(t154)
    var t155 Result__string__string = configure_and_format(false, true)
    var t156 string = show(t155)
    println__T_string(t156)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t158)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv161 string
    retv161 = self__38
    return retv161
}

func main() {
    main0()
}
