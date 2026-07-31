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
    var retv165 Result__unit__string
    var jp167 Result__unit__string
    if ok__0 {
        var t168 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp167 = t168
    } else {
        var t169 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp167 = t169
    }
    retv165 = jp167
    return retv165
}

func read_duration(ok__1 bool) Result__string__string {
    var retv171 Result__string__string
    var jp173 Result__string__string
    if ok__1 {
        var t174 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp173 = t174
    } else {
        var t175 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp173 = t175
    }
    retv171 = jp173
    return retv171
}

func format_duration(value__2 string) string {
    var retv177 string
    var t178 string = "duration=" + value__2
    retv177 = t178
    return retv177
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv180 Result__string__string
    var mtmp152 Result__unit__string = configure(config_ok__3)
    switch mtmp152.(type) {
    case Result__unit__string_Ok:
        var mtmp156 Result__string__string = read_duration(read_ok__4)
        var jp183 string
        switch mtmp156.(type) {
        case Result__string__string_Ok:
            var x157 string = mtmp156.(Result__string__string_Ok)._0
            var try_value__27 string = x157
            jp183 = try_value__27
            var value__5 string = jp183
            var t184 string = format_duration(value__5)
            var t185 Result__string__string = Result__string__string_Ok{
                _0: t184,
            }
            retv180 = t185
            return retv180
        case Result__string__string_Err:
            var x158 string = mtmp156.(Result__string__string_Err)._0
            var try_residual__27 string = x158
            var t186 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv180 = t186
            return retv180
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x154 string = mtmp152.(Result__unit__string_Err)._0
        var try_residual__23 string = x154
        var t187 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv180 = t187
        return retv180
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv189 string
    var jp191 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x159 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x159
        var t192 string = "ok " + value__7
        jp191 = t192
    case Result__string__string_Err:
        var x160 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x160
        var t193 string = "err " + err__8
        jp191 = t193
    default:
        panic("non-exhaustive match")
    }
    retv189 = jp191
    return retv189
}

func main0() struct{} {
    var t195 Result__string__string = configure_and_format(true, true)
    var t196 string = show(t195)
    println__T_string(t196)
    var t197 Result__string__string = configure_and_format(true, false)
    var t198 string = show(t197)
    println__T_string(t198)
    var t199 Result__string__string = configure_and_format(false, true)
    var t200 string = show(t199)
    println__T_string(t200)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv205 string
    retv205 = self__38
    return retv205
}

func main() {
    main0()
}
