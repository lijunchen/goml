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
    var retv168 Result__unit__string
    var jp170 Result__unit__string
    if ok__0 {
        var t171 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp170 = t171
    } else {
        var t172 Result__unit__string = Result__unit__string_Err{
            _0: "config failed",
        }
        jp170 = t172
    }
    retv168 = jp170
    return retv168
}

func read_duration(ok__1 bool) Result__string__string {
    var retv174 Result__string__string
    var jp176 Result__string__string
    if ok__1 {
        var t177 Result__string__string = Result__string__string_Ok{
            _0: "2s",
        }
        jp176 = t177
    } else {
        var t178 Result__string__string = Result__string__string_Err{
            _0: "duration failed",
        }
        jp176 = t178
    }
    retv174 = jp176
    return retv174
}

func format_duration(value__2 string) string {
    var retv180 string
    var t181 string = "duration=" + value__2
    retv180 = t181
    return retv180
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var retv183 Result__string__string
    var mtmp155 Result__unit__string = configure(config_ok__3)
    switch mtmp155.(type) {
    case Result__unit__string_Ok:
        var mtmp159 Result__string__string = read_duration(read_ok__4)
        var jp186 string
        switch mtmp159.(type) {
        case Result__string__string_Ok:
            var x160 string = mtmp159.(Result__string__string_Ok)._0
            var try_value__27 string = x160
            jp186 = try_value__27
            var value__5 string = jp186
            var t187 string = format_duration(value__5)
            var t188 Result__string__string = Result__string__string_Ok{
                _0: t187,
            }
            retv183 = t188
            return retv183
        case Result__string__string_Err:
            var x161 string = mtmp159.(Result__string__string_Err)._0
            var try_residual__27 string = x161
            var t189 Result__string__string = Result__string__string_Err{
                _0: try_residual__27,
            }
            retv183 = t189
            return retv183
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__string_Err:
        var x157 string = mtmp155.(Result__unit__string_Err)._0
        var try_residual__23 string = x157
        var t190 Result__string__string = Result__string__string_Err{
            _0: try_residual__23,
        }
        retv183 = t190
        return retv183
    default:
        panic("non-exhaustive match")
    }
}

func show(res__6 Result__string__string) string {
    var retv192 string
    var jp194 string
    switch res__6.(type) {
    case Result__string__string_Ok:
        var x162 string = res__6.(Result__string__string_Ok)._0
        var value__7 string = x162
        var t195 string = "ok " + value__7
        jp194 = t195
    case Result__string__string_Err:
        var x163 string = res__6.(Result__string__string_Err)._0
        var err__8 string = x163
        var t196 string = "err " + err__8
        jp194 = t196
    default:
        panic("non-exhaustive match")
    }
    retv192 = jp194
    return retv192
}

func main0() struct{} {
    var t198 Result__string__string = configure_and_format(true, true)
    var t199 string = show(t198)
    println__T_string(t199)
    var t200 Result__string__string = configure_and_format(true, false)
    var t201 string = show(t200)
    println__T_string(t201)
    var t202 Result__string__string = configure_and_format(false, true)
    var t203 string = show(t202)
    println__T_string(t203)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv208 string
    retv208 = self__38
    return retv208
}

func main() {
    main0()
}
