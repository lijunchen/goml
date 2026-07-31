package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    var retv163 Result__string__string
    var jp165 Result__string__string
    if ok__0 {
        var t166 Result__string__string = Ok{
            _0: "goml",
        }
        jp165 = t166
    } else {
        var t167 Result__string__string = Err{
            _0: "parse failed",
        }
        jp165 = t167
    }
    retv163 = jp165
    return retv163
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv169 Result__string__string
    var mtmp152 Result__string__string = parse_text(ok__1)
    var jp171 string
    switch mtmp152.(type) {
    case Ok:
        var x153 string = mtmp152.(Ok)._0
        var try_value__12 string = x153
        jp171 = try_value__12
        var text__2 string = jp171
        var t172 string = text__2 + "!"
        var t173 Result__string__string = Ok{
            _0: t172,
        }
        retv169 = t173
        return retv169
    case Err:
        var x154 string = mtmp152.(Err)._0
        var try_residual__12 string = x154
        var t174 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv169 = t174
        return retv169
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv176 Result__string__string
    var mtmp155 Result__string__string = normalize_text(ok__3)
    var jp178 string
    switch mtmp155.(type) {
    case Ok:
        var x156 string = mtmp155.(Ok)._0
        var try_value__20 string = x156
        jp178 = try_value__20
        var text__4 string = jp178
        var t179 string = "[" + text__4
        var t180 string = t179 + "]"
        var t181 Result__string__string = Ok{
            _0: t180,
        }
        retv176 = t181
        return retv176
    case Err:
        var x157 string = mtmp155.(Err)._0
        var try_residual__20 string = x157
        var t182 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv176 = t182
        return retv176
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv184 string
    var jp186 string
    switch res__5.(type) {
    case Ok:
        var x158 string = res__5.(Ok)._0
        var value__6 string = x158
        var t187 string = "ok " + value__6
        jp186 = t187
    case Err:
        var x159 string = res__5.(Err)._0
        var err__7 string = x159
        var t188 string = "err " + err__7
        jp186 = t188
    default:
        panic("non-exhaustive match")
    }
    retv184 = jp186
    return retv184
}

func main0() struct{} {
    var t190 Result__string__string = decorate_text(true)
    var t191 string = show(t190)
    println__T_string(t191)
    var t192 Result__string__string = decorate_text(false)
    var t193 string = show(t192)
    println__T_string(t193)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv198 string
    retv198 = self__38
    return retv198
}

func main() {
    main0()
}
