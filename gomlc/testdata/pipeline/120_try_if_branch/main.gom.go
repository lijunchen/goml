package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    var retv164 Result__int32__string
    var jp166 Result__int32__string
    if flag__0 {
        var t167 Result__int32__string = Ok{
            _0: 5,
        }
        jp166 = t167
    } else {
        var t168 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp166 = t168
    }
    retv164 = jp166
    return retv164
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv170 Result__int32__string
    var jp172 int32
    if flag__1 {
        var mtmp155 Result__int32__string = parse(fallback__2)
        var jp176 int32
        switch mtmp155.(type) {
        case Ok:
            var x156 int32 = mtmp155.(Ok)._0
            var try_value__13 int32 = x156
            jp176 = try_value__13
            jp172 = jp176
            var value__3 int32 = jp172
            var t173 int32 = value__3 + 1
            var t174 Result__int32__string = Ok{
                _0: t173,
            }
            retv170 = t174
            return retv170
        case Err:
            var x157 string = mtmp155.(Err)._0
            var try_residual__13 string = x157
            var t177 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv170 = t177
            return retv170
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp172 = 10
        var value__3 int32 = jp172
        var t173 int32 = value__3 + 1
        var t174 Result__int32__string = Ok{
            _0: t173,
        }
        retv170 = t174
        return retv170
    }
}

func show(res__4 Result__int32__string) string {
    var retv179 string
    var jp181 string
    switch res__4.(type) {
    case Ok:
        var x158 int32 = res__4.(Ok)._0
        var value__5 int32 = x158
        var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t183 string = "ok=" + t182
        jp181 = t183
    case Err:
        var x159 string = res__4.(Err)._0
        var err__6 string = x159
        var t184 string = "err=" + err__6
        jp181 = t184
    default:
        panic("non-exhaustive match")
    }
    retv179 = jp181
    return retv179
}

func main0() struct{} {
    var t186 Result__int32__string = bump(true, true)
    var t187 string = show(t186)
    println__T_string(t187)
    var t188 Result__int32__string = bump(true, false)
    var t189 string = show(t188)
    println__T_string(t189)
    var t190 Result__int32__string = bump(false, false)
    var t191 string = show(t190)
    println__T_string(t191)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv193 string
    var t194 string = _goml_runtime_core_int32_to_string(self__6)
    retv193 = t194
    return retv193
}

func println__T_string(value__1 string) struct{} {
    var t196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv199 string
    retv199 = self__38
    return retv199
}

func main() {
    main0()
}
