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
    var retv161 Result__int32__string
    var jp163 Result__int32__string
    if flag__0 {
        var t164 Result__int32__string = Ok{
            _0: 5,
        }
        jp163 = t164
    } else {
        var t165 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp163 = t165
    }
    retv161 = jp163
    return retv161
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv167 Result__int32__string
    var jp169 int32
    if flag__1 {
        var mtmp152 Result__int32__string = parse(fallback__2)
        var jp173 int32
        switch mtmp152.(type) {
        case Ok:
            var x153 int32 = mtmp152.(Ok)._0
            var try_value__13 int32 = x153
            jp173 = try_value__13
            jp169 = jp173
            var value__3 int32 = jp169
            var t170 int32 = value__3 + 1
            var t171 Result__int32__string = Ok{
                _0: t170,
            }
            retv167 = t171
            return retv167
        case Err:
            var x154 string = mtmp152.(Err)._0
            var try_residual__13 string = x154
            var t174 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv167 = t174
            return retv167
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp169 = 10
        var value__3 int32 = jp169
        var t170 int32 = value__3 + 1
        var t171 Result__int32__string = Ok{
            _0: t170,
        }
        retv167 = t171
        return retv167
    }
}

func show(res__4 Result__int32__string) string {
    var retv176 string
    var jp178 string
    switch res__4.(type) {
    case Ok:
        var x155 int32 = res__4.(Ok)._0
        var value__5 int32 = x155
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t180 string = "ok=" + t179
        jp178 = t180
    case Err:
        var x156 string = res__4.(Err)._0
        var err__6 string = x156
        var t181 string = "err=" + err__6
        jp178 = t181
    default:
        panic("non-exhaustive match")
    }
    retv176 = jp178
    return retv176
}

func main0() struct{} {
    var t183 Result__int32__string = bump(true, true)
    var t184 string = show(t183)
    println__T_string(t184)
    var t185 Result__int32__string = bump(true, false)
    var t186 string = show(t185)
    println__T_string(t186)
    var t187 Result__int32__string = bump(false, false)
    var t188 string = show(t187)
    println__T_string(t188)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv190 string
    var t191 string = _goml_runtime_core_int32_to_string(self__6)
    retv190 = t191
    return retv190
}

func println__T_string(value__1 string) struct{} {
    var t193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv196 string
    retv196 = self__38
    return retv196
}

func main() {
    main0()
}
