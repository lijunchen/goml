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

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    var retv159 Result__int32__string
    var jp161 Result__int32__string
    if flag__0 {
        var t162 Result__int32__string = Ok{
            _0: 41,
        }
        jp161 = t162
    } else {
        var t163 Result__int32__string = Err{
            _0: "bad",
        }
        jp161 = t163
    }
    retv159 = jp161
    return retv159
}

func compute(flag__1 bool) Result__int32__string {
    var retv165 Result__int32__string
    var mtmp152 Result__int32__string = parse(flag__1)
    var jp167 int32
    switch mtmp152.(type) {
    case Err:
        var x153 string = mtmp152.(Err)._0
        var try_residual__12 string = x153
        var t170 Result__int32__string = Err{
            _0: try_residual__12,
        }
        retv165 = t170
        return retv165
    case Ok:
        var x154 int32 = mtmp152.(Ok)._0
        var try_value__12 int32 = x154
        jp167 = try_value__12
        var value__2 int32 = jp167
        var t168 int32 = value__2 + 1
        var t169 Result__int32__string = Ok{
            _0: t168,
        }
        retv165 = t169
        return retv165
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Result__int32__string) string {
    var retv172 string
    var jp174 string
    switch value__3.(type) {
    case Err:
        var x155 string = value__3.(Err)._0
        var error__5 string = x155
        jp174 = error__5
    case Ok:
        var x156 int32 = value__3.(Ok)._0
        var value__4 int32 = x156
        var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp174 = t175
    default:
        panic("non-exhaustive match")
    }
    retv172 = jp174
    return retv172
}

func main0() struct{} {
    var t177 Result__int32__string = compute(true)
    var t178 string = show(t177)
    println__T_string(t178)
    var t179 Result__int32__string = compute(false)
    var t180 string = show(t179)
    println__T_string(t180)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv183 string
    var t184 string = _goml_runtime_core_int32_to_string(self__6)
    retv183 = t184
    return retv183
}

func println__T_string(value__1 string) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv189 string
    retv189 = self__38
    return retv189
}

func main() {
    main0()
}
