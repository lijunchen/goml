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
    if flag__0 {
        var t165 Result__int32__string = Ok{
            _0: 41,
        }
        return t165
    } else {
        var t166 Result__int32__string = Err{
            _0: "bad",
        }
        return t166
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp155 Result__int32__string = parse(flag__1)
    var jp170 int32
    switch mtmp155.(type) {
    case Err:
        var x156 string = mtmp155.(Err)._0
        var t173 Result__int32__string = Err{
            _0: x156,
        }
        return t173
    case Ok:
        var x157 int32 = mtmp155.(Ok)._0
        jp170 = x157
        var t171 int32 = jp170 + 1
        var t172 Result__int32__string = Ok{
            _0: t171,
        }
        return t172
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Result__int32__string) string {
    switch value__3.(type) {
    case Err:
        var x158 string = value__3.(Err)._0
        return x158
    case Ok:
        var x159 int32 = value__3.(Ok)._0
        var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x159)
        return t178
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t180 Result__int32__string = compute(true)
    var t181 string = show(t180)
    println__T_string(t181)
    var t182 Result__int32__string = compute(false)
    var t183 string = show(t182)
    println__T_string(t183)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t187 string = _goml_runtime_core_int32_to_string(self__6)
    return t187
}

func println__T_string(value__1 string) struct{} {
    var t189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
