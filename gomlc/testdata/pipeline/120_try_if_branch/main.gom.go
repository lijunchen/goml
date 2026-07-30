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
    var retv117 Result__int32__string
    var jp119 Result__int32__string
    if flag__0 {
        var t120 Result__int32__string = Ok{
            _0: 5,
        }
        jp119 = t120
    } else {
        var t121 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp119 = t121
    }
    retv117 = jp119
    return retv117
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv123 Result__int32__string
    var jp125 int32
    if flag__1 {
        var mtmp108 Result__int32__string = parse(fallback__2)
        var jp129 int32
        switch mtmp108.(type) {
        case Ok:
            var x109 int32 = mtmp108.(Ok)._0
            var try_value__13 int32 = x109
            jp129 = try_value__13
            jp125 = jp129
            var value__3 int32 = jp125
            var t126 int32 = value__3 + 1
            var t127 Result__int32__string = Ok{
                _0: t126,
            }
            retv123 = t127
            return retv123
        case Err:
            var x110 string = mtmp108.(Err)._0
            var try_residual__13 string = x110
            var t130 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv123 = t130
            return retv123
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp125 = 10
        var value__3 int32 = jp125
        var t126 int32 = value__3 + 1
        var t127 Result__int32__string = Ok{
            _0: t126,
        }
        retv123 = t127
        return retv123
    }
}

func show(res__4 Result__int32__string) string {
    var retv132 string
    var jp134 string
    switch res__4.(type) {
    case Ok:
        var x111 int32 = res__4.(Ok)._0
        var value__5 int32 = x111
        var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t136 string = "ok=" + t135
        jp134 = t136
    case Err:
        var x112 string = res__4.(Err)._0
        var err__6 string = x112
        var t137 string = "err=" + err__6
        jp134 = t137
    default:
        panic("non-exhaustive match")
    }
    retv132 = jp134
    return retv132
}

func main0() struct{} {
    var t139 Result__int32__string = bump(true, true)
    var t140 string = show(t139)
    println__T_string(t140)
    var t141 Result__int32__string = bump(true, false)
    var t142 string = show(t141)
    println__T_string(t142)
    var t143 Result__int32__string = bump(false, false)
    var t144 string = show(t143)
    println__T_string(t144)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv146 string
    var t147 string = _goml_runtime_core_int32_to_string(self__6)
    retv146 = t147
    return retv146
}

func println__T_string(value__1 string) struct{} {
    var t149 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv152 string
    retv152 = self__38
    return retv152
}

func main() {
    main0()
}
