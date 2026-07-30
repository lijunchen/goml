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
    var retv115 Result__int32__string
    var jp117 Result__int32__string
    if flag__0 {
        var t118 Result__int32__string = Ok{
            _0: 41,
        }
        jp117 = t118
    } else {
        var t119 Result__int32__string = Err{
            _0: "bad",
        }
        jp117 = t119
    }
    retv115 = jp117
    return retv115
}

func compute(flag__1 bool) Result__int32__string {
    var retv121 Result__int32__string
    var mtmp108 Result__int32__string = parse(flag__1)
    var jp123 int32
    switch mtmp108.(type) {
    case Err:
        var x109 string = mtmp108.(Err)._0
        var try_residual__12 string = x109
        var t126 Result__int32__string = Err{
            _0: try_residual__12,
        }
        retv121 = t126
        return retv121
    case Ok:
        var x110 int32 = mtmp108.(Ok)._0
        var try_value__12 int32 = x110
        jp123 = try_value__12
        var value__2 int32 = jp123
        var t124 int32 = value__2 + 1
        var t125 Result__int32__string = Ok{
            _0: t124,
        }
        retv121 = t125
        return retv121
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Result__int32__string) string {
    var retv128 string
    var jp130 string
    switch value__3.(type) {
    case Err:
        var x111 string = value__3.(Err)._0
        var error__5 string = x111
        jp130 = error__5
    case Ok:
        var x112 int32 = value__3.(Ok)._0
        var value__4 int32 = x112
        var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp130 = t131
    default:
        panic("non-exhaustive match")
    }
    retv128 = jp130
    return retv128
}

func main0() struct{} {
    var t133 Result__int32__string = compute(true)
    var t134 string = show(t133)
    println__T_string(t134)
    var t135 Result__int32__string = compute(false)
    var t136 string = show(t135)
    println__T_string(t136)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv139 string
    var t140 string = _goml_runtime_core_int32_to_string(self__6)
    retv139 = t140
    return retv139
}

func println__T_string(value__1 string) struct{} {
    var t142 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t142)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv145 string
    retv145 = self__38
    return retv145
}

func main() {
    main0()
}
