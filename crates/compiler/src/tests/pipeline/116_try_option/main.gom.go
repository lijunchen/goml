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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    var retv64 Option__int32
    var jp66 Option__int32
    if flag__0 {
        var t67 Option__int32 = Some{
            _0: 4,
        }
        jp66 = t67
    } else {
        jp66 = None{}
    }
    retv64 = jp66
    return retv64
}

func add(a__1 int32, b__2 int32) int32 {
    var retv69 int32
    var t70 int32 = a__1 + b__2
    retv69 = t70
    return retv69
}

func plus_two(flag__3 bool) Option__int32 {
    var retv72 Option__int32
    var mtmp58 Option__int32 = maybe_value(flag__3)
    var jp74 int32
    switch mtmp58.(type) {
    case None:
        retv72 = None{}
        return retv72
    case Some:
        var x59 int32 = mtmp58.(Some)._0
        var try_value__15 int32 = x59
        jp74 = try_value__15
        var t75 int32 = add(jp74, 2)
        var t76 Option__int32 = Some{
            _0: t75,
        }
        retv72 = t76
        return retv72
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv78 string
    var jp80 string
    switch opt__4.(type) {
    case None:
        jp80 = "none"
    case Some:
        var x60 int32 = opt__4.(Some)._0
        var value__5 int32 = x60
        var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t82 string = "some=" + t81
        jp80 = t82
    default:
        panic("non-exhaustive match")
    }
    retv78 = jp80
    return retv78
}

func main0() struct{} {
    var t84 Option__int32 = plus_two(true)
    var t85 string = show(t84)
    println__T_string(t85)
    var t86 Option__int32 = plus_two(false)
    var t87 string = show(t86)
    println__T_string(t87)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__2)
    retv89 = t90
    return retv89
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv95 string
    retv95 = self__34
    return retv95
}

func main() {
    main0()
}
