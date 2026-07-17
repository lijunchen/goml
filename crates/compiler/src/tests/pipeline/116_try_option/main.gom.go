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
    var retv67 Option__int32
    var jp69 Option__int32
    if flag__0 {
        var t70 Option__int32 = Some{
            _0: 4,
        }
        jp69 = t70
    } else {
        jp69 = None{}
    }
    retv67 = jp69
    return retv67
}

func add(a__1 int32, b__2 int32) int32 {
    var retv72 int32
    var t73 int32 = a__1 + b__2
    retv72 = t73
    return retv72
}

func plus_two(flag__3 bool) Option__int32 {
    var retv75 Option__int32
    var mtmp61 Option__int32 = maybe_value(flag__3)
    var jp77 int32
    switch mtmp61.(type) {
    case None:
        retv75 = None{}
        return retv75
    case Some:
        var x62 int32 = mtmp61.(Some)._0
        var try_value__15 int32 = x62
        jp77 = try_value__15
        var t78 int32 = add(jp77, 2)
        var t79 Option__int32 = Some{
            _0: t78,
        }
        retv75 = t79
        return retv75
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv81 string
    var jp83 string
    switch opt__4.(type) {
    case None:
        jp83 = "none"
    case Some:
        var x63 int32 = opt__4.(Some)._0
        var value__5 int32 = x63
        var t84 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t85 string = "some=" + t84
        jp83 = t85
    default:
        panic("non-exhaustive match")
    }
    retv81 = jp83
    return retv81
}

func main0() struct{} {
    var t87 Option__int32 = plus_two(true)
    var t88 string = show(t87)
    println__T_string(t88)
    var t89 Option__int32 = plus_two(false)
    var t90 string = show(t89)
    println__T_string(t90)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int32_to_string(self__5)
    retv92 = t93
    return retv92
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv98 string
    retv98 = self__37
    return retv98
}

func main() {
    main0()
}
