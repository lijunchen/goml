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
    var retv10 Option__int32
    var jp12 Option__int32
    if flag__0 {
        var t13 Option__int32 = Some{
            _0: 4,
        }
        jp12 = t13
    } else {
        jp12 = None{}
    }
    retv10 = jp12
    return retv10
}

func add(a__1 int32, b__2 int32) int32 {
    var retv15 int32
    var t16 int32 = a__1 + b__2
    retv15 = t16
    return retv15
}

func plus_two(flag__3 bool) Option__int32 {
    var retv18 Option__int32
    var mtmp4 Option__int32 = maybe_value(flag__3)
    var jp20 int32
    switch mtmp4.(type) {
    case None:
        retv18 = None{}
        return retv18
    case Some:
        var x5 int32 = mtmp4.(Some)._0
        var try_value__15 int32 = x5
        jp20 = try_value__15
        var t21 int32 = add(jp20, 2)
        var t22 Option__int32 = Some{
            _0: t21,
        }
        retv18 = t22
        return retv18
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv24 string
    var jp26 string
    switch opt__4.(type) {
    case None:
        jp26 = "none"
    case Some:
        var x6 int32 = opt__4.(Some)._0
        var value__5 int32 = x6
        var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t28 string = "some=" + t27
        jp26 = t28
    default:
        panic("non-exhaustive match")
    }
    retv24 = jp26
    return retv24
}

func main0() struct{} {
    var t30 Option__int32 = plus_two(true)
    var t31 string = show(t30)
    println__T_string(t31)
    var t32 Option__int32 = plus_two(false)
    var t33 string = show(t32)
    println__T_string(t33)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv35 string
    var t36 string = _goml_runtime_core_int32_to_string(self__2)
    retv35 = t36
    return retv35
}

func println__T_string(value__1 string) struct{} {
    var t38 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t38)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv41 string
    retv41 = self__9
    return retv41
}

func main() {
    main0()
}
