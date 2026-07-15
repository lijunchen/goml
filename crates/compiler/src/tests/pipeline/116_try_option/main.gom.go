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
    var retv28 Option__int32
    var jp30 Option__int32
    if flag__0 {
        var t31 Option__int32 = Some{
            _0: 4,
        }
        jp30 = t31
    } else {
        jp30 = None{}
    }
    retv28 = jp30
    return retv28
}

func add(a__1 int32, b__2 int32) int32 {
    var retv33 int32
    var t34 int32 = a__1 + b__2
    retv33 = t34
    return retv33
}

func plus_two(flag__3 bool) Option__int32 {
    var retv36 Option__int32
    var mtmp22 Option__int32 = maybe_value(flag__3)
    var jp38 int32
    switch mtmp22.(type) {
    case None:
        retv36 = None{}
        return retv36
    case Some:
        var x23 int32 = mtmp22.(Some)._0
        var try_value__15 int32 = x23
        jp38 = try_value__15
        var t39 int32 = add(jp38, 2)
        var t40 Option__int32 = Some{
            _0: t39,
        }
        retv36 = t40
        return retv36
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv42 string
    var jp44 string
    switch opt__4.(type) {
    case None:
        jp44 = "none"
    case Some:
        var x24 int32 = opt__4.(Some)._0
        var value__5 int32 = x24
        var t45 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t46 string = "some=" + t45
        jp44 = t46
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func main0() struct{} {
    var t48 Option__int32 = plus_two(true)
    var t49 string = show(t48)
    println__T_string(t49)
    var t50 Option__int32 = plus_two(false)
    var t51 string = show(t50)
    println__T_string(t51)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv53 string
    var t54 string = _goml_runtime_core_int32_to_string(self__2)
    retv53 = t54
    return retv53
}

func println__T_string(value__1 string) struct{} {
    var t56 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t56)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv59 string
    retv59 = self__9
    return retv59
}

func main() {
    main0()
}
