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

func maybe_seed(flag__0 bool) Option__int32 {
    var retv30 Option__int32
    var jp32 Option__int32
    if flag__0 {
        var t33 Option__int32 = Some{
            _0: 3,
        }
        jp32 = t33
    } else {
        jp32 = None{}
    }
    retv30 = jp32
    return retv30
}

func maybe_double(value__1 int32) Option__int32 {
    var retv35 Option__int32
    var t38 bool = value__1 > 0
    var jp37 Option__int32
    if t38 {
        var t39 int32 = value__1 * 2
        var t40 Option__int32 = Some{
            _0: t39,
        }
        jp37 = t40
    } else {
        jp37 = None{}
    }
    retv35 = jp37
    return retv35
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv42 Option__int32
    var mtmp22 Option__int32 = maybe_seed(flag__2)
    var jp44 int32
    switch mtmp22.(type) {
    case None:
        retv42 = None{}
        return retv42
    case Some:
        var x23 int32 = mtmp22.(Some)._0
        var try_value__22 int32 = x23
        jp44 = try_value__22
        var a__3 int32 = jp44
        var mtmp24 Option__int32 = maybe_double(a__3)
        var jp46 int32
        switch mtmp24.(type) {
        case None:
            retv42 = None{}
            return retv42
        case Some:
            var x25 int32 = mtmp24.(Some)._0
            var try_value__26 int32 = x25
            jp46 = try_value__26
            var b__4 int32 = jp46
            var t47 int32 = a__3 + b__4
            var t48 Option__int32 = Some{
                _0: t47,
            }
            retv42 = t48
            return retv42
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv50 string
    var jp52 string
    switch opt__5.(type) {
    case None:
        jp52 = "none"
    case Some:
        var x26 int32 = opt__5.(Some)._0
        var value__6 int32 = x26
        var t53 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t54 string = "some=" + t53
        jp52 = t54
    default:
        panic("non-exhaustive match")
    }
    retv50 = jp52
    return retv50
}

func main0() struct{} {
    var t56 Option__int32 = maybe_total(true)
    var t57 string = show(t56)
    println__T_string(t57)
    var t58 Option__int32 = maybe_total(false)
    var t59 string = show(t58)
    println__T_string(t59)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv61 string
    var t62 string = _goml_runtime_core_int32_to_string(self__2)
    retv61 = t62
    return retv61
}

func println__T_string(value__1 string) struct{} {
    var t64 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv67 string
    retv67 = self__9
    return retv67
}

func main() {
    main0()
}
