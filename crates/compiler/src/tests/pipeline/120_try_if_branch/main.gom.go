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
    var retv16 Result__int32__string
    var jp18 Result__int32__string
    if flag__0 {
        var t19 Result__int32__string = Ok{
            _0: 5,
        }
        jp18 = t19
    } else {
        var t20 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp18 = t20
    }
    retv16 = jp18
    return retv16
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv22 Result__int32__string
    var jp24 int32
    if flag__1 {
        var mtmp7 Result__int32__string = parse(fallback__2)
        var jp28 int32
        switch mtmp7.(type) {
        case Ok:
            var x8 int32 = mtmp7.(Ok)._0
            var try_value__13 int32 = x8
            jp28 = try_value__13
            jp24 = jp28
            var value__3 int32 = jp24
            var t25 int32 = value__3 + 1
            var t26 Result__int32__string = Ok{
                _0: t25,
            }
            retv22 = t26
            return retv22
        case Err:
            var x9 string = mtmp7.(Err)._0
            var try_residual__13 string = x9
            var t29 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv22 = t29
            return retv22
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp24 = 10
        var value__3 int32 = jp24
        var t25 int32 = value__3 + 1
        var t26 Result__int32__string = Ok{
            _0: t25,
        }
        retv22 = t26
        return retv22
    }
}

func show(res__4 Result__int32__string) string {
    var retv31 string
    var jp33 string
    switch res__4.(type) {
    case Ok:
        var x10 int32 = res__4.(Ok)._0
        var value__5 int32 = x10
        var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t35 string = "ok=" + t34
        jp33 = t35
    case Err:
        var x11 string = res__4.(Err)._0
        var err__6 string = x11
        var t36 string = "err=" + err__6
        jp33 = t36
    default:
        panic("non-exhaustive match")
    }
    retv31 = jp33
    return retv31
}

func main0() struct{} {
    var t38 Result__int32__string = bump(true, true)
    var t39 string = show(t38)
    println__T_string(t39)
    var t40 Result__int32__string = bump(true, false)
    var t41 string = show(t40)
    println__T_string(t41)
    var t42 Result__int32__string = bump(false, false)
    var t43 string = show(t42)
    println__T_string(t43)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv45 string
    var t46 string = _goml_runtime_core_int32_to_string(self__2)
    retv45 = t46
    return retv45
}

func println__T_string(value__1 string) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv51 string
    retv51 = self__9
    return retv51
}

func main() {
    main0()
}
