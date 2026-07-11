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
    var retv13 Result__int32__string
    var jp15 Result__int32__string
    if flag__0 {
        var t16 Result__int32__string = Ok{
            _0: 5,
        }
        jp15 = t16
    } else {
        var t17 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp15 = t17
    }
    retv13 = jp15
    return retv13
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv19 Result__int32__string
    var jp21 int32
    if flag__1 {
        var mtmp4 Result__int32__string = parse(fallback__2)
        var jp25 int32
        switch mtmp4.(type) {
        case Ok:
            var x5 int32 = mtmp4.(Ok)._0
            var try_value__13 int32 = x5
            jp25 = try_value__13
            jp21 = jp25
            var value__3 int32 = jp21
            var t22 int32 = value__3 + 1
            var t23 Result__int32__string = Ok{
                _0: t22,
            }
            retv19 = t23
            return retv19
        case Err:
            var x6 string = mtmp4.(Err)._0
            var try_residual__13 string = x6
            var t26 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv19 = t26
            return retv19
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp21 = 10
        var value__3 int32 = jp21
        var t22 int32 = value__3 + 1
        var t23 Result__int32__string = Ok{
            _0: t22,
        }
        retv19 = t23
        return retv19
    }
}

func show(res__4 Result__int32__string) string {
    var retv28 string
    var jp30 string
    switch res__4.(type) {
    case Ok:
        var x7 int32 = res__4.(Ok)._0
        var value__5 int32 = x7
        var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t32 string = "ok=" + t31
        jp30 = t32
    case Err:
        var x8 string = res__4.(Err)._0
        var err__6 string = x8
        var t33 string = "err=" + err__6
        jp30 = t33
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t35 Result__int32__string = bump(true, true)
    var t36 string = show(t35)
    println__T_string(t36)
    var t37 Result__int32__string = bump(true, false)
    var t38 string = show(t37)
    println__T_string(t38)
    var t39 Result__int32__string = bump(false, false)
    var t40 string = show(t39)
    println__T_string(t40)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv42 string
    var t43 string = _goml_runtime_core_int32_to_string(self__2)
    retv42 = t43
    return retv42
}

func println__T_string(value__1 string) struct{} {
    var t45 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t45)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv48 string
    retv48 = self__9
    return retv48
}

func main() {
    main0()
}
