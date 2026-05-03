package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
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

func bump(counter__0 *ref_int32_x, ok__1 bool) Result__int32__string {
    var retv9 Result__int32__string
    var t10 int32 = ref_get__Ref_5int32(counter__0)
    var t11 int32 = t10 + 1
    ref_set__Ref_5int32(counter__0, t11)
    var jp13 Result__int32__string
    if ok__1 {
        var t14 int32 = ref_get__Ref_5int32(counter__0)
        var t15 Result__int32__string = Ok{
            _0: t14,
        }
        jp13 = t15
    } else {
        var t16 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp13 = t16
    }
    retv9 = jp13
    return retv9
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv18 Result__int32__string
    var mtmp1 Result__int32__string = bump(counter__2, ok__3)
    var jp20 int32
    switch mtmp1.(type) {
    case Ok:
        var x2 int32 = mtmp1.(Ok)._0
        var try_value__23 int32 = x2
        jp20 = try_value__23
        var value__4 int32 = jp20
        var t21 int32 = ref_get__Ref_5int32(counter__2)
        var t22 int32 = value__4 + t21
        var t23 Result__int32__string = Ok{
            _0: t22,
        }
        retv18 = t23
        return retv18
    case Err:
        var x3 string = mtmp1.(Err)._0
        var try_residual__23 string = x3
        var t24 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv18 = t24
        return retv18
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv26 string
    var jp28 string
    switch res__5.(type) {
    case Ok:
        var x4 int32 = res__5.(Ok)._0
        var value__6 int32 = x4
        var t29 string = int32_to_string(value__6)
        var t30 string = "ok " + t29
        jp28 = t30
    case Err:
        var x5 string = res__5.(Err)._0
        var err__7 string = x5
        var t31 string = "err " + err__7
        jp28 = t31
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func run(ok__8 bool) string {
    var retv33 string
    var counter__9 *ref_int32_x = ref__Ref_5int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t34 string = show(result__10)
    var t35 string = t34 + " count="
    var t36 int32 = ref_get__Ref_5int32(counter__9)
    var t37 string = int32_to_string(t36)
    var t38 string = t35 + t37
    retv33 = t38
    return retv33
}

func main0() struct{} {
    var t40 string = run(true)
    println__T_string(t40)
    var t41 string = run(false)
    println__T_string(t41)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
