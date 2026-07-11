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
    var retv16 Result__int32__string
    var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t18 int32 = t17 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t18)
    var jp20 Result__int32__string
    if ok__1 {
        var t21 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t22 Result__int32__string = Ok{
            _0: t21,
        }
        jp20 = t22
    } else {
        var t23 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp20 = t23
    }
    retv16 = jp20
    return retv16
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv25 Result__int32__string
    var mtmp8 Result__int32__string = bump(counter__2, ok__3)
    var jp27 int32
    switch mtmp8.(type) {
    case Ok:
        var x9 int32 = mtmp8.(Ok)._0
        var try_value__23 int32 = x9
        jp27 = try_value__23
        var value__4 int32 = jp27
        var t28 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t29 int32 = value__4 + t28
        var t30 Result__int32__string = Ok{
            _0: t29,
        }
        retv25 = t30
        return retv25
    case Err:
        var x10 string = mtmp8.(Err)._0
        var try_residual__23 string = x10
        var t31 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv25 = t31
        return retv25
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv33 string
    var jp35 string
    switch res__5.(type) {
    case Ok:
        var x11 int32 = res__5.(Ok)._0
        var value__6 int32 = x11
        var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t37 string = "ok " + t36
        jp35 = t37
    case Err:
        var x12 string = res__5.(Err)._0
        var err__7 string = x12
        var t38 string = "err " + err__7
        jp35 = t38
    default:
        panic("non-exhaustive match")
    }
    retv33 = jp35
    return retv33
}

func run(ok__8 bool) string {
    var retv40 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t41 string = show(result__10)
    var t42 string = t41 + " count="
    var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t44 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t43)
    var t45 string = t42 + t44
    retv40 = t45
    return retv40
}

func main0() struct{} {
    var t47 string = run(true)
    println__T_string(t47)
    var t48 string = run(false)
    println__T_string(t48)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv50 int32
    var t51 int32 = ref_get__Ref_5int32(self__115)
    retv50 = t51
    return retv50
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv55 string
    var t56 string = _goml_runtime_core_int32_to_string(self__2)
    retv55 = t56
    return retv55
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv58 *ref_int32_x
    var t59 *ref_int32_x = ref__Ref_5int32(value__114)
    retv58 = t59
    return retv58
}

func println__T_string(value__1 string) struct{} {
    var t61 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t61)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv64 string
    retv64 = self__9
    return retv64
}

func main() {
    main0()
}
