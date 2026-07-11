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
    var retv13 Result__int32__string
    var t14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t15 int32 = t14 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t15)
    var jp17 Result__int32__string
    if ok__1 {
        var t18 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t19 Result__int32__string = Ok{
            _0: t18,
        }
        jp17 = t19
    } else {
        var t20 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp17 = t20
    }
    retv13 = jp17
    return retv13
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv22 Result__int32__string
    var mtmp5 Result__int32__string = bump(counter__2, ok__3)
    var jp24 int32
    switch mtmp5.(type) {
    case Ok:
        var x6 int32 = mtmp5.(Ok)._0
        var try_value__23 int32 = x6
        jp24 = try_value__23
        var value__4 int32 = jp24
        var t25 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t26 int32 = value__4 + t25
        var t27 Result__int32__string = Ok{
            _0: t26,
        }
        retv22 = t27
        return retv22
    case Err:
        var x7 string = mtmp5.(Err)._0
        var try_residual__23 string = x7
        var t28 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv22 = t28
        return retv22
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv30 string
    var jp32 string
    switch res__5.(type) {
    case Ok:
        var x8 int32 = res__5.(Ok)._0
        var value__6 int32 = x8
        var t33 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t34 string = "ok " + t33
        jp32 = t34
    case Err:
        var x9 string = res__5.(Err)._0
        var err__7 string = x9
        var t35 string = "err " + err__7
        jp32 = t35
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func run(ok__8 bool) string {
    var retv37 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t38 string = show(result__10)
    var t39 string = t38 + " count="
    var t40 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t40)
    var t42 string = t39 + t41
    retv37 = t42
    return retv37
}

func main0() struct{} {
    var t44 string = run(true)
    println__T_string(t44)
    var t45 string = run(false)
    println__T_string(t45)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv47 int32
    var t48 int32 = ref_get__Ref_5int32(self__103)
    retv47 = t48
    return retv47
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv52 string
    var t53 string = _goml_runtime_core_int32_to_string(self__2)
    retv52 = t53
    return retv52
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv55 *ref_int32_x
    var t56 *ref_int32_x = ref__Ref_5int32(value__102)
    retv55 = t56
    return retv55
}

func println__T_string(value__1 string) struct{} {
    var t58 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t58)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv61 string
    retv61 = self__9
    return retv61
}

func main() {
    main0()
}
