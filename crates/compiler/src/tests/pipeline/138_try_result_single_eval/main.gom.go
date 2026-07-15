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
    var retv31 Result__int32__string
    var t32 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t33 int32 = t32 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t33)
    var jp35 Result__int32__string
    if ok__1 {
        var t36 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t37 Result__int32__string = Ok{
            _0: t36,
        }
        jp35 = t37
    } else {
        var t38 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp35 = t38
    }
    retv31 = jp35
    return retv31
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv40 Result__int32__string
    var mtmp23 Result__int32__string = bump(counter__2, ok__3)
    var jp42 int32
    switch mtmp23.(type) {
    case Ok:
        var x24 int32 = mtmp23.(Ok)._0
        var try_value__23 int32 = x24
        jp42 = try_value__23
        var value__4 int32 = jp42
        var t43 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t44 int32 = value__4 + t43
        var t45 Result__int32__string = Ok{
            _0: t44,
        }
        retv40 = t45
        return retv40
    case Err:
        var x25 string = mtmp23.(Err)._0
        var try_residual__23 string = x25
        var t46 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv40 = t46
        return retv40
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv48 string
    var jp50 string
    switch res__5.(type) {
    case Ok:
        var x26 int32 = res__5.(Ok)._0
        var value__6 int32 = x26
        var t51 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t52 string = "ok " + t51
        jp50 = t52
    case Err:
        var x27 string = res__5.(Err)._0
        var err__7 string = x27
        var t53 string = "err " + err__7
        jp50 = t53
    default:
        panic("non-exhaustive match")
    }
    retv48 = jp50
    return retv48
}

func run(ok__8 bool) string {
    var retv55 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t56 string = show(result__10)
    var t57 string = t56 + " count="
    var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t59 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t58)
    var t60 string = t57 + t59
    retv55 = t60
    return retv55
}

func main0() struct{} {
    var t62 string = run(true)
    println__T_string(t62)
    var t63 string = run(false)
    println__T_string(t63)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv65 int32
    var t66 int32 = ref_get__Ref_5int32(self__141)
    retv65 = t66
    return retv65
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int32_to_string(self__2)
    retv70 = t71
    return retv70
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv73 *ref_int32_x
    var t74 *ref_int32_x = ref__Ref_5int32(value__140)
    retv73 = t74
    return retv73
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv79 string
    retv79 = self__9
    return retv79
}

func main() {
    main0()
}
