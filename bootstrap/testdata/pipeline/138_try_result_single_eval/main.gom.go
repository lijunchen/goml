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
    var retv73 Result__int32__string
    var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t75 int32 = t74 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t75)
    var jp77 Result__int32__string
    if ok__1 {
        var t78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t79 Result__int32__string = Ok{
            _0: t78,
        }
        jp77 = t79
    } else {
        var t80 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp77 = t80
    }
    retv73 = jp77
    return retv73
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv82 Result__int32__string
    var mtmp65 Result__int32__string = bump(counter__2, ok__3)
    var jp84 int32
    switch mtmp65.(type) {
    case Ok:
        var x66 int32 = mtmp65.(Ok)._0
        var try_value__23 int32 = x66
        jp84 = try_value__23
        var value__4 int32 = jp84
        var t85 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t86 int32 = value__4 + t85
        var t87 Result__int32__string = Ok{
            _0: t86,
        }
        retv82 = t87
        return retv82
    case Err:
        var x67 string = mtmp65.(Err)._0
        var try_residual__23 string = x67
        var t88 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv82 = t88
        return retv82
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv90 string
    var jp92 string
    switch res__5.(type) {
    case Ok:
        var x68 int32 = res__5.(Ok)._0
        var value__6 int32 = x68
        var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t94 string = "ok " + t93
        jp92 = t94
    case Err:
        var x69 string = res__5.(Err)._0
        var err__7 string = x69
        var t95 string = "err " + err__7
        jp92 = t95
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func run(ok__8 bool) string {
    var retv97 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t98 string = show(result__10)
    var t99 string = t98 + " count="
    var t100 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t101 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t100)
    var t102 string = t99 + t101
    retv97 = t102
    return retv97
}

func main0() struct{} {
    var t104 string = run(true)
    println__T_string(t104)
    var t105 string = run(false)
    println__T_string(t105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv107 int32
    var t108 int32 = ref_get__Ref_5int32(self__210)
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv112 string
    var t113 string = _goml_runtime_core_int32_to_string(self__6)
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv115 *ref_int32_x
    var t116 *ref_int32_x = ref__Ref_5int32(value__209)
    retv115 = t116
    return retv115
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv121 string
    retv121 = self__38
    return retv121
}

func main() {
    main0()
}
