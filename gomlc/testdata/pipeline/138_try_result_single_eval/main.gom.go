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
    var retv77 Result__int32__string
    var t78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t79 int32 = t78 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t79)
    var jp81 Result__int32__string
    if ok__1 {
        var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t83 Result__int32__string = Ok{
            _0: t82,
        }
        jp81 = t83
    } else {
        var t84 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp81 = t84
    }
    retv77 = jp81
    return retv77
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv86 Result__int32__string
    var mtmp69 Result__int32__string = bump(counter__2, ok__3)
    var jp88 int32
    switch mtmp69.(type) {
    case Ok:
        var x70 int32 = mtmp69.(Ok)._0
        var try_value__23 int32 = x70
        jp88 = try_value__23
        var value__4 int32 = jp88
        var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t90 int32 = value__4 + t89
        var t91 Result__int32__string = Ok{
            _0: t90,
        }
        retv86 = t91
        return retv86
    case Err:
        var x71 string = mtmp69.(Err)._0
        var try_residual__23 string = x71
        var t92 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv86 = t92
        return retv86
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv94 string
    var jp96 string
    switch res__5.(type) {
    case Ok:
        var x72 int32 = res__5.(Ok)._0
        var value__6 int32 = x72
        var t97 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t98 string = "ok " + t97
        jp96 = t98
    case Err:
        var x73 string = res__5.(Err)._0
        var err__7 string = x73
        var t99 string = "err " + err__7
        jp96 = t99
    default:
        panic("non-exhaustive match")
    }
    retv94 = jp96
    return retv94
}

func run(ok__8 bool) string {
    var retv101 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t102 string = show(result__10)
    var t103 string = t102 + " count="
    var t104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t105 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t104)
    var t106 string = t103 + t105
    retv101 = t106
    return retv101
}

func main0() struct{} {
    var t108 string = run(true)
    println__T_string(t108)
    var t109 string = run(false)
    println__T_string(t109)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv111 int32
    var t112 int32 = ref_get__Ref_5int32(self__208)
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_int32_to_string(self__6)
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv119 *ref_int32_x
    var t120 *ref_int32_x = ref__Ref_5int32(value__207)
    retv119 = t120
    return retv119
}

func println__T_string(value__1 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func main() {
    main0()
}
