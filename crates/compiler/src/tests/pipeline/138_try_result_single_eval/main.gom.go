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
    var retv70 Result__int32__string
    var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t72 int32 = t71 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t72)
    var jp74 Result__int32__string
    if ok__1 {
        var t75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t76 Result__int32__string = Ok{
            _0: t75,
        }
        jp74 = t76
    } else {
        var t77 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp74 = t77
    }
    retv70 = jp74
    return retv70
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv79 Result__int32__string
    var mtmp62 Result__int32__string = bump(counter__2, ok__3)
    var jp81 int32
    switch mtmp62.(type) {
    case Ok:
        var x63 int32 = mtmp62.(Ok)._0
        var try_value__23 int32 = x63
        jp81 = try_value__23
        var value__4 int32 = jp81
        var t82 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t83 int32 = value__4 + t82
        var t84 Result__int32__string = Ok{
            _0: t83,
        }
        retv79 = t84
        return retv79
    case Err:
        var x64 string = mtmp62.(Err)._0
        var try_residual__23 string = x64
        var t85 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv79 = t85
        return retv79
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv87 string
    var jp89 string
    switch res__5.(type) {
    case Ok:
        var x65 int32 = res__5.(Ok)._0
        var value__6 int32 = x65
        var t90 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t91 string = "ok " + t90
        jp89 = t91
    case Err:
        var x66 string = res__5.(Err)._0
        var err__7 string = x66
        var t92 string = "err " + err__7
        jp89 = t92
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func run(ok__8 bool) string {
    var retv94 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t95 string = show(result__10)
    var t96 string = t95 + " count="
    var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t97)
    var t99 string = t96 + t98
    retv94 = t99
    return retv94
}

func main0() struct{} {
    var t101 string = run(true)
    println__T_string(t101)
    var t102 string = run(false)
    println__T_string(t102)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv104 int32
    var t105 int32 = ref_get__Ref_5int32(self__205)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv109 string
    var t110 string = _goml_runtime_core_int32_to_string(self__5)
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv112 *ref_int32_x
    var t113 *ref_int32_x = ref__Ref_5int32(value__204)
    retv112 = t113
    return retv112
}

func println__T_string(value__1 string) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv118 string
    retv118 = self__37
    return retv118
}

func main() {
    main0()
}
