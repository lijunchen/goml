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
    var retv67 Result__int32__string
    var t68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t69 int32 = t68 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t69)
    var jp71 Result__int32__string
    if ok__1 {
        var t72 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t73 Result__int32__string = Ok{
            _0: t72,
        }
        jp71 = t73
    } else {
        var t74 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp71 = t74
    }
    retv67 = jp71
    return retv67
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv76 Result__int32__string
    var mtmp59 Result__int32__string = bump(counter__2, ok__3)
    var jp78 int32
    switch mtmp59.(type) {
    case Ok:
        var x60 int32 = mtmp59.(Ok)._0
        var try_value__23 int32 = x60
        jp78 = try_value__23
        var value__4 int32 = jp78
        var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t80 int32 = value__4 + t79
        var t81 Result__int32__string = Ok{
            _0: t80,
        }
        retv76 = t81
        return retv76
    case Err:
        var x61 string = mtmp59.(Err)._0
        var try_residual__23 string = x61
        var t82 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv76 = t82
        return retv76
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv84 string
    var jp86 string
    switch res__5.(type) {
    case Ok:
        var x62 int32 = res__5.(Ok)._0
        var value__6 int32 = x62
        var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t88 string = "ok " + t87
        jp86 = t88
    case Err:
        var x63 string = res__5.(Err)._0
        var err__7 string = x63
        var t89 string = "err " + err__7
        jp86 = t89
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func run(ok__8 bool) string {
    var retv91 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t92 string = show(result__10)
    var t93 string = t92 + " count="
    var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t94)
    var t96 string = t93 + t95
    retv91 = t96
    return retv91
}

func main0() struct{} {
    var t98 string = run(true)
    println__T_string(t98)
    var t99 string = run(false)
    println__T_string(t99)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv101 int32
    var t102 int32 = ref_get__Ref_5int32(self__202)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv106 string
    var t107 string = _goml_runtime_core_int32_to_string(self__2)
    retv106 = t107
    return retv106
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv109 *ref_int32_x
    var t110 *ref_int32_x = ref__Ref_5int32(value__201)
    retv109 = t110
    return retv109
}

func println__T_string(value__1 string) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv115 string
    retv115 = self__34
    return retv115
}

func main() {
    main0()
}
