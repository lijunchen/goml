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
    var retv117 Result__int32__string
    var t118 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t119 int32 = t118 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t119)
    var jp121 Result__int32__string
    if ok__1 {
        var t122 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t123 Result__int32__string = Ok{
            _0: t122,
        }
        jp121 = t123
    } else {
        var t124 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp121 = t124
    }
    retv117 = jp121
    return retv117
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv126 Result__int32__string
    var mtmp109 Result__int32__string = bump(counter__2, ok__3)
    var jp128 int32
    switch mtmp109.(type) {
    case Ok:
        var x110 int32 = mtmp109.(Ok)._0
        var try_value__23 int32 = x110
        jp128 = try_value__23
        var value__4 int32 = jp128
        var t129 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t130 int32 = value__4 + t129
        var t131 Result__int32__string = Ok{
            _0: t130,
        }
        retv126 = t131
        return retv126
    case Err:
        var x111 string = mtmp109.(Err)._0
        var try_residual__23 string = x111
        var t132 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv126 = t132
        return retv126
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv134 string
    var jp136 string
    switch res__5.(type) {
    case Ok:
        var x112 int32 = res__5.(Ok)._0
        var value__6 int32 = x112
        var t137 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t138 string = "ok " + t137
        jp136 = t138
    case Err:
        var x113 string = res__5.(Err)._0
        var err__7 string = x113
        var t139 string = "err " + err__7
        jp136 = t139
    default:
        panic("non-exhaustive match")
    }
    retv134 = jp136
    return retv134
}

func run(ok__8 bool) string {
    var retv141 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t142 string = show(result__10)
    var t143 string = t142 + " count="
    var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t145 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t144)
    var t146 string = t143 + t145
    retv141 = t146
    return retv141
}

func main0() struct{} {
    var t148 string = run(true)
    println__T_string(t148)
    var t149 string = run(false)
    println__T_string(t149)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv151 int32
    var t152 int32 = ref_get__Ref_5int32(self__208)
    retv151 = t152
    return retv151
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv156 string
    var t157 string = _goml_runtime_core_int32_to_string(self__6)
    retv156 = t157
    return retv156
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv159 *ref_int32_x
    var t160 *ref_int32_x = ref__Ref_5int32(value__207)
    retv159 = t160
    return retv159
}

func println__T_string(value__1 string) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv165 string
    retv165 = self__38
    return retv165
}

func main() {
    main0()
}
