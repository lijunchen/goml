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
    var retv161 Result__int32__string
    var t162 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t163 int32 = t162 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t163)
    var jp165 Result__int32__string
    if ok__1 {
        var t166 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t167 Result__int32__string = Ok{
            _0: t166,
        }
        jp165 = t167
    } else {
        var t168 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp165 = t168
    }
    retv161 = jp165
    return retv161
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv170 Result__int32__string
    var mtmp153 Result__int32__string = bump(counter__2, ok__3)
    var jp172 int32
    switch mtmp153.(type) {
    case Ok:
        var x154 int32 = mtmp153.(Ok)._0
        var try_value__23 int32 = x154
        jp172 = try_value__23
        var value__4 int32 = jp172
        var t173 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t174 int32 = value__4 + t173
        var t175 Result__int32__string = Ok{
            _0: t174,
        }
        retv170 = t175
        return retv170
    case Err:
        var x155 string = mtmp153.(Err)._0
        var try_residual__23 string = x155
        var t176 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv170 = t176
        return retv170
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv178 string
    var jp180 string
    switch res__5.(type) {
    case Ok:
        var x156 int32 = res__5.(Ok)._0
        var value__6 int32 = x156
        var t181 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t182 string = "ok " + t181
        jp180 = t182
    case Err:
        var x157 string = res__5.(Err)._0
        var err__7 string = x157
        var t183 string = "err " + err__7
        jp180 = t183
    default:
        panic("non-exhaustive match")
    }
    retv178 = jp180
    return retv178
}

func run(ok__8 bool) string {
    var retv185 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t186 string = show(result__10)
    var t187 string = t186 + " count="
    var t188 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t189 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t188)
    var t190 string = t187 + t189
    retv185 = t190
    return retv185
}

func main0() struct{} {
    var t192 string = run(true)
    println__T_string(t192)
    var t193 string = run(false)
    println__T_string(t193)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv195 int32
    var t196 int32 = ref_get__Ref_5int32(self__208)
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv200 string
    var t201 string = _goml_runtime_core_int32_to_string(self__6)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv203 *ref_int32_x
    var t204 *ref_int32_x = ref__Ref_5int32(value__207)
    retv203 = t204
    return retv203
}

func println__T_string(value__1 string) struct{} {
    var t206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv209 string
    retv209 = self__38
    return retv209
}

func main() {
    main0()
}
