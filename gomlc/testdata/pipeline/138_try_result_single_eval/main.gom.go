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
    var retv164 Result__int32__string
    var t165 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
    var t166 int32 = t165 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__0, t166)
    var jp168 Result__int32__string
    if ok__1 {
        var t169 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__0)
        var t170 Result__int32__string = Ok{
            _0: t169,
        }
        jp168 = t170
    } else {
        var t171 Result__int32__string = Err{
            _0: "bump failed",
        }
        jp168 = t171
    }
    retv164 = jp168
    return retv164
}

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var retv173 Result__int32__string
    var mtmp156 Result__int32__string = bump(counter__2, ok__3)
    var jp175 int32
    switch mtmp156.(type) {
    case Ok:
        var x157 int32 = mtmp156.(Ok)._0
        var try_value__23 int32 = x157
        jp175 = try_value__23
        var value__4 int32 = jp175
        var t176 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var t177 int32 = value__4 + t176
        var t178 Result__int32__string = Ok{
            _0: t177,
        }
        retv173 = t178
        return retv173
    case Err:
        var x158 string = mtmp156.(Err)._0
        var try_residual__23 string = x158
        var t179 Result__int32__string = Err{
            _0: try_residual__23,
        }
        retv173 = t179
        return retv173
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    var retv181 string
    var jp183 string
    switch res__5.(type) {
    case Ok:
        var x159 int32 = res__5.(Ok)._0
        var value__6 int32 = x159
        var t184 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t185 string = "ok " + t184
        jp183 = t185
    case Err:
        var x160 string = res__5.(Err)._0
        var err__7 string = x160
        var t186 string = "err " + err__7
        jp183 = t186
    default:
        panic("non-exhaustive match")
    }
    retv181 = jp183
    return retv181
}

func run(ok__8 bool) string {
    var retv188 string
    var counter__9 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var result__10 Result__int32__string = use_try(counter__9, ok__8)
    var t189 string = show(result__10)
    var t190 string = t189 + " count="
    var t191 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__9)
    var t192 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t191)
    var t193 string = t190 + t192
    retv188 = t193
    return retv188
}

func main0() struct{} {
    var t195 string = run(true)
    println__T_string(t195)
    var t196 string = run(false)
    println__T_string(t196)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv198 int32
    var t199 int32 = ref_get__Ref_5int32(self__208)
    retv198 = t199
    return retv198
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv203 string
    var t204 string = _goml_runtime_core_int32_to_string(self__6)
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv206 *ref_int32_x
    var t207 *ref_int32_x = ref__Ref_5int32(value__207)
    retv206 = t207
    return retv206
}

func println__T_string(value__1 string) struct{} {
    var t209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv212 string
    retv212 = self__38
    return retv212
}

func main() {
    main0()
}
