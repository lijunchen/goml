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

func use_try(counter__2 *ref_int32_x, ok__3 bool) Result__int32__string {
    var mtmp137 Result__int32__string
    var inline209 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline210 int32 = inline209 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline210)
    if ok__3 {
        var inline212 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline213 Result__int32__string = Ok{
            _0: inline212,
        }
        mtmp137 = inline213
    } else {
        var inline214 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp137 = inline214
    }
    var jp156 int32
    switch mtmp137.(type) {
    case Ok:
        var x138 int32 = mtmp137.(Ok)._0
        jp156 = x138
        var t157 int32
        var inline207 int32 = ref_get__Ref_5int32(counter__2)
        t157 = inline207
        var t158 int32 = jp156 + t157
        var t159 Result__int32__string = Ok{
            _0: t158,
        }
        return t159
    case Err:
        var x139 string = mtmp137.(Err)._0
        var t160 Result__int32__string = Err{
            _0: x139,
        }
        return t160
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x140 int32 = res__5.(Ok)._0
        var t165 string
        var inline216 string = _goml_runtime_core_int32_to_string(x140)
        t165 = inline216
        var t166 string = "ok " + t165
        return t166
    case Err:
        var x141 string = res__5.(Err)._0
        var t167 string = "err " + x141
        return t167
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t176 string
    var inline261 bool = true
    var inline262 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline263 Result__int32__string = use_try(inline262, inline261)
    var inline264 string = show(inline263)
    var inline265 string = inline264 + " count="
    var inline266 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline262)
    var inline267 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline266)
    var inline268 string = inline265 + inline267
    t176 = inline268
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline258)
    var t177 string
    var inline249 bool = false
    var inline250 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline251 Result__int32__string = use_try(inline250, inline249)
    var inline252 string = show(inline251)
    var inline253 string = inline252 + " count="
    var inline254 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline250)
    var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline254)
    var inline256 string = inline253 + inline255
    t177 = inline256
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline246)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__233 *ref_int32_x) int32 {
    var t180 int32 = ref_get__Ref_5int32(self__233)
    return t180
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__234 *ref_int32_x, value__235 int32) struct{} {
    ref_set__Ref_5int32(self__234, value__235)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t185 string = _goml_runtime_core_int32_to_string(self__35)
    return t185
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__232 int32) *ref_int32_x {
    var t188 *ref_int32_x = ref__Ref_5int32(value__232)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
