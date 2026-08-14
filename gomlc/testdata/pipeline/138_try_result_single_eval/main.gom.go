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
    var mtmp183 Result__int32__string
    var inline255 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline256 int32 = inline255 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline256)
    if ok__3 {
        var inline258 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline259 Result__int32__string = Ok{
            _0: inline258,
        }
        mtmp183 = inline259
    } else {
        var inline260 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp183 = inline260
    }
    var jp202 int32
    switch mtmp183.(type) {
    case Ok:
        var x184 int32 = mtmp183.(Ok)._0
        jp202 = x184
        var t203 int32
        var inline253 int32 = ref_get__Ref_5int32(counter__2)
        t203 = inline253
        var t204 int32 = jp202 + t203
        var t205 Result__int32__string = Ok{
            _0: t204,
        }
        return t205
    case Err:
        var x185 string = mtmp183.(Err)._0
        var t206 Result__int32__string = Err{
            _0: x185,
        }
        return t206
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x186 int32 = res__5.(Ok)._0
        var t211 string
        var inline262 string = _goml_runtime_core_int32_to_string(x186)
        t211 = inline262
        var t212 string = "ok " + t211
        return t212
    case Err:
        var x187 string = res__5.(Err)._0
        var t213 string = "err " + x187
        return t213
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t222 string
    var inline307 bool = true
    var inline308 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline309 Result__int32__string = use_try(inline308, inline307)
    var inline310 string = show(inline309)
    var inline311 string = inline310 + " count="
    var inline312 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline308)
    var inline313 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline312)
    var inline314 string = inline311 + inline313
    t222 = inline314
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline304)
    var t223 string
    var inline295 bool = false
    var inline296 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline297 Result__int32__string = use_try(inline296, inline295)
    var inline298 string = show(inline297)
    var inline299 string = inline298 + " count="
    var inline300 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline296)
    var inline301 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline300)
    var inline302 string = inline299 + inline301
    t223 = inline302
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline292)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__271 *ref_int32_x) int32 {
    var t226 int32 = ref_get__Ref_5int32(self__271)
    return t226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__272 *ref_int32_x, value__273 int32) struct{} {
    ref_set__Ref_5int32(self__272, value__273)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t231 string = _goml_runtime_core_int32_to_string(self__33)
    return t231
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__270 int32) *ref_int32_x {
    var t234 *ref_int32_x = ref__Ref_5int32(value__270)
    return t234
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
