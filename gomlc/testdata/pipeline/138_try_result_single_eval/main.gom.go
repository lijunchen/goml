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
    var mtmp178 Result__int32__string
    var inline250 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline251 int32 = inline250 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline251)
    if ok__3 {
        var inline253 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline254 Result__int32__string = Ok{
            _0: inline253,
        }
        mtmp178 = inline254
    } else {
        var inline255 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp178 = inline255
    }
    var jp197 int32
    switch mtmp178.(type) {
    case Ok:
        var x179 int32 = mtmp178.(Ok)._0
        jp197 = x179
        var t198 int32
        var inline248 int32 = ref_get__Ref_5int32(counter__2)
        t198 = inline248
        var t199 int32 = jp197 + t198
        var t200 Result__int32__string = Ok{
            _0: t199,
        }
        return t200
    case Err:
        var x180 string = mtmp178.(Err)._0
        var t201 Result__int32__string = Err{
            _0: x180,
        }
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x181 int32 = res__5.(Ok)._0
        var t206 string
        var inline257 string = _goml_runtime_core_int32_to_string(x181)
        t206 = inline257
        var t207 string = "ok " + t206
        return t207
    case Err:
        var x182 string = res__5.(Err)._0
        var t208 string = "err " + x182
        return t208
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t217 string
    var inline302 bool = true
    var inline303 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline304 Result__int32__string = use_try(inline303, inline302)
    var inline305 string = show(inline304)
    var inline306 string = inline305 + " count="
    var inline307 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline303)
    var inline308 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline307)
    var inline309 string = inline306 + inline308
    t217 = inline309
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline299)
    var t218 string
    var inline290 bool = false
    var inline291 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline292 Result__int32__string = use_try(inline291, inline290)
    var inline293 string = show(inline292)
    var inline294 string = inline293 + " count="
    var inline295 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline291)
    var inline296 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline295)
    var inline297 string = inline294 + inline296
    t218 = inline297
    var inline287 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline287)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__237 *ref_int32_x) int32 {
    var t221 int32 = ref_get__Ref_5int32(self__237)
    return t221
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__238 *ref_int32_x, value__239 int32) struct{} {
    ref_set__Ref_5int32(self__238, value__239)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__35)
    return t226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__236 int32) *ref_int32_x {
    var t229 *ref_int32_x = ref__Ref_5int32(value__236)
    return t229
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
