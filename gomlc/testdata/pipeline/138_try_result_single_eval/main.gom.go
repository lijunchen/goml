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
    var mtmp188 Result__int32__string
    var inline260 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline261 int32 = inline260 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline261)
    if ok__3 {
        var inline263 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline264 Result__int32__string = Ok{
            _0: inline263,
        }
        mtmp188 = inline264
    } else {
        var inline265 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp188 = inline265
    }
    var jp207 int32
    switch mtmp188.(type) {
    case Ok:
        var x189 int32 = mtmp188.(Ok)._0
        jp207 = x189
        var t208 int32
        var inline258 int32 = ref_get__Ref_5int32(counter__2)
        t208 = inline258
        var t209 int32 = jp207 + t208
        var t210 Result__int32__string = Ok{
            _0: t209,
        }
        return t210
    case Err:
        var x190 string = mtmp188.(Err)._0
        var t211 Result__int32__string = Err{
            _0: x190,
        }
        return t211
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x191 int32 = res__5.(Ok)._0
        var t216 string
        var inline267 string = _goml_runtime_core_int32_to_string(x191)
        t216 = inline267
        var t217 string = "ok " + t216
        return t217
    case Err:
        var x192 string = res__5.(Err)._0
        var t218 string = "err " + x192
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t227 string
    var inline312 bool = true
    var inline313 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline314 Result__int32__string = use_try(inline313, inline312)
    var inline315 string = show(inline314)
    var inline316 string = inline315 + " count="
    var inline317 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline313)
    var inline318 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline317)
    var inline319 string = inline316 + inline318
    t227 = inline319
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline309)
    var t228 string
    var inline300 bool = false
    var inline301 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline302 Result__int32__string = use_try(inline301, inline300)
    var inline303 string = show(inline302)
    var inline304 string = inline303 + " count="
    var inline305 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline301)
    var inline306 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline305)
    var inline307 string = inline304 + inline306
    t228 = inline307
    var inline297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline297)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__274 *ref_int32_x) int32 {
    var t231 int32 = ref_get__Ref_5int32(self__274)
    return t231
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__275 *ref_int32_x, value__276 int32) struct{} {
    ref_set__Ref_5int32(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t236 string = _goml_runtime_core_int32_to_string(self__33)
    return t236
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__273 int32) *ref_int32_x {
    var t239 *ref_int32_x = ref__Ref_5int32(value__273)
    return t239
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
