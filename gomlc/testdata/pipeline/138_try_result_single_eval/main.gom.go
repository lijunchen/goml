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
    var mtmp173 Result__int32__string
    var inline245 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline246 int32 = inline245 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline246)
    if ok__3 {
        var inline248 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline249 Result__int32__string = Ok{
            _0: inline248,
        }
        mtmp173 = inline249
    } else {
        var inline250 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp173 = inline250
    }
    var jp192 int32
    switch mtmp173.(type) {
    case Ok:
        var x174 int32 = mtmp173.(Ok)._0
        jp192 = x174
        var t193 int32
        var inline243 int32 = ref_get__Ref_5int32(counter__2)
        t193 = inline243
        var t194 int32 = jp192 + t193
        var t195 Result__int32__string = Ok{
            _0: t194,
        }
        return t195
    case Err:
        var x175 string = mtmp173.(Err)._0
        var t196 Result__int32__string = Err{
            _0: x175,
        }
        return t196
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x176 int32 = res__5.(Ok)._0
        var t201 string
        var inline252 string = _goml_runtime_core_int32_to_string(x176)
        t201 = inline252
        var t202 string = "ok " + t201
        return t202
    case Err:
        var x177 string = res__5.(Err)._0
        var t203 string = "err " + x177
        return t203
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t212 string
    var inline297 bool = true
    var inline298 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline299 Result__int32__string = use_try(inline298, inline297)
    var inline300 string = show(inline299)
    var inline301 string = inline300 + " count="
    var inline302 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline298)
    var inline303 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline302)
    var inline304 string = inline301 + inline303
    t212 = inline304
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline294)
    var t213 string
    var inline285 bool = false
    var inline286 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline287 Result__int32__string = use_try(inline286, inline285)
    var inline288 string = show(inline287)
    var inline289 string = inline288 + " count="
    var inline290 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline286)
    var inline291 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline290)
    var inline292 string = inline289 + inline291
    t213 = inline292
    var inline282 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline282)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__256 *ref_int32_x) int32 {
    var t216 int32 = ref_get__Ref_5int32(self__256)
    return t216
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__257 *ref_int32_x, value__258 int32) struct{} {
    ref_set__Ref_5int32(self__257, value__258)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__33)
    return t221
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__255 int32) *ref_int32_x {
    var t224 *ref_int32_x = ref__Ref_5int32(value__255)
    return t224
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
