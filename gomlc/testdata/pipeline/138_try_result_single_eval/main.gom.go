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
    var mtmp156 Result__int32__string
    var inline228 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
    var inline229 int32 = inline228 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(counter__2, inline229)
    if ok__3 {
        var inline231 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(counter__2)
        var inline232 Result__int32__string = Ok{
            _0: inline231,
        }
        mtmp156 = inline232
    } else {
        var inline233 Result__int32__string = Err{
            _0: "bump failed",
        }
        mtmp156 = inline233
    }
    var jp175 int32
    switch mtmp156.(type) {
    case Ok:
        var x157 int32 = mtmp156.(Ok)._0
        jp175 = x157
        var t176 int32
        var inline226 int32 = ref_get__Ref_5int32(counter__2)
        t176 = inline226
        var t177 int32 = jp175 + t176
        var t178 Result__int32__string = Ok{
            _0: t177,
        }
        return t178
    case Err:
        var x158 string = mtmp156.(Err)._0
        var t179 Result__int32__string = Err{
            _0: x158,
        }
        return t179
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__int32__string) string {
    switch res__5.(type) {
    case Ok:
        var x159 int32 = res__5.(Ok)._0
        var t184 string
        var inline235 string = _goml_runtime_core_int32_to_string(x159)
        t184 = inline235
        var t185 string = "ok " + t184
        return t185
    case Err:
        var x160 string = res__5.(Err)._0
        var t186 string = "err " + x160
        return t186
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t195 string
    var inline280 bool = true
    var inline281 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline282 Result__int32__string = use_try(inline281, inline280)
    var inline283 string = show(inline282)
    var inline284 string = inline283 + " count="
    var inline285 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline281)
    var inline286 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline285)
    var inline287 string = inline284 + inline286
    t195 = inline287
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline277)
    var t196 string
    var inline268 bool = false
    var inline269 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var inline270 Result__int32__string = use_try(inline269, inline268)
    var inline271 string = show(inline270)
    var inline272 string = inline271 + " count="
    var inline273 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline269)
    var inline274 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline273)
    var inline275 string = inline272 + inline274
    t196 = inline275
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline265)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t199 int32 = ref_get__Ref_5int32(self__208)
    return t199
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__6)
    return t204
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t207 *ref_int32_x = ref__Ref_5int32(value__207)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
