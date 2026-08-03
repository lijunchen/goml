package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var sum__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop204:
    for {
        var t205 int
        var inline256 int = ref_get__Ref_3int(i__1)
        t205 = inline256
        var t206 bool = t205 <= 100
        if t206 {
            var t213 int
            var inline254 int = ref_get__Ref_3int(i__1)
            t213 = inline254
            var t214 bool
            var inline251 int = 50
            var inline252 bool = t213 == inline251
            t214 = inline252
            if t214 {
                break Loop_loop204
            } else {
                var t208 int
                var inline249 int = ref_get__Ref_3int(sum__0)
                t208 = inline249
                var t209 int
                var inline247 int = ref_get__Ref_3int(i__1)
                t209 = inline247
                var t210 int = t208 + t209
                ref_set__Ref_3int(sum__0, t210)
                var t211 int
                var inline243 int = ref_get__Ref_3int(i__1)
                t211 = inline243
                var t212 int = t211 + 1
                ref_set__Ref_3int(i__1, t212)
                continue
            }
        } else {
            break Loop_loop204
        }
    }
    var inline290 string = "sum up to break: "
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline290)
    _goml_runtime_core_string_print(inline291)
    var t191 int
    var inline288 int = ref_get__Ref_3int(sum__0)
    t191 = inline288
    var inline285 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t191)
    _goml_runtime_core_string_println(inline285)
    var even_sum__2 *ref_int_x
    var inline282 int = 0
    var inline283 *ref_int_x = ref__Ref_3int(inline282)
    even_sum__2 = inline283
    var j__3 *ref_int_x
    var inline279 int = 1
    var inline280 *ref_int_x = ref__Ref_3int(inline279)
    j__3 = inline280
    Loop_loop194:
    for {
        var t195 int
        var inline268 int = ref_get__Ref_3int(j__3)
        t195 = inline268
        var t196 bool = t195 <= 10
        if t196 {
            var cur__4 int
            var inline266 int = ref_get__Ref_3int(j__3)
            cur__4 = inline266
            var t197 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t197)
            var t199 int = cur__4 / 2
            var t200 int = t199 * 2
            var t201 bool
            var inline262 bool = cur__4 == t200
            t201 = inline262
            if t201 {
                var t202 int
                var inline260 int = ref_get__Ref_3int(even_sum__2)
                t202 = inline260
                var t203 int = t202 + cur__4
                ref_set__Ref_3int(even_sum__2, t203)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop194
        }
    }
    var inline275 string = "even sum: "
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline275)
    _goml_runtime_core_string_print(inline276)
    var t193 int
    var inline273 int = ref_get__Ref_3int(even_sum__2)
    t193 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline270)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t217 *ref_int_x = ref__Ref_3int(value__236)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t236 string = _goml_runtime_core_int_to_string(self__69)
    return t236
}

func main() {
    main0()
}
