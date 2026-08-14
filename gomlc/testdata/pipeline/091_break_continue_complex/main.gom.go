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
    Loop_loop209:
    for {
        var t210 int
        var inline261 int = ref_get__Ref_3int(i__1)
        t210 = inline261
        var t211 bool = t210 <= 100
        if t211 {
            var t218 int
            var inline259 int = ref_get__Ref_3int(i__1)
            t218 = inline259
            var t219 bool
            var inline256 int = 50
            var inline257 bool = t218 == inline256
            t219 = inline257
            if t219 {
                break Loop_loop209
            } else {
                var t213 int
                var inline254 int = ref_get__Ref_3int(sum__0)
                t213 = inline254
                var t214 int
                var inline252 int = ref_get__Ref_3int(i__1)
                t214 = inline252
                var t215 int = t213 + t214
                ref_set__Ref_3int(sum__0, t215)
                var t216 int
                var inline248 int = ref_get__Ref_3int(i__1)
                t216 = inline248
                var t217 int = t216 + 1
                ref_set__Ref_3int(i__1, t217)
                continue
            }
        } else {
            break Loop_loop209
        }
    }
    var inline295 string = "sum up to break: "
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline295)
    _goml_runtime_core_string_print(inline296)
    var t196 int
    var inline293 int = ref_get__Ref_3int(sum__0)
    t196 = inline293
    var inline290 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t196)
    _goml_runtime_core_string_println(inline290)
    var even_sum__2 *ref_int_x
    var inline287 int = 0
    var inline288 *ref_int_x = ref__Ref_3int(inline287)
    even_sum__2 = inline288
    var j__3 *ref_int_x
    var inline284 int = 1
    var inline285 *ref_int_x = ref__Ref_3int(inline284)
    j__3 = inline285
    Loop_loop199:
    for {
        var t200 int
        var inline273 int = ref_get__Ref_3int(j__3)
        t200 = inline273
        var t201 bool = t200 <= 10
        if t201 {
            var cur__4 int
            var inline271 int = ref_get__Ref_3int(j__3)
            cur__4 = inline271
            var t202 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t202)
            var t204 int = cur__4 / 2
            var t205 int = t204 * 2
            var t206 bool
            var inline267 bool = cur__4 == t205
            t206 = inline267
            if t206 {
                var t207 int
                var inline265 int = ref_get__Ref_3int(even_sum__2)
                t207 = inline265
                var t208 int = t207 + cur__4
                ref_set__Ref_3int(even_sum__2, t208)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop199
        }
    }
    var inline280 string = "even sum: "
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline280)
    _goml_runtime_core_string_print(inline281)
    var t198 int
    var inline278 int = ref_get__Ref_3int(even_sum__2)
    t198 = inline278
    var inline275 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline275)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t222 *ref_int_x = ref__Ref_3int(value__270)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t241 string = _goml_runtime_core_int_to_string(self__67)
    return t241
}

func main() {
    main0()
}
