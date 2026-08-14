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
    Loop_loop214:
    for {
        var t215 int
        var inline266 int = ref_get__Ref_3int(i__1)
        t215 = inline266
        var t216 bool = t215 <= 100
        if t216 {
            var t223 int
            var inline264 int = ref_get__Ref_3int(i__1)
            t223 = inline264
            var t224 bool
            var inline261 int = 50
            var inline262 bool = t223 == inline261
            t224 = inline262
            if t224 {
                break Loop_loop214
            } else {
                var t218 int
                var inline259 int = ref_get__Ref_3int(sum__0)
                t218 = inline259
                var t219 int
                var inline257 int = ref_get__Ref_3int(i__1)
                t219 = inline257
                var t220 int = t218 + t219
                ref_set__Ref_3int(sum__0, t220)
                var t221 int
                var inline253 int = ref_get__Ref_3int(i__1)
                t221 = inline253
                var t222 int = t221 + 1
                ref_set__Ref_3int(i__1, t222)
                continue
            }
        } else {
            break Loop_loop214
        }
    }
    var inline300 string = "sum up to break: "
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline300)
    _goml_runtime_core_string_print(inline301)
    var t201 int
    var inline298 int = ref_get__Ref_3int(sum__0)
    t201 = inline298
    var inline295 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t201)
    _goml_runtime_core_string_println(inline295)
    var even_sum__2 *ref_int_x
    var inline292 int = 0
    var inline293 *ref_int_x = ref__Ref_3int(inline292)
    even_sum__2 = inline293
    var j__3 *ref_int_x
    var inline289 int = 1
    var inline290 *ref_int_x = ref__Ref_3int(inline289)
    j__3 = inline290
    Loop_loop204:
    for {
        var t205 int
        var inline278 int = ref_get__Ref_3int(j__3)
        t205 = inline278
        var t206 bool = t205 <= 10
        if t206 {
            var cur__4 int
            var inline276 int = ref_get__Ref_3int(j__3)
            cur__4 = inline276
            var t207 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t207)
            var t209 int = cur__4 / 2
            var t210 int = t209 * 2
            var t211 bool
            var inline272 bool = cur__4 == t210
            t211 = inline272
            if t211 {
                var t212 int
                var inline270 int = ref_get__Ref_3int(even_sum__2)
                t212 = inline270
                var t213 int = t212 + cur__4
                ref_set__Ref_3int(even_sum__2, t213)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop204
        }
    }
    var inline285 string = "even sum: "
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
    _goml_runtime_core_string_print(inline286)
    var t203 int
    var inline283 int = ref_get__Ref_3int(even_sum__2)
    t203 = inline283
    var inline280 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline280)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t227 *ref_int_x = ref__Ref_3int(value__273)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t246 string = _goml_runtime_core_int_to_string(self__67)
    return t246
}

func main() {
    main0()
}
