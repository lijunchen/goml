package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
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
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop211:
    for {
        var t222 int
        var inline275 int = ref_get__Ref_3int(i__0)
        t222 = inline275
        var t223 bool
        var inline272 int = 0
        var inline273 bool = t222 == inline272
        t223 = inline273
        var jp213 bool
        if t223 {
            var inline249 int = 1
            ref_set__Ref_3int(i__0, inline249)
            jp213 = true
        } else {
            var t226 int
            var inline252 int = ref_get__Ref_3int(i__0)
            t226 = inline252
            var t227 bool = t226 < 4
            if t227 {
                jp213 = true
            } else {
                jp213 = false
            }
        }
        if jp213 {
            var t214 int
            var inline270 int = ref_get__Ref_3int(total__1)
            t214 = inline270
            var t215 int
            var inline268 int = ref_get__Ref_3int(i__0)
            t215 = inline268
            var t216 int = t214 + t215
            ref_set__Ref_3int(total__1, t216)
            var t220 int
            var inline264 int = ref_get__Ref_3int(i__0)
            t220 = inline264
            var t221 bool
            var inline261 int = 1
            var inline262 bool = t220 == inline261
            t221 = inline262
            if t221 {
                var inline254 int = 2
                ref_set__Ref_3int(i__0, inline254)
                continue
            } else {
                var t218 int
                var inline259 int = ref_get__Ref_3int(i__0)
                t218 = inline259
                var t219 int = t218 + 1
                ref_set__Ref_3int(i__0, t219)
                continue
            }
        } else {
            break Loop_loop211
        }
    }
    var t199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t199)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x
    var inline301 int = 0
    var inline302 *ref_int_x = ref__Ref_3int(inline301)
    total2__3 = inline302
    Loop_loop202:
    for {
        var mtmp189 int
        var inline294 int = ref_get__Ref_3int(j__2)
        mtmp189 = inline294
        var jp204 bool
        switch mtmp189 {
        case 0:
            var inline277 int = 1
            ref_set__Ref_3int(j__2, inline277)
            jp204 = true
        case 1:
            var inline280 int = 2
            ref_set__Ref_3int(j__2, inline280)
            jp204 = true
        case 2:
            jp204 = true
        default:
            jp204 = false
        }
        if jp204 {
            var t205 int
            var inline292 int = ref_get__Ref_3int(total2__3)
            t205 = inline292
            var t206 int
            var inline290 int = ref_get__Ref_3int(j__2)
            t206 = inline290
            var t207 int = t205 + t206
            ref_set__Ref_3int(total2__3, t207)
            var t209 int
            var inline286 int = ref_get__Ref_3int(j__2)
            t209 = inline286
            var t210 bool
            var inline283 int = 2
            var inline284 bool = t209 == inline283
            t210 = inline284
            if t210 {
                break Loop_loop202
            } else {
                continue
            }
        } else {
            break Loop_loop202
        }
    }
    var t201 int
    var inline299 int = ref_get__Ref_3int(total2__3)
    t201 = inline299
    var inline296 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t201)
    _goml_runtime_core_string_println(inline296)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t230 *ref_int_x = ref__Ref_3int(value__270)
    return t230
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__271 *ref_int_x) int {
    var t233 int = ref_get__Ref_3int(self__271)
    return t233
}

func println__T_int(value__1 int) struct{} {
    var t240 string
    var inline304 string = _goml_runtime_core_int_to_string(value__1)
    t240 = inline304
    _goml_runtime_core_string_println(t240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t244 string = _goml_runtime_core_int_to_string(self__67)
    return t244
}

func main() {
    main0()
}
