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
    Loop_loop216:
    for {
        var t227 int
        var inline280 int = ref_get__Ref_3int(i__0)
        t227 = inline280
        var t228 bool
        var inline277 int = 0
        var inline278 bool = t227 == inline277
        t228 = inline278
        var jp218 bool
        if t228 {
            var inline254 int = 1
            ref_set__Ref_3int(i__0, inline254)
            jp218 = true
        } else {
            var t231 int
            var inline257 int = ref_get__Ref_3int(i__0)
            t231 = inline257
            var t232 bool = t231 < 4
            if t232 {
                jp218 = true
            } else {
                jp218 = false
            }
        }
        if jp218 {
            var t219 int
            var inline275 int = ref_get__Ref_3int(total__1)
            t219 = inline275
            var t220 int
            var inline273 int = ref_get__Ref_3int(i__0)
            t220 = inline273
            var t221 int = t219 + t220
            ref_set__Ref_3int(total__1, t221)
            var t225 int
            var inline269 int = ref_get__Ref_3int(i__0)
            t225 = inline269
            var t226 bool
            var inline266 int = 1
            var inline267 bool = t225 == inline266
            t226 = inline267
            if t226 {
                var inline259 int = 2
                ref_set__Ref_3int(i__0, inline259)
                continue
            } else {
                var t223 int
                var inline264 int = ref_get__Ref_3int(i__0)
                t223 = inline264
                var t224 int = t223 + 1
                ref_set__Ref_3int(i__0, t224)
                continue
            }
        } else {
            break Loop_loop216
        }
    }
    var t204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t204)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x
    var inline306 int = 0
    var inline307 *ref_int_x = ref__Ref_3int(inline306)
    total2__3 = inline307
    Loop_loop207:
    for {
        var mtmp194 int
        var inline299 int = ref_get__Ref_3int(j__2)
        mtmp194 = inline299
        var jp209 bool
        switch mtmp194 {
        case 0:
            var inline282 int = 1
            ref_set__Ref_3int(j__2, inline282)
            jp209 = true
        case 1:
            var inline285 int = 2
            ref_set__Ref_3int(j__2, inline285)
            jp209 = true
        case 2:
            jp209 = true
        default:
            jp209 = false
        }
        if jp209 {
            var t210 int
            var inline297 int = ref_get__Ref_3int(total2__3)
            t210 = inline297
            var t211 int
            var inline295 int = ref_get__Ref_3int(j__2)
            t211 = inline295
            var t212 int = t210 + t211
            ref_set__Ref_3int(total2__3, t212)
            var t214 int
            var inline291 int = ref_get__Ref_3int(j__2)
            t214 = inline291
            var t215 bool
            var inline288 int = 2
            var inline289 bool = t214 == inline288
            t215 = inline289
            if t215 {
                break Loop_loop207
            } else {
                continue
            }
        } else {
            break Loop_loop207
        }
    }
    var t206 int
    var inline304 int = ref_get__Ref_3int(total2__3)
    t206 = inline304
    var inline301 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t206)
    _goml_runtime_core_string_println(inline301)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t235 *ref_int_x = ref__Ref_3int(value__273)
    return t235
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__274 *ref_int_x) int {
    var t238 int = ref_get__Ref_3int(self__274)
    return t238
}

func println__T_int(value__1 int) struct{} {
    var t245 string
    var inline309 string = _goml_runtime_core_int_to_string(value__1)
    t245 = inline309
    _goml_runtime_core_string_println(t245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t249 string = _goml_runtime_core_int_to_string(self__67)
    return t249
}

func main() {
    main0()
}
