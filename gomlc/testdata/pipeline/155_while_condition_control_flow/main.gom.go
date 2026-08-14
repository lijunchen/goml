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
    Loop_loop234:
    for {
        var t240 int
        var inline272 int = ref_get__Ref_3int(i__0)
        t240 = inline272
        var t241 bool = t240 < 3
        var jp236 bool
        if t241 {
            jp236 = true
        } else {
            jp236 = false
        }
        if jp236 {
            var t237 int
            var inline270 int = ref_get__Ref_3int(i__0)
            t237 = inline270
            var inline267 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t237)
            _goml_runtime_core_string_println(inline267)
            var t238 int
            var inline265 int = ref_get__Ref_3int(i__0)
            t238 = inline265
            var t239 int = t238 + 1
            ref_set__Ref_3int(i__0, t239)
            continue
        } else {
            break Loop_loop234
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop217:
    for {
        var t225 int
        var inline294 int = ref_get__Ref_3int(j__1)
        t225 = inline294
        var t226 bool = t225 < 4
        var jp219 bool
        if t226 {
            var t229 int
            var inline282 int = ref_get__Ref_3int(j__1)
            t229 = inline282
            var t230 bool
            var inline279 int = 1
            var inline280 bool = t229 == inline279
            t230 = inline280
            if t230 {
                jp219 = true
            } else {
                var t231 int
                var inline277 int = ref_get__Ref_3int(j__1)
                t231 = inline277
                var t232 bool
                var inline274 int = 3
                var inline275 bool = t231 == inline274
                t232 = inline275
                var t233 bool = !t232
                jp219 = t233
            }
        } else {
            jp219 = false
        }
        if jp219 {
            var t220 int
            var inline292 int = ref_get__Ref_3int(total__2)
            t220 = inline292
            var t221 int
            var inline290 int = ref_get__Ref_3int(j__1)
            t221 = inline290
            var t222 int = t220 + t221
            ref_set__Ref_3int(total__2, t222)
            var t223 int
            var inline286 int = ref_get__Ref_3int(j__1)
            t223 = inline286
            var t224 int = t223 + 1
            ref_set__Ref_3int(j__1, t224)
            continue
        } else {
            break Loop_loop217
        }
    }
    var t202 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t202)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop205:
    for {
        var mtmp194 int
        var inline311 int = ref_get__Ref_3int(k__3)
        mtmp194 = inline311
        var jp207 bool
        switch mtmp194 {
        case 0:
            jp207 = true
        case 1:
            var t215 int
            var inline299 int = ref_get__Ref_3int(sum__4)
            t215 = inline299
            var t216 bool
            var inline296 int = 0
            var inline297 bool = t215 == inline296
            t216 = inline297
            if t216 {
                jp207 = true
            } else {
                jp207 = false
            }
        case 2:
            jp207 = true
        default:
            jp207 = false
        }
        if jp207 {
            var t208 int
            var inline309 int = ref_get__Ref_3int(sum__4)
            t208 = inline309
            var t209 int
            var inline307 int = ref_get__Ref_3int(k__3)
            t209 = inline307
            var t210 int = t208 + t209
            ref_set__Ref_3int(sum__4, t210)
            var t211 int
            var inline303 int = ref_get__Ref_3int(k__3)
            t211 = inline303
            var t212 int = t211 + 1
            ref_set__Ref_3int(k__3, t212)
            continue
        } else {
            break Loop_loop205
        }
    }
    var t204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t244 *ref_int_x = ref__Ref_3int(value__273)
    return t244
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__274 *ref_int_x) int {
    var t247 int = ref_get__Ref_3int(self__274)
    return t247
}

func println__T_int(value__1 int) struct{} {
    var t249 string
    var inline313 string = _goml_runtime_core_int_to_string(value__1)
    t249 = inline313
    _goml_runtime_core_string_println(t249)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t258 string = _goml_runtime_core_int_to_string(self__67)
    return t258
}

func main() {
    main0()
}
