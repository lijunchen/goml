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
    Loop_loop224:
    for {
        var t230 int
        var inline262 int = ref_get__Ref_3int(i__0)
        t230 = inline262
        var t231 bool = t230 < 3
        var jp226 bool
        if t231 {
            jp226 = true
        } else {
            jp226 = false
        }
        if jp226 {
            var t227 int
            var inline260 int = ref_get__Ref_3int(i__0)
            t227 = inline260
            var inline257 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t227)
            _goml_runtime_core_string_println(inline257)
            var t228 int
            var inline255 int = ref_get__Ref_3int(i__0)
            t228 = inline255
            var t229 int = t228 + 1
            ref_set__Ref_3int(i__0, t229)
            continue
        } else {
            break Loop_loop224
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop207:
    for {
        var t215 int
        var inline284 int = ref_get__Ref_3int(j__1)
        t215 = inline284
        var t216 bool = t215 < 4
        var jp209 bool
        if t216 {
            var t219 int
            var inline272 int = ref_get__Ref_3int(j__1)
            t219 = inline272
            var t220 bool
            var inline269 int = 1
            var inline270 bool = t219 == inline269
            t220 = inline270
            if t220 {
                jp209 = true
            } else {
                var t221 int
                var inline267 int = ref_get__Ref_3int(j__1)
                t221 = inline267
                var t222 bool
                var inline264 int = 3
                var inline265 bool = t221 == inline264
                t222 = inline265
                var t223 bool = !t222
                jp209 = t223
            }
        } else {
            jp209 = false
        }
        if jp209 {
            var t210 int
            var inline282 int = ref_get__Ref_3int(total__2)
            t210 = inline282
            var t211 int
            var inline280 int = ref_get__Ref_3int(j__1)
            t211 = inline280
            var t212 int = t210 + t211
            ref_set__Ref_3int(total__2, t212)
            var t213 int
            var inline276 int = ref_get__Ref_3int(j__1)
            t213 = inline276
            var t214 int = t213 + 1
            ref_set__Ref_3int(j__1, t214)
            continue
        } else {
            break Loop_loop207
        }
    }
    var t192 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t192)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop195:
    for {
        var mtmp184 int
        var inline301 int = ref_get__Ref_3int(k__3)
        mtmp184 = inline301
        var jp197 bool
        switch mtmp184 {
        case 0:
            jp197 = true
        case 1:
            var t205 int
            var inline289 int = ref_get__Ref_3int(sum__4)
            t205 = inline289
            var t206 bool
            var inline286 int = 0
            var inline287 bool = t205 == inline286
            t206 = inline287
            if t206 {
                jp197 = true
            } else {
                jp197 = false
            }
        case 2:
            jp197 = true
        default:
            jp197 = false
        }
        if jp197 {
            var t198 int
            var inline299 int = ref_get__Ref_3int(sum__4)
            t198 = inline299
            var t199 int
            var inline297 int = ref_get__Ref_3int(k__3)
            t199 = inline297
            var t200 int = t198 + t199
            ref_set__Ref_3int(sum__4, t200)
            var t201 int
            var inline293 int = ref_get__Ref_3int(k__3)
            t201 = inline293
            var t202 int = t201 + 1
            ref_set__Ref_3int(k__3, t202)
            continue
        } else {
            break Loop_loop195
        }
    }
    var t194 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t194)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t234 *ref_int_x = ref__Ref_3int(value__236)
    return t234
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t237 int = ref_get__Ref_3int(self__237)
    return t237
}

func println__T_int(value__31 int) struct{} {
    var t239 string
    var inline303 string = _goml_runtime_core_int_to_string(value__31)
    t239 = inline303
    _goml_runtime_core_string_println(t239)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t248 string = _goml_runtime_core_int_to_string(self__69)
    return t248
}

func main() {
    main0()
}
