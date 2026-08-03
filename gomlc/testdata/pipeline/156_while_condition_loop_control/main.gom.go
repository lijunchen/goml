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
    Loop_loop206:
    for {
        var t217 int
        var inline270 int = ref_get__Ref_3int(i__0)
        t217 = inline270
        var t218 bool
        var inline267 int = 0
        var inline268 bool = t217 == inline267
        t218 = inline268
        var jp208 bool
        if t218 {
            var inline244 int = 1
            ref_set__Ref_3int(i__0, inline244)
            jp208 = true
        } else {
            var t221 int
            var inline247 int = ref_get__Ref_3int(i__0)
            t221 = inline247
            var t222 bool = t221 < 4
            if t222 {
                jp208 = true
            } else {
                jp208 = false
            }
        }
        if jp208 {
            var t209 int
            var inline265 int = ref_get__Ref_3int(total__1)
            t209 = inline265
            var t210 int
            var inline263 int = ref_get__Ref_3int(i__0)
            t210 = inline263
            var t211 int = t209 + t210
            ref_set__Ref_3int(total__1, t211)
            var t215 int
            var inline259 int = ref_get__Ref_3int(i__0)
            t215 = inline259
            var t216 bool
            var inline256 int = 1
            var inline257 bool = t215 == inline256
            t216 = inline257
            if t216 {
                var inline249 int = 2
                ref_set__Ref_3int(i__0, inline249)
                continue
            } else {
                var t213 int
                var inline254 int = ref_get__Ref_3int(i__0)
                t213 = inline254
                var t214 int = t213 + 1
                ref_set__Ref_3int(i__0, t214)
                continue
            }
        } else {
            break Loop_loop206
        }
    }
    var t194 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t194)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x
    var inline296 int = 0
    var inline297 *ref_int_x = ref__Ref_3int(inline296)
    total2__3 = inline297
    Loop_loop197:
    for {
        var mtmp184 int
        var inline289 int = ref_get__Ref_3int(j__2)
        mtmp184 = inline289
        var jp199 bool
        switch mtmp184 {
        case 0:
            var inline272 int = 1
            ref_set__Ref_3int(j__2, inline272)
            jp199 = true
        case 1:
            var inline275 int = 2
            ref_set__Ref_3int(j__2, inline275)
            jp199 = true
        case 2:
            jp199 = true
        default:
            jp199 = false
        }
        if jp199 {
            var t200 int
            var inline287 int = ref_get__Ref_3int(total2__3)
            t200 = inline287
            var t201 int
            var inline285 int = ref_get__Ref_3int(j__2)
            t201 = inline285
            var t202 int = t200 + t201
            ref_set__Ref_3int(total2__3, t202)
            var t204 int
            var inline281 int = ref_get__Ref_3int(j__2)
            t204 = inline281
            var t205 bool
            var inline278 int = 2
            var inline279 bool = t204 == inline278
            t205 = inline279
            if t205 {
                break Loop_loop197
            } else {
                continue
            }
        } else {
            break Loop_loop197
        }
    }
    var t196 int
    var inline294 int = ref_get__Ref_3int(total2__3)
    t196 = inline294
    var inline291 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t196)
    _goml_runtime_core_string_println(inline291)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t225 *ref_int_x = ref__Ref_3int(value__236)
    return t225
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t228 int = ref_get__Ref_3int(self__237)
    return t228
}

func println__T_int(value__31 int) struct{} {
    var t235 string
    var inline299 string = _goml_runtime_core_int_to_string(value__31)
    t235 = inline299
    _goml_runtime_core_string_println(t235)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t239 string = _goml_runtime_core_int_to_string(self__69)
    return t239
}

func main() {
    main0()
}
