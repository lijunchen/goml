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
    Loop_loop201:
    for {
        var t212 int
        var inline265 int = ref_get__Ref_3int(i__0)
        t212 = inline265
        var t213 bool
        var inline262 int = 0
        var inline263 bool = t212 == inline262
        t213 = inline263
        var jp203 bool
        if t213 {
            var inline239 int = 1
            ref_set__Ref_3int(i__0, inline239)
            jp203 = true
        } else {
            var t216 int
            var inline242 int = ref_get__Ref_3int(i__0)
            t216 = inline242
            var t217 bool = t216 < 4
            if t217 {
                jp203 = true
            } else {
                jp203 = false
            }
        }
        if jp203 {
            var t204 int
            var inline260 int = ref_get__Ref_3int(total__1)
            t204 = inline260
            var t205 int
            var inline258 int = ref_get__Ref_3int(i__0)
            t205 = inline258
            var t206 int = t204 + t205
            ref_set__Ref_3int(total__1, t206)
            var t210 int
            var inline254 int = ref_get__Ref_3int(i__0)
            t210 = inline254
            var t211 bool
            var inline251 int = 1
            var inline252 bool = t210 == inline251
            t211 = inline252
            if t211 {
                var inline244 int = 2
                ref_set__Ref_3int(i__0, inline244)
                continue
            } else {
                var t208 int
                var inline249 int = ref_get__Ref_3int(i__0)
                t208 = inline249
                var t209 int = t208 + 1
                ref_set__Ref_3int(i__0, t209)
                continue
            }
        } else {
            break Loop_loop201
        }
    }
    var t189 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__1)
    println__T_int(t189)
    var j__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total2__3 *ref_int_x
    var inline291 int = 0
    var inline292 *ref_int_x = ref__Ref_3int(inline291)
    total2__3 = inline292
    Loop_loop192:
    for {
        var mtmp179 int
        var inline284 int = ref_get__Ref_3int(j__2)
        mtmp179 = inline284
        var jp194 bool
        switch mtmp179 {
        case 0:
            var inline267 int = 1
            ref_set__Ref_3int(j__2, inline267)
            jp194 = true
        case 1:
            var inline270 int = 2
            ref_set__Ref_3int(j__2, inline270)
            jp194 = true
        case 2:
            jp194 = true
        default:
            jp194 = false
        }
        if jp194 {
            var t195 int
            var inline282 int = ref_get__Ref_3int(total2__3)
            t195 = inline282
            var t196 int
            var inline280 int = ref_get__Ref_3int(j__2)
            t196 = inline280
            var t197 int = t195 + t196
            ref_set__Ref_3int(total2__3, t197)
            var t199 int
            var inline276 int = ref_get__Ref_3int(j__2)
            t199 = inline276
            var t200 bool
            var inline273 int = 2
            var inline274 bool = t199 == inline273
            t200 = inline274
            if t200 {
                break Loop_loop192
            } else {
                continue
            }
        } else {
            break Loop_loop192
        }
    }
    var t191 int
    var inline289 int = ref_get__Ref_3int(total2__3)
    t191 = inline289
    var inline286 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t191)
    _goml_runtime_core_string_println(inline286)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__255 int) *ref_int_x {
    var t220 *ref_int_x = ref__Ref_3int(value__255)
    return t220
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__256 *ref_int_x) int {
    var t223 int = ref_get__Ref_3int(self__256)
    return t223
}

func println__T_int(value__1 int) struct{} {
    var t230 string
    var inline294 string = _goml_runtime_core_int_to_string(value__1)
    t230 = inline294
    _goml_runtime_core_string_println(t230)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t234 string = _goml_runtime_core_int_to_string(self__67)
    return t234
}

func main() {
    main0()
}
