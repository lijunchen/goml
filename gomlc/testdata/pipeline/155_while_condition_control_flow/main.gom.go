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
    Loop_loop229:
    for {
        var t235 int
        var inline267 int = ref_get__Ref_3int(i__0)
        t235 = inline267
        var t236 bool = t235 < 3
        var jp231 bool
        if t236 {
            jp231 = true
        } else {
            jp231 = false
        }
        if jp231 {
            var t232 int
            var inline265 int = ref_get__Ref_3int(i__0)
            t232 = inline265
            var inline262 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t232)
            _goml_runtime_core_string_println(inline262)
            var t233 int
            var inline260 int = ref_get__Ref_3int(i__0)
            t233 = inline260
            var t234 int = t233 + 1
            ref_set__Ref_3int(i__0, t234)
            continue
        } else {
            break Loop_loop229
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop212:
    for {
        var t220 int
        var inline289 int = ref_get__Ref_3int(j__1)
        t220 = inline289
        var t221 bool = t220 < 4
        var jp214 bool
        if t221 {
            var t224 int
            var inline277 int = ref_get__Ref_3int(j__1)
            t224 = inline277
            var t225 bool
            var inline274 int = 1
            var inline275 bool = t224 == inline274
            t225 = inline275
            if t225 {
                jp214 = true
            } else {
                var t226 int
                var inline272 int = ref_get__Ref_3int(j__1)
                t226 = inline272
                var t227 bool
                var inline269 int = 3
                var inline270 bool = t226 == inline269
                t227 = inline270
                var t228 bool = !t227
                jp214 = t228
            }
        } else {
            jp214 = false
        }
        if jp214 {
            var t215 int
            var inline287 int = ref_get__Ref_3int(total__2)
            t215 = inline287
            var t216 int
            var inline285 int = ref_get__Ref_3int(j__1)
            t216 = inline285
            var t217 int = t215 + t216
            ref_set__Ref_3int(total__2, t217)
            var t218 int
            var inline281 int = ref_get__Ref_3int(j__1)
            t218 = inline281
            var t219 int = t218 + 1
            ref_set__Ref_3int(j__1, t219)
            continue
        } else {
            break Loop_loop212
        }
    }
    var t197 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t197)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop200:
    for {
        var mtmp189 int
        var inline306 int = ref_get__Ref_3int(k__3)
        mtmp189 = inline306
        var jp202 bool
        switch mtmp189 {
        case 0:
            jp202 = true
        case 1:
            var t210 int
            var inline294 int = ref_get__Ref_3int(sum__4)
            t210 = inline294
            var t211 bool
            var inline291 int = 0
            var inline292 bool = t210 == inline291
            t211 = inline292
            if t211 {
                jp202 = true
            } else {
                jp202 = false
            }
        case 2:
            jp202 = true
        default:
            jp202 = false
        }
        if jp202 {
            var t203 int
            var inline304 int = ref_get__Ref_3int(sum__4)
            t203 = inline304
            var t204 int
            var inline302 int = ref_get__Ref_3int(k__3)
            t204 = inline302
            var t205 int = t203 + t204
            ref_set__Ref_3int(sum__4, t205)
            var t206 int
            var inline298 int = ref_get__Ref_3int(k__3)
            t206 = inline298
            var t207 int = t206 + 1
            ref_set__Ref_3int(k__3, t207)
            continue
        } else {
            break Loop_loop200
        }
    }
    var t199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t199)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t239 *ref_int_x = ref__Ref_3int(value__270)
    return t239
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__271 *ref_int_x) int {
    var t242 int = ref_get__Ref_3int(self__271)
    return t242
}

func println__T_int(value__1 int) struct{} {
    var t244 string
    var inline308 string = _goml_runtime_core_int_to_string(value__1)
    t244 = inline308
    _goml_runtime_core_string_println(t244)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t253 string = _goml_runtime_core_int_to_string(self__67)
    return t253
}

func main() {
    main0()
}
