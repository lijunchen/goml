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
    Loop_loop219:
    for {
        var t225 int
        var inline257 int = ref_get__Ref_3int(i__0)
        t225 = inline257
        var t226 bool = t225 < 3
        var jp221 bool
        if t226 {
            jp221 = true
        } else {
            jp221 = false
        }
        if jp221 {
            var t222 int
            var inline255 int = ref_get__Ref_3int(i__0)
            t222 = inline255
            var inline252 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t222)
            _goml_runtime_core_string_println(inline252)
            var t223 int
            var inline250 int = ref_get__Ref_3int(i__0)
            t223 = inline250
            var t224 int = t223 + 1
            ref_set__Ref_3int(i__0, t224)
            continue
        } else {
            break Loop_loop219
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop202:
    for {
        var t210 int
        var inline279 int = ref_get__Ref_3int(j__1)
        t210 = inline279
        var t211 bool = t210 < 4
        var jp204 bool
        if t211 {
            var t214 int
            var inline267 int = ref_get__Ref_3int(j__1)
            t214 = inline267
            var t215 bool
            var inline264 int = 1
            var inline265 bool = t214 == inline264
            t215 = inline265
            if t215 {
                jp204 = true
            } else {
                var t216 int
                var inline262 int = ref_get__Ref_3int(j__1)
                t216 = inline262
                var t217 bool
                var inline259 int = 3
                var inline260 bool = t216 == inline259
                t217 = inline260
                var t218 bool = !t217
                jp204 = t218
            }
        } else {
            jp204 = false
        }
        if jp204 {
            var t205 int
            var inline277 int = ref_get__Ref_3int(total__2)
            t205 = inline277
            var t206 int
            var inline275 int = ref_get__Ref_3int(j__1)
            t206 = inline275
            var t207 int = t205 + t206
            ref_set__Ref_3int(total__2, t207)
            var t208 int
            var inline271 int = ref_get__Ref_3int(j__1)
            t208 = inline271
            var t209 int = t208 + 1
            ref_set__Ref_3int(j__1, t209)
            continue
        } else {
            break Loop_loop202
        }
    }
    var t187 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t187)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop190:
    for {
        var mtmp179 int
        var inline296 int = ref_get__Ref_3int(k__3)
        mtmp179 = inline296
        var jp192 bool
        switch mtmp179 {
        case 0:
            jp192 = true
        case 1:
            var t200 int
            var inline284 int = ref_get__Ref_3int(sum__4)
            t200 = inline284
            var t201 bool
            var inline281 int = 0
            var inline282 bool = t200 == inline281
            t201 = inline282
            if t201 {
                jp192 = true
            } else {
                jp192 = false
            }
        case 2:
            jp192 = true
        default:
            jp192 = false
        }
        if jp192 {
            var t193 int
            var inline294 int = ref_get__Ref_3int(sum__4)
            t193 = inline294
            var t194 int
            var inline292 int = ref_get__Ref_3int(k__3)
            t194 = inline292
            var t195 int = t193 + t194
            ref_set__Ref_3int(sum__4, t195)
            var t196 int
            var inline288 int = ref_get__Ref_3int(k__3)
            t196 = inline288
            var t197 int = t196 + 1
            ref_set__Ref_3int(k__3, t197)
            continue
        } else {
            break Loop_loop190
        }
    }
    var t189 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t189)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t229 *ref_int_x = ref__Ref_3int(value__257)
    return t229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t232 int = ref_get__Ref_3int(self__258)
    return t232
}

func println__T_int(value__31 int) struct{} {
    var t234 string
    var inline298 string = _goml_runtime_core_int_to_string(value__31)
    t234 = inline298
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t243 string = _goml_runtime_core_int_to_string(self__69)
    return t243
}

func main() {
    main0()
}
