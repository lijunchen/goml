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
    var i__0 *ref_int_x
    var inline280 int = 0
    var inline281 *ref_int_x = ref__Ref_3int(inline280)
    i__0 = inline281
    var sum__1 *ref_int_x
    var inline277 int = 0
    var inline278 *ref_int_x = ref__Ref_3int(inline277)
    sum__1 = inline278
    Loop_loop199:
    for {
        var t200 int
        var inline247 int = ref_get__Ref_3int(i__0)
        t200 = inline247
        var t201 bool = t200 < 5
        if t201 {
            var t202 int
            var inline245 int = ref_get__Ref_3int(i__0)
            t202 = inline245
            var t203 int = t202 + 1
            ref_set__Ref_3int(i__0, t203)
            var t208 int
            var inline241 int = ref_get__Ref_3int(i__0)
            t208 = inline241
            var t209 bool
            var inline238 int = 3
            var inline239 bool = t208 == inline238
            t209 = inline239
            var jp205 int
            if t209 {
                continue
            } else {
                var inline232 int = ref_get__Ref_3int(i__0)
                jp205 = inline232
                var t206 int
                var inline236 int = ref_get__Ref_3int(sum__1)
                t206 = inline236
                var t207 int = t206 + jp205
                ref_set__Ref_3int(sum__1, t207)
                continue
            }
        } else {
            break Loop_loop199
        }
    }
    var t188 int
    var inline275 int = ref_get__Ref_3int(sum__1)
    t188 = inline275
    var inline272 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
    _goml_runtime_core_string_println(inline272)
    var j__3 *ref_int_x
    var inline269 int = 0
    var inline270 *ref_int_x = ref__Ref_3int(inline269)
    j__3 = inline270
    var total__4 *ref_int_x
    var inline266 int = 0
    var inline267 *ref_int_x = ref__Ref_3int(inline266)
    total__4 = inline267
    Loop_loop191:
    for {
        var t192 int
        var inline259 int = ref_get__Ref_3int(j__3)
        t192 = inline259
        var t193 int = t192 + 1
        ref_set__Ref_3int(j__3, t193)
        var mtmp182 int
        var inline255 int = ref_get__Ref_3int(j__3)
        mtmp182 = inline255
        var jp195 int
        switch mtmp182 {
        case 5:
            break Loop_loop191
        default:
            var inline249 int = ref_get__Ref_3int(j__3)
            jp195 = inline249
            var t196 int
            var inline253 int = ref_get__Ref_3int(total__4)
            t196 = inline253
            var t197 int = t196 + jp195
            ref_set__Ref_3int(total__4, t197)
            continue
        }
    }
    var t190 int
    var inline264 int = ref_get__Ref_3int(total__4)
    t190 = inline264
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t190)
    _goml_runtime_core_string_println(inline261)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t227 string = _goml_runtime_core_int_to_string(self__69)
    return t227
}

func main() {
    main0()
}
