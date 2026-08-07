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
    var inline275 int = 0
    var inline276 *ref_int_x = ref__Ref_3int(inline275)
    i__0 = inline276
    var sum__1 *ref_int_x
    var inline272 int = 0
    var inline273 *ref_int_x = ref__Ref_3int(inline272)
    sum__1 = inline273
    Loop_loop194:
    for {
        var t195 int
        var inline242 int = ref_get__Ref_3int(i__0)
        t195 = inline242
        var t196 bool = t195 < 5
        if t196 {
            var t197 int
            var inline240 int = ref_get__Ref_3int(i__0)
            t197 = inline240
            var t198 int = t197 + 1
            ref_set__Ref_3int(i__0, t198)
            var t203 int
            var inline236 int = ref_get__Ref_3int(i__0)
            t203 = inline236
            var t204 bool
            var inline233 int = 3
            var inline234 bool = t203 == inline233
            t204 = inline234
            var jp200 int
            if t204 {
                continue
            } else {
                var inline227 int = ref_get__Ref_3int(i__0)
                jp200 = inline227
                var t201 int
                var inline231 int = ref_get__Ref_3int(sum__1)
                t201 = inline231
                var t202 int = t201 + jp200
                ref_set__Ref_3int(sum__1, t202)
                continue
            }
        } else {
            break Loop_loop194
        }
    }
    var t183 int
    var inline270 int = ref_get__Ref_3int(sum__1)
    t183 = inline270
    var inline267 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t183)
    _goml_runtime_core_string_println(inline267)
    var j__3 *ref_int_x
    var inline264 int = 0
    var inline265 *ref_int_x = ref__Ref_3int(inline264)
    j__3 = inline265
    var total__4 *ref_int_x
    var inline261 int = 0
    var inline262 *ref_int_x = ref__Ref_3int(inline261)
    total__4 = inline262
    Loop_loop186:
    for {
        var t187 int
        var inline254 int = ref_get__Ref_3int(j__3)
        t187 = inline254
        var t188 int = t187 + 1
        ref_set__Ref_3int(j__3, t188)
        var mtmp177 int
        var inline250 int = ref_get__Ref_3int(j__3)
        mtmp177 = inline250
        var jp190 int
        switch mtmp177 {
        case 5:
            break Loop_loop186
        default:
            var inline244 int = ref_get__Ref_3int(j__3)
            jp190 = inline244
            var t191 int
            var inline248 int = ref_get__Ref_3int(total__4)
            t191 = inline248
            var t192 int = t191 + jp190
            ref_set__Ref_3int(total__4, t192)
            continue
        }
    }
    var t185 int
    var inline259 int = ref_get__Ref_3int(total__4)
    t185 = inline259
    var inline256 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t185)
    _goml_runtime_core_string_println(inline256)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t222 string = _goml_runtime_core_int_to_string(self__69)
    return t222
}

func main() {
    main0()
}
