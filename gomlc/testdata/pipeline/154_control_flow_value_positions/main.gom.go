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
    var inline285 int = 0
    var inline286 *ref_int_x = ref__Ref_3int(inline285)
    i__0 = inline286
    var sum__1 *ref_int_x
    var inline282 int = 0
    var inline283 *ref_int_x = ref__Ref_3int(inline282)
    sum__1 = inline283
    Loop_loop204:
    for {
        var t205 int
        var inline252 int = ref_get__Ref_3int(i__0)
        t205 = inline252
        var t206 bool = t205 < 5
        if t206 {
            var t207 int
            var inline250 int = ref_get__Ref_3int(i__0)
            t207 = inline250
            var t208 int = t207 + 1
            ref_set__Ref_3int(i__0, t208)
            var t213 int
            var inline246 int = ref_get__Ref_3int(i__0)
            t213 = inline246
            var t214 bool
            var inline243 int = 3
            var inline244 bool = t213 == inline243
            t214 = inline244
            var jp210 int
            if t214 {
                continue
            } else {
                var inline237 int = ref_get__Ref_3int(i__0)
                jp210 = inline237
                var t211 int
                var inline241 int = ref_get__Ref_3int(sum__1)
                t211 = inline241
                var t212 int = t211 + jp210
                ref_set__Ref_3int(sum__1, t212)
                continue
            }
        } else {
            break Loop_loop204
        }
    }
    var t193 int
    var inline280 int = ref_get__Ref_3int(sum__1)
    t193 = inline280
    var inline277 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline277)
    var j__3 *ref_int_x
    var inline274 int = 0
    var inline275 *ref_int_x = ref__Ref_3int(inline274)
    j__3 = inline275
    var total__4 *ref_int_x
    var inline271 int = 0
    var inline272 *ref_int_x = ref__Ref_3int(inline271)
    total__4 = inline272
    Loop_loop196:
    for {
        var t197 int
        var inline264 int = ref_get__Ref_3int(j__3)
        t197 = inline264
        var t198 int = t197 + 1
        ref_set__Ref_3int(j__3, t198)
        var mtmp187 int
        var inline260 int = ref_get__Ref_3int(j__3)
        mtmp187 = inline260
        var jp200 int
        switch mtmp187 {
        case 5:
            break Loop_loop196
        default:
            var inline254 int = ref_get__Ref_3int(j__3)
            jp200 = inline254
            var t201 int
            var inline258 int = ref_get__Ref_3int(total__4)
            t201 = inline258
            var t202 int = t201 + jp200
            ref_set__Ref_3int(total__4, t202)
            continue
        }
    }
    var t195 int
    var inline269 int = ref_get__Ref_3int(total__4)
    t195 = inline269
    var inline266 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t195)
    _goml_runtime_core_string_println(inline266)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t232 string = _goml_runtime_core_int_to_string(self__67)
    return t232
}

func main() {
    main0()
}
