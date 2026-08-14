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
    var inline290 int = 0
    var inline291 *ref_int_x = ref__Ref_3int(inline290)
    i__0 = inline291
    var sum__1 *ref_int_x
    var inline287 int = 0
    var inline288 *ref_int_x = ref__Ref_3int(inline287)
    sum__1 = inline288
    Loop_loop209:
    for {
        var t210 int
        var inline257 int = ref_get__Ref_3int(i__0)
        t210 = inline257
        var t211 bool = t210 < 5
        if t211 {
            var t212 int
            var inline255 int = ref_get__Ref_3int(i__0)
            t212 = inline255
            var t213 int = t212 + 1
            ref_set__Ref_3int(i__0, t213)
            var t218 int
            var inline251 int = ref_get__Ref_3int(i__0)
            t218 = inline251
            var t219 bool
            var inline248 int = 3
            var inline249 bool = t218 == inline248
            t219 = inline249
            var jp215 int
            if t219 {
                continue
            } else {
                var inline242 int = ref_get__Ref_3int(i__0)
                jp215 = inline242
                var t216 int
                var inline246 int = ref_get__Ref_3int(sum__1)
                t216 = inline246
                var t217 int = t216 + jp215
                ref_set__Ref_3int(sum__1, t217)
                continue
            }
        } else {
            break Loop_loop209
        }
    }
    var t198 int
    var inline285 int = ref_get__Ref_3int(sum__1)
    t198 = inline285
    var inline282 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline282)
    var j__3 *ref_int_x
    var inline279 int = 0
    var inline280 *ref_int_x = ref__Ref_3int(inline279)
    j__3 = inline280
    var total__4 *ref_int_x
    var inline276 int = 0
    var inline277 *ref_int_x = ref__Ref_3int(inline276)
    total__4 = inline277
    Loop_loop201:
    for {
        var t202 int
        var inline269 int = ref_get__Ref_3int(j__3)
        t202 = inline269
        var t203 int = t202 + 1
        ref_set__Ref_3int(j__3, t203)
        var mtmp192 int
        var inline265 int = ref_get__Ref_3int(j__3)
        mtmp192 = inline265
        var jp205 int
        switch mtmp192 {
        case 5:
            break Loop_loop201
        default:
            var inline259 int = ref_get__Ref_3int(j__3)
            jp205 = inline259
            var t206 int
            var inline263 int = ref_get__Ref_3int(total__4)
            t206 = inline263
            var t207 int = t206 + jp205
            ref_set__Ref_3int(total__4, t207)
            continue
        }
    }
    var t200 int
    var inline274 int = ref_get__Ref_3int(total__4)
    t200 = inline274
    var inline271 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t200)
    _goml_runtime_core_string_println(inline271)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t237 string = _goml_runtime_core_int_to_string(self__67)
    return t237
}

func main() {
    main0()
}
