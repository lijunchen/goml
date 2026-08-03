package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func accumulate(limit__1 int32) Option__int32 {
    var sum__2 *ref_int32_x
    var inline267 int32 = 0
    var inline268 *ref_int32_x = ref__Ref_5int32(inline267)
    sum__2 = inline268
    var i__3 *ref_int32_x
    var inline264 int32 = 0
    var inline265 *ref_int32_x = ref__Ref_5int32(inline264)
    i__3 = inline265
    Loop_loop198:
    for {
        var t199 int32
        var inline260 int32 = ref_get__Ref_5int32(i__3)
        t199 = inline260
        var t200 bool = t199 < limit__1
        if t200 {
            var cur__4 int32
            var inline258 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline258
            var t201 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t201)
            var t207 bool
            var inline253 int32 = 1
            var inline254 bool = cur__4 == inline253
            t207 = inline254
            if t207 {
                continue
            } else {
                var mtmp179 Option__int32
                var inline249 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 2)
                if inline249 {
                    mtmp179 = None{}
                } else {
                    var inline250 int32 = cur__4 + 10
                    var inline251 Option__int32 = Some{
                        _0: inline250,
                    }
                    mtmp179 = inline251
                }
                var jp204 int32
                switch mtmp179.(type) {
                case None:
                    return None{}
                case Some:
                    var x180 int32 = mtmp179.(Some)._0
                    jp204 = x180
                    var t205 int32
                    var inline247 int32 = ref_get__Ref_5int32(sum__2)
                    t205 = inline247
                    var t206 int32 = t205 + jp204
                    ref_set__Ref_5int32(sum__2, t206)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop198
        }
    }
    var t196 int32
    var inline262 int32 = ref_get__Ref_5int32(sum__2)
    t196 = inline262
    var t197 Option__int32 = Some{
        _0: t196,
    }
    return t197
}

func main0() struct{} {
    var t215 Option__int32 = accumulate(2)
    var t216 string
    switch t215.(type) {
    case None:
        t216 = "none"
    case Some:
        var inline283 int32 = t215.(Some)._0
        var inline285 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline283)
        var inline286 string = "some=" + inline285
        t216 = inline286
    default:
        panic("non-exhaustive match")
    }
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline280)
    var t217 Option__int32 = accumulate(4)
    var t218 string
    switch t217.(type) {
    case None:
        t218 = "none"
    case Some:
        var inline275 int32 = t217.(Some)._0
        var inline277 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline275)
        var inline278 string = "some=" + inline277
        t218 = inline278
    default:
        panic("non-exhaustive match")
    }
    var inline272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline272)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__94 int32, other__95 int32) bool {
    var t221 bool = self__94 == other__95
    return t221
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__35)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
