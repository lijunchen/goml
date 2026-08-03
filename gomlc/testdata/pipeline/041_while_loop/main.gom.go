package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

func sum_to(limit__0 int32) int32 {
    var acc__1 *ref_int32_x
    var inline267 int32 = 0
    var inline268 *ref_int32_x = ref__Ref_5int32(inline267)
    acc__1 = inline268
    var i__2 *ref_int32_x
    var inline264 int32 = 0
    var inline265 *ref_int32_x = ref__Ref_5int32(inline264)
    i__2 = inline265
    Loop_loop191:
    for {
        var t192 int32
        var inline260 int32 = ref_get__Ref_5int32(i__2)
        t192 = inline260
        var t193 bool = t192 < limit__0
        if t193 {
            var current__3 int32
            var inline258 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline258
            var t194 int32
            var inline256 int32 = ref_get__Ref_5int32(acc__1)
            t194 = inline256
            var t195 int32 = t194 + current__3
            ref_set__Ref_5int32(acc__1, t195)
            var t196 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t196)
            continue
        } else {
            break Loop_loop191
        }
    }
    var inline262 int32 = ref_get__Ref_5int32(acc__1)
    return inline262
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline292 int32 = 0
    var inline293 *ref_int32_x = ref__Ref_5int32(inline292)
    acc__5 = inline293
    var i__6 *ref_int32_x
    var inline289 int32 = 0
    var inline290 *ref_int32_x = ref__Ref_5int32(inline289)
    i__6 = inline290
    var is_even__7 *ref_bool_x
    var inline286 bool = true
    var inline287 *ref_bool_x = ref__Ref_4bool(inline286)
    is_even__7 = inline287
    Loop_loop201:
    for {
        var t202 int32
        var inline282 int32 = ref_get__Ref_5int32(i__6)
        t202 = inline282
        var t203 bool = t202 < limit__4
        if t203 {
            var current__8 int32
            var inline280 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline280
            var t204 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t204)
            var add_now__9 bool
            var inline276 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline276
            var t205 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t205)
            if add_now__9 {
                var t207 int32
                var inline272 int32 = ref_get__Ref_5int32(acc__5)
                t207 = inline272
                var t208 int32 = t207 + current__8
                ref_set__Ref_5int32(acc__5, t208)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop201
        }
    }
    var inline284 int32 = ref_get__Ref_5int32(acc__5)
    return inline284
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline305 string = "sum_to(5)="
    var inline306 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline305)
    _goml_runtime_core_string_print(inline306)
    var inline302 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline302)
    var inline298 string = "sum_even(6)="
    var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline298)
    _goml_runtime_core_string_print(inline299)
    var inline295 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline295)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t237 string = _goml_runtime_core_int32_to_string(self__72)
    return t237
}

func main() {
    main0()
}
