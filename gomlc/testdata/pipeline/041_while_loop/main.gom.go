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
    var inline277 int32 = 0
    var inline278 *ref_int32_x = ref__Ref_5int32(inline277)
    acc__1 = inline278
    var i__2 *ref_int32_x
    var inline274 int32 = 0
    var inline275 *ref_int32_x = ref__Ref_5int32(inline274)
    i__2 = inline275
    Loop_loop201:
    for {
        var t202 int32
        var inline270 int32 = ref_get__Ref_5int32(i__2)
        t202 = inline270
        var t203 bool = t202 < limit__0
        if t203 {
            var current__3 int32
            var inline268 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline268
            var t204 int32
            var inline266 int32 = ref_get__Ref_5int32(acc__1)
            t204 = inline266
            var t205 int32 = t204 + current__3
            ref_set__Ref_5int32(acc__1, t205)
            var t206 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t206)
            continue
        } else {
            break Loop_loop201
        }
    }
    var inline272 int32 = ref_get__Ref_5int32(acc__1)
    return inline272
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline302 int32 = 0
    var inline303 *ref_int32_x = ref__Ref_5int32(inline302)
    acc__5 = inline303
    var i__6 *ref_int32_x
    var inline299 int32 = 0
    var inline300 *ref_int32_x = ref__Ref_5int32(inline299)
    i__6 = inline300
    var is_even__7 *ref_bool_x
    var inline296 bool = true
    var inline297 *ref_bool_x = ref__Ref_4bool(inline296)
    is_even__7 = inline297
    Loop_loop211:
    for {
        var t212 int32
        var inline292 int32 = ref_get__Ref_5int32(i__6)
        t212 = inline292
        var t213 bool = t212 < limit__4
        if t213 {
            var current__8 int32
            var inline290 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline290
            var t214 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t214)
            var add_now__9 bool
            var inline286 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline286
            var t215 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t215)
            if add_now__9 {
                var t217 int32
                var inline282 int32 = ref_get__Ref_5int32(acc__5)
                t217 = inline282
                var t218 int32 = t217 + current__8
                ref_set__Ref_5int32(acc__5, t218)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop211
        }
    }
    var inline294 int32 = ref_get__Ref_5int32(acc__5)
    return inline294
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline315 string = "sum_to(5)="
    var inline316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline315)
    _goml_runtime_core_string_print(inline316)
    var inline312 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline312)
    var inline308 string = "sum_even(6)="
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline308)
    _goml_runtime_core_string_print(inline309)
    var inline305 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline305)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t247 string = _goml_runtime_core_int32_to_string(self__70)
    return t247
}

func main() {
    main0()
}
