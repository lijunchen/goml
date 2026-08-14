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
    var inline272 int32 = 0
    var inline273 *ref_int32_x = ref__Ref_5int32(inline272)
    acc__1 = inline273
    var i__2 *ref_int32_x
    var inline269 int32 = 0
    var inline270 *ref_int32_x = ref__Ref_5int32(inline269)
    i__2 = inline270
    Loop_loop196:
    for {
        var t197 int32
        var inline265 int32 = ref_get__Ref_5int32(i__2)
        t197 = inline265
        var t198 bool = t197 < limit__0
        if t198 {
            var current__3 int32
            var inline263 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline263
            var t199 int32
            var inline261 int32 = ref_get__Ref_5int32(acc__1)
            t199 = inline261
            var t200 int32 = t199 + current__3
            ref_set__Ref_5int32(acc__1, t200)
            var t201 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t201)
            continue
        } else {
            break Loop_loop196
        }
    }
    var inline267 int32 = ref_get__Ref_5int32(acc__1)
    return inline267
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline297 int32 = 0
    var inline298 *ref_int32_x = ref__Ref_5int32(inline297)
    acc__5 = inline298
    var i__6 *ref_int32_x
    var inline294 int32 = 0
    var inline295 *ref_int32_x = ref__Ref_5int32(inline294)
    i__6 = inline295
    var is_even__7 *ref_bool_x
    var inline291 bool = true
    var inline292 *ref_bool_x = ref__Ref_4bool(inline291)
    is_even__7 = inline292
    Loop_loop206:
    for {
        var t207 int32
        var inline287 int32 = ref_get__Ref_5int32(i__6)
        t207 = inline287
        var t208 bool = t207 < limit__4
        if t208 {
            var current__8 int32
            var inline285 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline285
            var t209 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t209)
            var add_now__9 bool
            var inline281 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline281
            var t210 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t210)
            if add_now__9 {
                var t212 int32
                var inline277 int32 = ref_get__Ref_5int32(acc__5)
                t212 = inline277
                var t213 int32 = t212 + current__8
                ref_set__Ref_5int32(acc__5, t213)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop206
        }
    }
    var inline289 int32 = ref_get__Ref_5int32(acc__5)
    return inline289
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline310 string = "sum_to(5)="
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline310)
    _goml_runtime_core_string_print(inline311)
    var inline307 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline307)
    var inline303 string = "sum_even(6)="
    var inline304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline303)
    _goml_runtime_core_string_print(inline304)
    var inline300 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline300)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t242 string = _goml_runtime_core_int32_to_string(self__70)
    return t242
}

func main() {
    main0()
}
