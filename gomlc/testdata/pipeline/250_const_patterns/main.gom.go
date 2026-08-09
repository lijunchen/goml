package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
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

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
}

type Tuple2_3int_5uint8 struct {
    _0 int
    _1 uint8
}

const (
    ratio float64 = 1.5
    computed_enabled bool = true
    classified_at_compile_time int = 1
)

func classify(value__1 int) string {
    switch value__1 {
    case 42:
        return "known"
    case 7:
        return "known"
    default:
        return "other"
    }
}

func classify_bool(value__2 bool) string {
    switch value__2 {
    case true:
        return "enabled"
    case false:
        return "disabled"
    default:
        panic("non-exhaustive match")
    }
}

func classify_computed_bool(value__3 bool) string {
    var t224 bool = value__3 == computed_enabled
    if t224 {
        return "enabled"
    } else {
        var t227 bool = value__3 == false
        if t227 {
            return "disabled"
        } else {
            var t228 string = missing__string("")
            return t228
        }
    }
}

func classify_pair(value__4 Tuple2_3int_5uint8) bool {
    var x174 int = value__4._0
    var x175 uint8 = value__4._1
    switch x175 {
    case 65:
        switch x174 {
        case 42:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func classify_string(value__5 string) bool {
    switch value__5 {
    case "hello":
        return true
    default:
        return false
    }
}

func classify_float(value__6 float64) bool {
    var t243 bool = value__6 == ratio
    if t243 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__12 *ref_int_x
    var inline326 int = 0
    var inline327 *ref_int_x = ref__Ref_3int(inline326)
    total__12 = inline327
    var for_source179 [2]int = [2]int{1, 2}
    var for_limit180 int = 2
    var for_index181 int = 0
    Loop_loop260:
    for {
        var t261 bool = for_index181 < for_limit180
        if t261 {
            var for_item182 int = array_get__Array_2_3int(for_source179, for_index181)
            var t262 int = for_index181 + 1
            for_index181 = t262
            var t263 int
            var inline322 int = ref_get__Ref_3int(total__12)
            t263 = inline322
            var t264 int = t263 + for_item182
            ref_set__Ref_3int(total__12, t264)
            continue
        } else {
            break Loop_loop260
        }
    }
    var inline324 int = ref_get__Ref_3int(total__12)
    return inline324
}

func main0() struct{} {
    var t267 string = classify(42)
    println__T_string(t267)
    var t268 string = classify(7)
    println__T_string(t268)
    var t269 string = classify(0)
    println__T_string(t269)
    var t270 string = classify_bool(true)
    println__T_string(t270)
    var t271 string = classify_bool(false)
    println__T_string(t271)
    var t272 string = classify_computed_bool(true)
    println__T_string(t272)
    var t273 string = classify_computed_bool(false)
    println__T_string(t273)
    var t274 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t275 bool = classify_pair(t274)
    println__T_bool(t275)
    var t276 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t277 bool = classify_pair(t276)
    println__T_bool(t277)
    var t278 bool = classify_string("hello")
    println__T_bool(t278)
    var t279 bool = classify_float(1.5)
    var inline369 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t279)
    _goml_runtime_core_string_println(inline369)
    var t280 int
    var inline367 int = 9
    t280 = inline367
    var inline364 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t280)
    _goml_runtime_core_string_println(inline364)
    var t281 int
    var inline360 int = 11
    t281 = inline360
    var inline357 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t281)
    _goml_runtime_core_string_println(inline357)
    var t282 bool
    var inline355 int = 42
    switch inline355 {
    case 42:
        t282 = true
    default:
        t282 = false
    }
    var inline352 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t282)
    _goml_runtime_core_string_println(inline352)
    var t283 bool
    var inline350 int = 41
    switch inline350 {
    case 42:
        t283 = true
    default:
        t283 = false
    }
    var inline347 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t283)
    _goml_runtime_core_string_println(inline347)
    var t284 bool
    var inline344 int = 42
    switch inline344 {
    case 42:
        t284 = true
    default:
        t284 = false
    }
    var inline341 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t284)
    _goml_runtime_core_string_println(inline341)
    var t285 bool
    var inline338 int = 41
    switch inline338 {
    case 42:
        t285 = true
    default:
        t285 = false
    }
    var inline335 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t285)
    _goml_runtime_core_string_println(inline335)
    var t286 int = for_binding()
    var inline332 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t286)
    _goml_runtime_core_string_println(inline332)
    var inline329 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(classified_at_compile_time)
    _goml_runtime_core_string_println(inline329)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t297 string
    t297 = value__31
    _goml_runtime_core_string_println(t297)
    return struct{}{}
}

func println__T_bool(value__31 bool) struct{} {
    var t300 string
    var inline373 string = _goml_runtime_core_bool_to_string(value__31)
    t300 = inline373
    _goml_runtime_core_string_println(t300)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t309 string = _goml_runtime_core_bool_to_string(self__66)
    return t309
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t312 string = _goml_runtime_core_int_to_string(self__69)
    return t312
}

func main() {
    main0()
}
