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
    RATIO float64 = 1.5
    COMPUTED_ENABLED bool = true
    CLASSIFIED_AT_COMPILE_TIME int = 1
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
    var t237 bool = value__3 == COMPUTED_ENABLED
    if t237 {
        return "enabled"
    } else {
        var t240 bool = value__3 == false
        if t240 {
            return "disabled"
        } else {
            var t241 string = missing__string("")
            return t241
        }
    }
}

func classify_comptime_guard(value__4 bool) string {
    var t246 bool = value__4 == true
    var jp245 string
    if t246 {
        jp245 = "enabled"
    } else {
        var t255 bool = value__4 == false
        if t255 {
            jp245 = "disabled"
        } else {
            var t256 string = missing__string("")
            jp245 = t256
        }
    }
    return jp245
}

func classify_pair(value__6 Tuple2_3int_5uint8) bool {
    var x185 int = value__6._0
    var x186 uint8 = value__6._1
    switch x186 {
    case 65:
        switch x185 {
        case 42:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func classify_string(value__7 string) bool {
    switch value__7 {
    case "hello":
        return true
    default:
        return false
    }
}

func classify_float(value__8 float64) bool {
    var t271 bool = value__8 == RATIO
    if t271 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__14 *ref_int_x
    var inline356 int = 0
    var inline357 *ref_int_x = ref__Ref_3int(inline356)
    total__14 = inline357
    var for_source190 [2]int = [2]int{1, 2}
    var for_limit191 int = 2
    var for_index192 int = 0
    Loop_loop288:
    for {
        var t289 bool = for_index192 < for_limit191
        if t289 {
            var for_item193 int = array_get__Array_2_3int(for_source190, for_index192)
            var t290 int = for_index192 + 1
            for_index192 = t290
            var t291 int
            var inline352 int = ref_get__Ref_3int(total__14)
            t291 = inline352
            var t292 int = t291 + for_item193
            ref_set__Ref_3int(total__14, t292)
            continue
        } else {
            break Loop_loop288
        }
    }
    var inline354 int = ref_get__Ref_3int(total__14)
    return inline354
}

func main0() struct{} {
    var t295 string = classify(42)
    println__T_string(t295)
    var t296 string = classify(7)
    println__T_string(t296)
    var t297 string = classify(0)
    println__T_string(t297)
    var t298 string = classify_bool(true)
    println__T_string(t298)
    var t299 string = classify_bool(false)
    println__T_string(t299)
    var t300 string = classify_computed_bool(true)
    println__T_string(t300)
    var t301 string = classify_computed_bool(false)
    println__T_string(t301)
    var t302 string = classify_comptime_guard(true)
    println__T_string(t302)
    var t303 string = classify_comptime_guard(false)
    println__T_string(t303)
    var t304 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t305 bool = classify_pair(t304)
    println__T_bool(t305)
    var t306 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t307 bool = classify_pair(t306)
    println__T_bool(t307)
    var t308 bool = classify_string("hello")
    println__T_bool(t308)
    var t309 bool = classify_float(1.5)
    var inline399 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t309)
    _goml_runtime_core_string_println(inline399)
    var t310 int
    var inline397 int = 9
    t310 = inline397
    var inline394 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t310)
    _goml_runtime_core_string_println(inline394)
    var t311 int
    var inline390 int = 11
    t311 = inline390
    var inline387 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t311)
    _goml_runtime_core_string_println(inline387)
    var t312 bool
    var inline385 int = 42
    switch inline385 {
    case 42:
        t312 = true
    default:
        t312 = false
    }
    var inline382 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t312)
    _goml_runtime_core_string_println(inline382)
    var t313 bool
    var inline380 int = 41
    switch inline380 {
    case 42:
        t313 = true
    default:
        t313 = false
    }
    var inline377 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t313)
    _goml_runtime_core_string_println(inline377)
    var t314 bool
    var inline374 int = 42
    switch inline374 {
    case 42:
        t314 = true
    default:
        t314 = false
    }
    var inline371 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t314)
    _goml_runtime_core_string_println(inline371)
    var t315 bool
    var inline368 int = 41
    switch inline368 {
    case 42:
        t315 = true
    default:
        t315 = false
    }
    var inline365 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t315)
    _goml_runtime_core_string_println(inline365)
    var t316 int = for_binding()
    var inline362 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t316)
    _goml_runtime_core_string_println(inline362)
    var inline359 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(CLASSIFIED_AT_COMPILE_TIME)
    _goml_runtime_core_string_println(inline359)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t327 string
    t327 = value__1
    _goml_runtime_core_string_println(t327)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t330 string
    var inline403 string = _goml_runtime_core_bool_to_string(value__1)
    t330 = inline403
    _goml_runtime_core_string_println(t330)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t339 string = _goml_runtime_core_bool_to_string(self__64)
    return t339
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t342 string = _goml_runtime_core_int_to_string(self__67)
    return t342
}

func main() {
    main0()
}
