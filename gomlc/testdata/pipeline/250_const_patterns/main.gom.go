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
    var t242 bool = value__3 == COMPUTED_ENABLED
    if t242 {
        return "enabled"
    } else {
        var t245 bool = value__3 == false
        if t245 {
            return "disabled"
        } else {
            var t246 string = missing__string("")
            return t246
        }
    }
}

func classify_comptime_guard(value__4 bool) string {
    var t251 bool = value__4 == true
    var jp250 string
    if t251 {
        jp250 = "enabled"
    } else {
        var t260 bool = value__4 == false
        if t260 {
            jp250 = "disabled"
        } else {
            var t261 string = missing__string("")
            jp250 = t261
        }
    }
    return jp250
}

func classify_pair(value__6 Tuple2_3int_5uint8) bool {
    var x190 int = value__6._0
    var x191 uint8 = value__6._1
    switch x191 {
    case 65:
        switch x190 {
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
    var t276 bool = value__8 == RATIO
    if t276 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__14 *ref_int_x
    var inline361 int = 0
    var inline362 *ref_int_x = ref__Ref_3int(inline361)
    total__14 = inline362
    var for_source195 [2]int = [2]int{1, 2}
    var for_limit196 int = 2
    var for_index197 int = 0
    Loop_loop293:
    for {
        var t294 bool = for_index197 < for_limit196
        if t294 {
            var for_item198 int = array_get__Array_2_3int(for_source195, for_index197)
            var t295 int = for_index197 + 1
            for_index197 = t295
            var t296 int
            var inline357 int = ref_get__Ref_3int(total__14)
            t296 = inline357
            var t297 int = t296 + for_item198
            ref_set__Ref_3int(total__14, t297)
            continue
        } else {
            break Loop_loop293
        }
    }
    var inline359 int = ref_get__Ref_3int(total__14)
    return inline359
}

func main0() struct{} {
    var t300 string = classify(42)
    println__T_string(t300)
    var t301 string = classify(7)
    println__T_string(t301)
    var t302 string = classify(0)
    println__T_string(t302)
    var t303 string = classify_bool(true)
    println__T_string(t303)
    var t304 string = classify_bool(false)
    println__T_string(t304)
    var t305 string = classify_computed_bool(true)
    println__T_string(t305)
    var t306 string = classify_computed_bool(false)
    println__T_string(t306)
    var t307 string = classify_comptime_guard(true)
    println__T_string(t307)
    var t308 string = classify_comptime_guard(false)
    println__T_string(t308)
    var t309 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t310 bool = classify_pair(t309)
    println__T_bool(t310)
    var t311 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t312 bool = classify_pair(t311)
    println__T_bool(t312)
    var t313 bool = classify_string("hello")
    println__T_bool(t313)
    var t314 bool = classify_float(1.5)
    var inline404 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t314)
    _goml_runtime_core_string_println(inline404)
    var t315 int
    var inline402 int = 9
    t315 = inline402
    var inline399 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t315)
    _goml_runtime_core_string_println(inline399)
    var t316 int
    var inline395 int = 11
    t316 = inline395
    var inline392 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t316)
    _goml_runtime_core_string_println(inline392)
    var t317 bool
    var inline390 int = 42
    switch inline390 {
    case 42:
        t317 = true
    default:
        t317 = false
    }
    var inline387 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t317)
    _goml_runtime_core_string_println(inline387)
    var t318 bool
    var inline385 int = 41
    switch inline385 {
    case 42:
        t318 = true
    default:
        t318 = false
    }
    var inline382 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t318)
    _goml_runtime_core_string_println(inline382)
    var t319 bool
    var inline379 int = 42
    switch inline379 {
    case 42:
        t319 = true
    default:
        t319 = false
    }
    var inline376 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t319)
    _goml_runtime_core_string_println(inline376)
    var t320 bool
    var inline373 int = 41
    switch inline373 {
    case 42:
        t320 = true
    default:
        t320 = false
    }
    var inline370 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t320)
    _goml_runtime_core_string_println(inline370)
    var t321 int = for_binding()
    var inline367 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t321)
    _goml_runtime_core_string_println(inline367)
    var inline364 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(CLASSIFIED_AT_COMPILE_TIME)
    _goml_runtime_core_string_println(inline364)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t332 string
    t332 = value__1
    _goml_runtime_core_string_println(t332)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t335 string
    var inline408 string = _goml_runtime_core_bool_to_string(value__1)
    t335 = inline408
    _goml_runtime_core_string_println(t335)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t344 string = _goml_runtime_core_bool_to_string(self__64)
    return t344
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t347 string = _goml_runtime_core_int_to_string(self__67)
    return t347
}

func main() {
    main0()
}
