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
    var t227 bool = value__3 == COMPUTED_ENABLED
    if t227 {
        return "enabled"
    } else {
        var t230 bool = value__3 == false
        if t230 {
            return "disabled"
        } else {
            var t231 string = missing__string("")
            return t231
        }
    }
}

func classify_comptime_guard(value__4 bool) string {
    var t236 bool = value__4 == true
    var jp235 string
    if t236 {
        jp235 = "enabled"
    } else {
        var t245 bool = value__4 == false
        if t245 {
            jp235 = "disabled"
        } else {
            var t246 string = missing__string("")
            jp235 = t246
        }
    }
    return jp235
}

func classify_pair(value__6 Tuple2_3int_5uint8) bool {
    var x175 int = value__6._0
    var x176 uint8 = value__6._1
    switch x176 {
    case 65:
        switch x175 {
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
    var t261 bool = value__8 == RATIO
    if t261 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__14 *ref_int_x
    var inline346 int = 0
    var inline347 *ref_int_x = ref__Ref_3int(inline346)
    total__14 = inline347
    var for_source180 [2]int = [2]int{1, 2}
    var for_limit181 int = 2
    var for_index182 int = 0
    Loop_loop278:
    for {
        var t279 bool = for_index182 < for_limit181
        if t279 {
            var for_item183 int = array_get__Array_2_3int(for_source180, for_index182)
            var t280 int = for_index182 + 1
            for_index182 = t280
            var t281 int
            var inline342 int = ref_get__Ref_3int(total__14)
            t281 = inline342
            var t282 int = t281 + for_item183
            ref_set__Ref_3int(total__14, t282)
            continue
        } else {
            break Loop_loop278
        }
    }
    var inline344 int = ref_get__Ref_3int(total__14)
    return inline344
}

func main0() struct{} {
    var t285 string = classify(42)
    println__T_string(t285)
    var t286 string = classify(7)
    println__T_string(t286)
    var t287 string = classify(0)
    println__T_string(t287)
    var t288 string = classify_bool(true)
    println__T_string(t288)
    var t289 string = classify_bool(false)
    println__T_string(t289)
    var t290 string = classify_computed_bool(true)
    println__T_string(t290)
    var t291 string = classify_computed_bool(false)
    println__T_string(t291)
    var t292 string = classify_comptime_guard(true)
    println__T_string(t292)
    var t293 string = classify_comptime_guard(false)
    println__T_string(t293)
    var t294 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t295 bool = classify_pair(t294)
    println__T_bool(t295)
    var t296 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t297 bool = classify_pair(t296)
    println__T_bool(t297)
    var t298 bool = classify_string("hello")
    println__T_bool(t298)
    var t299 bool = classify_float(1.5)
    var inline389 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t299)
    _goml_runtime_core_string_println(inline389)
    var t300 int
    var inline387 int = 9
    t300 = inline387
    var inline384 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t300)
    _goml_runtime_core_string_println(inline384)
    var t301 int
    var inline380 int = 11
    t301 = inline380
    var inline377 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t301)
    _goml_runtime_core_string_println(inline377)
    var t302 bool
    var inline375 int = 42
    switch inline375 {
    case 42:
        t302 = true
    default:
        t302 = false
    }
    var inline372 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t302)
    _goml_runtime_core_string_println(inline372)
    var t303 bool
    var inline370 int = 41
    switch inline370 {
    case 42:
        t303 = true
    default:
        t303 = false
    }
    var inline367 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t303)
    _goml_runtime_core_string_println(inline367)
    var t304 bool
    var inline364 int = 42
    switch inline364 {
    case 42:
        t304 = true
    default:
        t304 = false
    }
    var inline361 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t304)
    _goml_runtime_core_string_println(inline361)
    var t305 bool
    var inline358 int = 41
    switch inline358 {
    case 42:
        t305 = true
    default:
        t305 = false
    }
    var inline355 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t305)
    _goml_runtime_core_string_println(inline355)
    var t306 int = for_binding()
    var inline352 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t306)
    _goml_runtime_core_string_println(inline352)
    var inline349 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(CLASSIFIED_AT_COMPILE_TIME)
    _goml_runtime_core_string_println(inline349)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t317 string
    t317 = value__1
    _goml_runtime_core_string_println(t317)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t320 string
    var inline393 string = _goml_runtime_core_bool_to_string(value__1)
    t320 = inline393
    _goml_runtime_core_string_println(t320)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t329 string = _goml_runtime_core_bool_to_string(self__64)
    return t329
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t332 string = _goml_runtime_core_int_to_string(self__67)
    return t332
}

func main() {
    main0()
}
