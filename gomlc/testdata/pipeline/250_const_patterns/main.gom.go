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

type Tuple2_3int_5uint8 struct {
    _0 int
    _1 uint8
}

const (
    ratio float64 = 1.5
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

func classify_pair(value__3 Tuple2_3int_5uint8) bool {
    var x173 int = value__3._0
    var x174 uint8 = value__3._1
    switch x174 {
    case 65:
        switch x173 {
        case 42:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func classify_string(value__4 string) bool {
    switch value__4 {
    case "hello":
        return true
    default:
        return false
    }
}

func classify_float(value__5 float64) bool {
    var t229 bool = value__5 == ratio
    if t229 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__11 *ref_int_x
    var inline310 int = 0
    var inline311 *ref_int_x = ref__Ref_3int(inline310)
    total__11 = inline311
    var for_source178 [2]int = [2]int{1, 2}
    var for_limit179 int = 2
    var for_index180 int = 0
    Loop_loop246:
    for {
        var t247 bool = for_index180 < for_limit179
        if t247 {
            var for_item181 int = array_get__Array_2_3int(for_source178, for_index180)
            var t248 int = for_index180 + 1
            for_index180 = t248
            var t249 int
            var inline306 int = ref_get__Ref_3int(total__11)
            t249 = inline306
            var t250 int = t249 + for_item181
            ref_set__Ref_3int(total__11, t250)
            continue
        } else {
            break Loop_loop246
        }
    }
    var inline308 int = ref_get__Ref_3int(total__11)
    return inline308
}

func main0() struct{} {
    var t253 string = classify(42)
    println__T_string(t253)
    var t254 string = classify(7)
    println__T_string(t254)
    var t255 string = classify(0)
    println__T_string(t255)
    var t256 string = classify_bool(true)
    println__T_string(t256)
    var t257 string = classify_bool(false)
    println__T_string(t257)
    var t258 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t259 bool = classify_pair(t258)
    println__T_bool(t259)
    var t260 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t261 bool = classify_pair(t260)
    println__T_bool(t261)
    var t262 bool = classify_string("hello")
    println__T_bool(t262)
    var t263 bool = classify_float(1.5)
    var inline353 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t263)
    _goml_runtime_core_string_println(inline353)
    var t264 int
    var inline351 int = 9
    t264 = inline351
    var inline348 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t264)
    _goml_runtime_core_string_println(inline348)
    var t265 int
    var inline344 int = 11
    t265 = inline344
    var inline341 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t265)
    _goml_runtime_core_string_println(inline341)
    var t266 bool
    var inline339 int = 42
    switch inline339 {
    case 42:
        t266 = true
    default:
        t266 = false
    }
    var inline336 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t266)
    _goml_runtime_core_string_println(inline336)
    var t267 bool
    var inline334 int = 41
    switch inline334 {
    case 42:
        t267 = true
    default:
        t267 = false
    }
    var inline331 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t267)
    _goml_runtime_core_string_println(inline331)
    var t268 bool
    var inline328 int = 42
    switch inline328 {
    case 42:
        t268 = true
    default:
        t268 = false
    }
    var inline325 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t268)
    _goml_runtime_core_string_println(inline325)
    var t269 bool
    var inline322 int = 41
    switch inline322 {
    case 42:
        t269 = true
    default:
        t269 = false
    }
    var inline319 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t269)
    _goml_runtime_core_string_println(inline319)
    var t270 int = for_binding()
    var inline316 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t270)
    _goml_runtime_core_string_println(inline316)
    var inline313 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(classified_at_compile_time)
    _goml_runtime_core_string_println(inline313)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t281 string
    t281 = value__31
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func println__T_bool(value__31 bool) struct{} {
    var t284 string
    var inline357 string = _goml_runtime_core_bool_to_string(value__31)
    t284 = inline357
    _goml_runtime_core_string_println(t284)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t293 string = _goml_runtime_core_bool_to_string(self__66)
    return t293
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t296 string = _goml_runtime_core_int_to_string(self__69)
    return t296
}

func main() {
    main0()
}
