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

type Ordering int32

func optimized(base__0 int, count__1 int) int {
    var index__2 int = 0
    var result__3 int = 0
    var square__4 int = base__0 * base__0
    var scaled__5 int = square__4 * 17
    var offset__6 int = scaled__5 + base__0
    Loop_loop424:
    for {
        var t425 bool = index__2 < count__1
        if t425 {
            var t426 int = result__3 + offset__6
            result__3 = t426
            var t427 int = index__2 + 1
            index__2 = t427
            continue
        } else {
            break Loop_loop424
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop431:
    for {
        var t432 bool = index__9 < count__8
        if t432 {
            var quotient__11 int = 100 / divisor__7
            var t433 int = result__10 + quotient__11
            result__10 = t433
            var t434 int = index__9 + 1
            index__9 = t434
            continue
        } else {
            break Loop_loop431
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop438:
    for {
        var t439 bool = index__13 < count__12
        if t439 {
            var derived__16 int = value__14 + 1
            var t440 int = result__15 + derived__16
            result__15 = t440
            var t441 int = value__14 + 1
            value__14 = t441
            var t442 int = index__13 + 1
            index__13 = t442
            continue
        } else {
            break Loop_loop438
        }
    }
    return result__15
}

func main0() struct{} {
    var t444 int = optimized(3, 4)
    var inline460 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t444)
    _goml_runtime_core_string_println(inline460)
    var t445 int = guarded(0, 0)
    var inline457 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t445)
    _goml_runtime_core_string_println(inline457)
    var t446 int = changing(3)
    var inline454 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t446)
    _goml_runtime_core_string_println(inline454)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t452 string = _goml_runtime_core_int_to_string(self__151)
    return t452
}

func main() {
    main0()
}
