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
    Loop_loop427:
    for {
        var t428 bool = index__2 < count__1
        if t428 {
            var t429 int = result__3 + offset__6
            result__3 = t429
            var t430 int = index__2 + 1
            index__2 = t430
            continue
        } else {
            break Loop_loop427
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop434:
    for {
        var t435 bool = index__9 < count__8
        if t435 {
            var quotient__11 int = 100 / divisor__7
            var t436 int = result__10 + quotient__11
            result__10 = t436
            var t437 int = index__9 + 1
            index__9 = t437
            continue
        } else {
            break Loop_loop434
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop441:
    for {
        var t442 bool = index__13 < count__12
        if t442 {
            var derived__16 int = value__14 + 1
            var t443 int = result__15 + derived__16
            result__15 = t443
            var t444 int = value__14 + 1
            value__14 = t444
            var t445 int = index__13 + 1
            index__13 = t445
            continue
        } else {
            break Loop_loop441
        }
    }
    return result__15
}

func main0() struct{} {
    var t447 int = optimized(3, 4)
    var inline463 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t447)
    _goml_runtime_core_string_println(inline463)
    var t448 int = guarded(0, 0)
    var inline460 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t448)
    _goml_runtime_core_string_println(inline460)
    var t449 int = changing(3)
    var inline457 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t449)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t455 string = _goml_runtime_core_int_to_string(self__151)
    return t455
}

func main() {
    main0()
}
