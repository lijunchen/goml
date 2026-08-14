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

func optimized(base__0 int, count__1 int) int {
    var index__2 int = 0
    var result__3 int = 0
    var square__4 int = base__0 * base__0
    var scaled__5 int = square__4 * 17
    var offset__6 int = scaled__5 + base__0
    Loop_loop203:
    for {
        var t204 bool = index__2 < count__1
        if t204 {
            var t205 int = result__3 + offset__6
            result__3 = t205
            var t206 int = index__2 + 1
            index__2 = t206
            continue
        } else {
            break Loop_loop203
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop210:
    for {
        var t211 bool = index__9 < count__8
        if t211 {
            var quotient__11 int = 100 / divisor__7
            var t212 int = result__10 + quotient__11
            result__10 = t212
            var t213 int = index__9 + 1
            index__9 = t213
            continue
        } else {
            break Loop_loop210
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop217:
    for {
        var t218 bool = index__13 < count__12
        if t218 {
            var derived__16 int = value__14 + 1
            var t219 int = result__15 + derived__16
            result__15 = t219
            var t220 int = value__14 + 1
            value__14 = t220
            var t221 int = index__13 + 1
            index__13 = t221
            continue
        } else {
            break Loop_loop217
        }
    }
    return result__15
}

func main0() struct{} {
    var t223 int = optimized(3, 4)
    var inline239 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t223)
    _goml_runtime_core_string_println(inline239)
    var t224 int = guarded(0, 0)
    var inline236 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t224)
    _goml_runtime_core_string_println(inline236)
    var t225 int = changing(3)
    var inline233 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t225)
    _goml_runtime_core_string_println(inline233)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t231 string = _goml_runtime_core_int_to_string(self__67)
    return t231
}

func main() {
    main0()
}
