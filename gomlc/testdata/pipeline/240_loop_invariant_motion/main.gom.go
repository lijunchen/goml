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
    Loop_loop152:
    for {
        var t153 bool = index__2 < count__1
        if t153 {
            var t154 int = result__3 + offset__6
            result__3 = t154
            var t155 int = index__2 + 1
            index__2 = t155
            continue
        } else {
            break Loop_loop152
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop159:
    for {
        var t160 bool = index__9 < count__8
        if t160 {
            var quotient__11 int = 100 / divisor__7
            var t161 int = result__10 + quotient__11
            result__10 = t161
            var t162 int = index__9 + 1
            index__9 = t162
            continue
        } else {
            break Loop_loop159
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop166:
    for {
        var t167 bool = index__13 < count__12
        if t167 {
            var derived__16 int = value__14 + 1
            var t168 int = result__15 + derived__16
            result__15 = t168
            var t169 int = value__14 + 1
            value__14 = t169
            var t170 int = index__13 + 1
            index__13 = t170
            continue
        } else {
            break Loop_loop166
        }
    }
    return result__15
}

func main0() struct{} {
    var t172 int = optimized(3, 4)
    var inline188 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t172)
    _goml_runtime_core_string_println(inline188)
    var t173 int = guarded(0, 0)
    var inline185 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t173)
    _goml_runtime_core_string_println(inline185)
    var t174 int = changing(3)
    var inline182 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t174)
    _goml_runtime_core_string_println(inline182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t180 string = _goml_runtime_core_int_to_string(self__69)
    return t180
}

func main() {
    main0()
}
