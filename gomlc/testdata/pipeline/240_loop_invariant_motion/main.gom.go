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
    Loop_loop193:
    for {
        var t194 bool = index__2 < count__1
        if t194 {
            var t195 int = result__3 + offset__6
            result__3 = t195
            var t196 int = index__2 + 1
            index__2 = t196
            continue
        } else {
            break Loop_loop193
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop200:
    for {
        var t201 bool = index__9 < count__8
        if t201 {
            var quotient__11 int = 100 / divisor__7
            var t202 int = result__10 + quotient__11
            result__10 = t202
            var t203 int = index__9 + 1
            index__9 = t203
            continue
        } else {
            break Loop_loop200
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop207:
    for {
        var t208 bool = index__13 < count__12
        if t208 {
            var derived__16 int = value__14 + 1
            var t209 int = result__15 + derived__16
            result__15 = t209
            var t210 int = value__14 + 1
            value__14 = t210
            var t211 int = index__13 + 1
            index__13 = t211
            continue
        } else {
            break Loop_loop207
        }
    }
    return result__15
}

func main0() struct{} {
    var t213 int = optimized(3, 4)
    var inline229 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t213)
    _goml_runtime_core_string_println(inline229)
    var t214 int = guarded(0, 0)
    var inline226 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t214)
    _goml_runtime_core_string_println(inline226)
    var t215 int = changing(3)
    var inline223 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t215)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t221 string = _goml_runtime_core_int_to_string(self__69)
    return t221
}

func main() {
    main0()
}
