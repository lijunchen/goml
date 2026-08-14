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
    Loop_loop198:
    for {
        var t199 bool = index__2 < count__1
        if t199 {
            var t200 int = result__3 + offset__6
            result__3 = t200
            var t201 int = index__2 + 1
            index__2 = t201
            continue
        } else {
            break Loop_loop198
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop205:
    for {
        var t206 bool = index__9 < count__8
        if t206 {
            var quotient__11 int = 100 / divisor__7
            var t207 int = result__10 + quotient__11
            result__10 = t207
            var t208 int = index__9 + 1
            index__9 = t208
            continue
        } else {
            break Loop_loop205
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop212:
    for {
        var t213 bool = index__13 < count__12
        if t213 {
            var derived__16 int = value__14 + 1
            var t214 int = result__15 + derived__16
            result__15 = t214
            var t215 int = value__14 + 1
            value__14 = t215
            var t216 int = index__13 + 1
            index__13 = t216
            continue
        } else {
            break Loop_loop212
        }
    }
    return result__15
}

func main0() struct{} {
    var t218 int = optimized(3, 4)
    var inline234 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t218)
    _goml_runtime_core_string_println(inline234)
    var t219 int = guarded(0, 0)
    var inline231 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t219)
    _goml_runtime_core_string_println(inline231)
    var t220 int = changing(3)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t220)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t226 string = _goml_runtime_core_int_to_string(self__67)
    return t226
}

func main() {
    main0()
}
