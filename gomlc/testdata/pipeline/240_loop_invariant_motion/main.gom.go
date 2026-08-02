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
    Loop_loop171:
    for {
        var t172 bool = index__2 < count__1
        if t172 {
            var t173 int = result__3 + offset__6
            result__3 = t173
            var t174 int = index__2 + 1
            index__2 = t174
            continue
        } else {
            break Loop_loop171
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop178:
    for {
        var t179 bool = index__9 < count__8
        if t179 {
            var quotient__11 int = 100 / divisor__7
            var t180 int = result__10 + quotient__11
            result__10 = t180
            var t181 int = index__9 + 1
            index__9 = t181
            continue
        } else {
            break Loop_loop178
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop185:
    for {
        var t186 bool = index__13 < count__12
        if t186 {
            var derived__16 int = value__14 + 1
            var t187 int = result__15 + derived__16
            result__15 = t187
            var t188 int = value__14 + 1
            value__14 = t188
            var t189 int = index__13 + 1
            index__13 = t189
            continue
        } else {
            break Loop_loop185
        }
    }
    return result__15
}

func main0() struct{} {
    var t191 int = optimized(3, 4)
    var inline207 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t191)
    _goml_runtime_core_string_println(inline207)
    var t192 int = guarded(0, 0)
    var inline204 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t192)
    _goml_runtime_core_string_println(inline204)
    var t193 int = changing(3)
    var inline201 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t199 string = _goml_runtime_core_int_to_string(self__40)
    return t199
}

func main() {
    main0()
}
