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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var base__4 int32 = 10
    var sum__5 int32 = base__4 + 5
    var diff__6 int32 = sum__5 - 3
    var prod__7 int32 = diff__6 * 2
    var quot__8 int32 = prod__7 / 4
    var inline283 string = "sum="
    var inline284 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline285 string = inline283 + inline284
    println__T_string(inline285)
    var inline278 string = "diff="
    var inline279 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline280 string = inline278 + inline279
    println__T_string(inline280)
    var inline273 string = "prod="
    var inline274 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline275 string = inline273 + inline274
    println__T_string(inline275)
    var inline268 string = "quot="
    var inline269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline270 string = inline268 + inline269
    println__T_string(inline270)
    var jp207 bool
    jp207 = false
    var jp209 bool
    jp209 = true
    var not_result__11 bool = !false
    var t220 bool = !jp207
    var jp213 bool
    if t220 {
        var t221 int32 = prod__7 * base__4
        var t222 int32 = sum__5 + t221
        var t223 int32 = prod__7 / 2
        var mtmp193 int32 = t222 - t223
        switch mtmp193 {
        case 0:
            jp213 = false
        default:
            jp213 = true
        }
    } else {
        jp213 = false
    }
    var jp211 bool
    if jp213 {
        jp211 = true
    } else {
        var t214 int32 = diff__6 - quot__8
        var t215 int32 = t214 + base__4
        var t216 int32 = sum__5 / 2
        var mtmp194 int32 = t215 - t216
        var jp218 bool
        switch mtmp194 {
        case 0:
            jp218 = false
        default:
            jp218 = true
        }
        var t219 bool = !jp218
        jp211 = t219
    }
    var inline263 string = "and="
    var inline264 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp207)
    var inline265 string = inline263 + inline264
    println__T_string(inline265)
    var inline258 string = "or="
    var inline259 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp209)
    var inline260 string = inline258 + inline259
    println__T_string(inline260)
    var inline253 string = "not="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline255 string = inline253 + inline254
    println__T_string(inline255)
    var inline248 string = "mixed="
    var inline249 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp211)
    var inline250 string = inline248 + inline249
    println__T_string(inline250)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t227 string
    t227 = value__1
    _goml_runtime_core_string_println(t227)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t231 string = _goml_runtime_core_int32_to_string(self__33)
    return t231
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t234 string = _goml_runtime_core_bool_to_string(self__64)
    return t234
}

func main() {
    main0()
}
