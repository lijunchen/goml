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
    var inline278 string = "sum="
    var inline279 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline280 string = inline278 + inline279
    println__T_string(inline280)
    var inline273 string = "diff="
    var inline274 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline275 string = inline273 + inline274
    println__T_string(inline275)
    var inline268 string = "prod="
    var inline269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline270 string = inline268 + inline269
    println__T_string(inline270)
    var inline263 string = "quot="
    var inline264 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline265 string = inline263 + inline264
    println__T_string(inline265)
    var jp202 bool
    jp202 = false
    var jp204 bool
    jp204 = true
    var not_result__11 bool = !false
    var t215 bool = !jp202
    var jp208 bool
    if t215 {
        var t216 int32 = prod__7 * base__4
        var t217 int32 = sum__5 + t216
        var t218 int32 = prod__7 / 2
        var mtmp188 int32 = t217 - t218
        switch mtmp188 {
        case 0:
            jp208 = false
        default:
            jp208 = true
        }
    } else {
        jp208 = false
    }
    var jp206 bool
    if jp208 {
        jp206 = true
    } else {
        var t209 int32 = diff__6 - quot__8
        var t210 int32 = t209 + base__4
        var t211 int32 = sum__5 / 2
        var mtmp189 int32 = t210 - t211
        var jp213 bool
        switch mtmp189 {
        case 0:
            jp213 = false
        default:
            jp213 = true
        }
        var t214 bool = !jp213
        jp206 = t214
    }
    var inline258 string = "and="
    var inline259 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp202)
    var inline260 string = inline258 + inline259
    println__T_string(inline260)
    var inline253 string = "or="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp204)
    var inline255 string = inline253 + inline254
    println__T_string(inline255)
    var inline248 string = "not="
    var inline249 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline250 string = inline248 + inline249
    println__T_string(inline250)
    var inline243 string = "mixed="
    var inline244 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp206)
    var inline245 string = inline243 + inline244
    println__T_string(inline245)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t222 string
    t222 = value__1
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__33)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t229 string = _goml_runtime_core_bool_to_string(self__64)
    return t229
}

func main() {
    main0()
}
