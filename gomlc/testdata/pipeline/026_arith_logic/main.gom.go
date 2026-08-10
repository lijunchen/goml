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
    var inline268 string = "sum="
    var inline269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline270 string = inline268 + inline269
    println__T_string(inline270)
    var inline263 string = "diff="
    var inline264 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline265 string = inline263 + inline264
    println__T_string(inline265)
    var inline258 string = "prod="
    var inline259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline260 string = inline258 + inline259
    println__T_string(inline260)
    var inline253 string = "quot="
    var inline254 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline255 string = inline253 + inline254
    println__T_string(inline255)
    var jp192 bool
    jp192 = false
    var jp194 bool
    jp194 = true
    var not_result__11 bool = !false
    var t205 bool = !jp192
    var jp198 bool
    if t205 {
        var t206 int32 = prod__7 * base__4
        var t207 int32 = sum__5 + t206
        var t208 int32 = prod__7 / 2
        var mtmp178 int32 = t207 - t208
        switch mtmp178 {
        case 0:
            jp198 = false
        default:
            jp198 = true
        }
    } else {
        jp198 = false
    }
    var jp196 bool
    if jp198 {
        jp196 = true
    } else {
        var t199 int32 = diff__6 - quot__8
        var t200 int32 = t199 + base__4
        var t201 int32 = sum__5 / 2
        var mtmp179 int32 = t200 - t201
        var jp203 bool
        switch mtmp179 {
        case 0:
            jp203 = false
        default:
            jp203 = true
        }
        var t204 bool = !jp203
        jp196 = t204
    }
    var inline248 string = "and="
    var inline249 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp192)
    var inline250 string = inline248 + inline249
    println__T_string(inline250)
    var inline243 string = "or="
    var inline244 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp194)
    var inline245 string = inline243 + inline244
    println__T_string(inline245)
    var inline238 string = "not="
    var inline239 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline240 string = inline238 + inline239
    println__T_string(inline240)
    var inline233 string = "mixed="
    var inline234 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp196)
    var inline235 string = inline233 + inline234
    println__T_string(inline235)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t212 string
    t212 = value__1
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t216 string = _goml_runtime_core_int32_to_string(self__33)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t219 string = _goml_runtime_core_bool_to_string(self__64)
    return t219
}

func main() {
    main0()
}
