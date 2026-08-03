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
    var inline273 string = "sum="
    var inline274 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline275 string = inline273 + inline274
    println__T_string(inline275)
    var inline268 string = "diff="
    var inline269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline270 string = inline268 + inline269
    println__T_string(inline270)
    var inline263 string = "prod="
    var inline264 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline265 string = inline263 + inline264
    println__T_string(inline265)
    var inline258 string = "quot="
    var inline259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline260 string = inline258 + inline259
    println__T_string(inline260)
    var jp197 bool
    jp197 = false
    var jp199 bool
    jp199 = true
    var not_result__11 bool = !false
    var t210 bool = !jp197
    var jp203 bool
    if t210 {
        var t211 int32 = prod__7 * base__4
        var t212 int32 = sum__5 + t211
        var t213 int32 = prod__7 / 2
        var mtmp183 int32 = t212 - t213
        switch mtmp183 {
        case 0:
            jp203 = false
        default:
            jp203 = true
        }
    } else {
        jp203 = false
    }
    var jp201 bool
    if jp203 {
        jp201 = true
    } else {
        var t204 int32 = diff__6 - quot__8
        var t205 int32 = t204 + base__4
        var t206 int32 = sum__5 / 2
        var mtmp184 int32 = t205 - t206
        var jp208 bool
        switch mtmp184 {
        case 0:
            jp208 = false
        default:
            jp208 = true
        }
        var t209 bool = !jp208
        jp201 = t209
    }
    var inline253 string = "and="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp197)
    var inline255 string = inline253 + inline254
    println__T_string(inline255)
    var inline248 string = "or="
    var inline249 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp199)
    var inline250 string = inline248 + inline249
    println__T_string(inline250)
    var inline243 string = "not="
    var inline244 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline245 string = inline243 + inline244
    println__T_string(inline245)
    var inline238 string = "mixed="
    var inline239 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp201)
    var inline240 string = inline238 + inline239
    println__T_string(inline240)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t217 string
    t217 = value__31
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__35)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t224 string = _goml_runtime_core_bool_to_string(self__66)
    return t224
}

func main() {
    main0()
}
