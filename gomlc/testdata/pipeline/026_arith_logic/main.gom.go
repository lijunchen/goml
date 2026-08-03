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
    var inline232 string = "sum="
    var inline233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline234 string = inline232 + inline233
    println__T_string(inline234)
    var inline227 string = "diff="
    var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline229 string = inline227 + inline228
    println__T_string(inline229)
    var inline222 string = "prod="
    var inline223 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline224 string = inline222 + inline223
    println__T_string(inline224)
    var inline217 string = "quot="
    var inline218 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline219 string = inline217 + inline218
    println__T_string(inline219)
    var jp156 bool
    jp156 = false
    var jp158 bool
    jp158 = true
    var not_result__11 bool = !false
    var t169 bool = !jp156
    var jp162 bool
    if t169 {
        var t170 int32 = prod__7 * base__4
        var t171 int32 = sum__5 + t170
        var t172 int32 = prod__7 / 2
        var mtmp142 int32 = t171 - t172
        switch mtmp142 {
        case 0:
            jp162 = false
        default:
            jp162 = true
        }
    } else {
        jp162 = false
    }
    var jp160 bool
    if jp162 {
        jp160 = true
    } else {
        var t163 int32 = diff__6 - quot__8
        var t164 int32 = t163 + base__4
        var t165 int32 = sum__5 / 2
        var mtmp143 int32 = t164 - t165
        var jp167 bool
        switch mtmp143 {
        case 0:
            jp167 = false
        default:
            jp167 = true
        }
        var t168 bool = !jp167
        jp160 = t168
    }
    var inline212 string = "and="
    var inline213 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp156)
    var inline214 string = inline212 + inline213
    println__T_string(inline214)
    var inline207 string = "or="
    var inline208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp158)
    var inline209 string = inline207 + inline208
    println__T_string(inline209)
    var inline202 string = "not="
    var inline203 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline204 string = inline202 + inline203
    println__T_string(inline204)
    var inline197 string = "mixed="
    var inline198 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp160)
    var inline199 string = inline197 + inline198
    println__T_string(inline199)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t176 string
    t176 = value__31
    _goml_runtime_core_string_println(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t180 string = _goml_runtime_core_int32_to_string(self__35)
    return t180
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t183 string = _goml_runtime_core_bool_to_string(self__66)
    return t183
}

func main() {
    main0()
}
