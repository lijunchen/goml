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
    var inline251 string = "sum="
    var inline252 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline253 string = inline251 + inline252
    println__T_string(inline253)
    var inline246 string = "diff="
    var inline247 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline248 string = inline246 + inline247
    println__T_string(inline248)
    var inline241 string = "prod="
    var inline242 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline243 string = inline241 + inline242
    println__T_string(inline243)
    var inline236 string = "quot="
    var inline237 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline238 string = inline236 + inline237
    println__T_string(inline238)
    var jp175 bool
    jp175 = false
    var jp177 bool
    jp177 = true
    var not_result__11 bool = !false
    var t188 bool = !jp175
    var jp181 bool
    if t188 {
        var t189 int32 = prod__7 * base__4
        var t190 int32 = sum__5 + t189
        var t191 int32 = prod__7 / 2
        var mtmp161 int32 = t190 - t191
        switch mtmp161 {
        case 0:
            jp181 = false
        default:
            jp181 = true
        }
    } else {
        jp181 = false
    }
    var jp179 bool
    if jp181 {
        jp179 = true
    } else {
        var t182 int32 = diff__6 - quot__8
        var t183 int32 = t182 + base__4
        var t184 int32 = sum__5 / 2
        var mtmp162 int32 = t183 - t184
        var jp186 bool
        switch mtmp162 {
        case 0:
            jp186 = false
        default:
            jp186 = true
        }
        var t187 bool = !jp186
        jp179 = t187
    }
    var inline231 string = "and="
    var inline232 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp175)
    var inline233 string = inline231 + inline232
    println__T_string(inline233)
    var inline226 string = "or="
    var inline227 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp177)
    var inline228 string = inline226 + inline227
    println__T_string(inline228)
    var inline221 string = "not="
    var inline222 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline223 string = inline221 + inline222
    println__T_string(inline223)
    var inline216 string = "mixed="
    var inline217 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp179)
    var inline218 string = inline216 + inline217
    println__T_string(inline218)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string
    t195 = value__1
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t199 string = _goml_runtime_core_int32_to_string(self__6)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t202 string = _goml_runtime_core_bool_to_string(self__37)
    return t202
}

func main() {
    main0()
}
