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

func show_int(label__0 string, value__1 int32) struct{} {
    var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t166 string = label__0 + t165
    println__T_string(t166)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t169 string = label__2 + t168
    println__T_string(t169)
    return struct{}{}
}

func main0() struct{} {
    var base__4 int32 = 10
    var sum__5 int32 = base__4 + 5
    var diff__6 int32 = sum__5 - 3
    var prod__7 int32 = diff__6 * 2
    var quot__8 int32 = prod__7 / 4
    show_int("sum=", sum__5)
    show_int("diff=", diff__6)
    show_int("prod=", prod__7)
    show_int("quot=", quot__8)
    var jp172 bool
    if true {
        jp172 = false
    } else {
        jp172 = false
    }
    var and_result__9 bool = jp172
    var jp174 bool
    if true {
        jp174 = true
    } else {
        jp174 = false
    }
    var or_result__10 bool = jp174
    var not_result__11 bool = !false
    var t185 bool = !and_result__9
    var jp178 bool
    if t185 {
        var t186 int32 = prod__7 * base__4
        var t187 int32 = sum__5 + t186
        var t188 int32 = prod__7 / 2
        var mtmp158 int32 = t187 - t188
        var jp190 bool
        switch mtmp158 {
        case 0:
            jp190 = false
        default:
            jp190 = true
        }
        jp178 = jp190
    } else {
        jp178 = false
    }
    var jp176 bool
    if jp178 {
        jp176 = true
    } else {
        var t179 int32 = diff__6 - quot__8
        var t180 int32 = t179 + base__4
        var t181 int32 = sum__5 / 2
        var mtmp159 int32 = t180 - t181
        var jp183 bool
        switch mtmp159 {
        case 0:
            jp183 = false
        default:
            jp183 = true
        }
        var t184 bool = !jp183
        jp176 = t184
    }
    var mixed__12 bool = jp176
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv195 string
    var t196 string = _goml_runtime_core_int32_to_string(self__6)
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv198 string
    var t199 string = _goml_runtime_core_bool_to_string(self__37)
    retv198 = t199
    return retv198
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv201 string
    retv201 = self__38
    return retv201
}

func main() {
    main0()
}
