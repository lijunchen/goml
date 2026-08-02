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
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t169 string = label__0 + t168
    println__T_string(t169)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t171 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t172 string = label__2 + t171
    println__T_string(t172)
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
    var jp175 bool
    if true {
        jp175 = false
    } else {
        jp175 = false
    }
    var and_result__9 bool = jp175
    var jp177 bool
    if true {
        jp177 = true
    } else {
        jp177 = false
    }
    var or_result__10 bool = jp177
    var not_result__11 bool = !false
    var t188 bool = !and_result__9
    var jp181 bool
    if t188 {
        var t189 int32 = prod__7 * base__4
        var t190 int32 = sum__5 + t189
        var t191 int32 = prod__7 / 2
        var mtmp161 int32 = t190 - t191
        var jp193 bool
        switch mtmp161 {
        case 0:
            jp193 = false
        default:
            jp193 = true
        }
        jp181 = jp193
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
    var mixed__12 bool = jp179
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv198 string
    var t199 string = _goml_runtime_core_int32_to_string(self__6)
    retv198 = t199
    return retv198
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv201 string
    var t202 string = _goml_runtime_core_bool_to_string(self__37)
    retv201 = t202
    return retv201
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv204 string
    retv204 = self__38
    return retv204
}

func main() {
    main0()
}
