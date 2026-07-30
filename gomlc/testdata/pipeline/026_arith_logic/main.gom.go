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
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t122 string = label__0 + t121
    println__T_string(t122)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t124 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t125 string = label__2 + t124
    println__T_string(t125)
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
    var jp128 bool
    if true {
        jp128 = false
    } else {
        jp128 = false
    }
    var and_result__9 bool = jp128
    var jp130 bool
    if true {
        jp130 = true
    } else {
        jp130 = false
    }
    var or_result__10 bool = jp130
    var not_result__11 bool = !false
    var t141 bool = !and_result__9
    var jp134 bool
    if t141 {
        var t142 int32 = prod__7 * base__4
        var t143 int32 = sum__5 + t142
        var t144 int32 = prod__7 / 2
        var mtmp114 int32 = t143 - t144
        var jp146 bool
        switch mtmp114 {
        case 0:
            jp146 = false
        default:
            jp146 = true
        }
        jp134 = jp146
    } else {
        jp134 = false
    }
    var jp132 bool
    if jp134 {
        jp132 = true
    } else {
        var t135 int32 = diff__6 - quot__8
        var t136 int32 = t135 + base__4
        var t137 int32 = sum__5 / 2
        var mtmp115 int32 = t136 - t137
        var jp139 bool
        switch mtmp115 {
        case 0:
            jp139 = false
        default:
            jp139 = true
        }
        var t140 bool = !jp139
        jp132 = t140
    }
    var mixed__12 bool = jp132
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv151 string
    var t152 string = _goml_runtime_core_int32_to_string(self__6)
    retv151 = t152
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv154 string
    var t155 string = _goml_runtime_core_bool_to_string(self__37)
    retv154 = t155
    return retv154
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv157 string
    retv157 = self__38
    return retv157
}

func main() {
    main0()
}
