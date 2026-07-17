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
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t75 string = label__0 + t74
    println__T_string(t75)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t78 string = label__2 + t77
    println__T_string(t78)
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
    var jp81 bool
    if true {
        jp81 = false
    } else {
        jp81 = false
    }
    var and_result__9 bool = jp81
    var jp83 bool
    if true {
        jp83 = true
    } else {
        jp83 = false
    }
    var or_result__10 bool = jp83
    var not_result__11 bool = !false
    var t94 bool = !and_result__9
    var jp87 bool
    if t94 {
        var t95 int32 = prod__7 * base__4
        var t96 int32 = sum__5 + t95
        var t97 int32 = prod__7 / 2
        var mtmp67 int32 = t96 - t97
        var jp99 bool
        switch mtmp67 {
        case 0:
            jp99 = false
        default:
            jp99 = true
        }
        jp87 = jp99
    } else {
        jp87 = false
    }
    var jp85 bool
    if jp87 {
        jp85 = true
    } else {
        var t88 int32 = diff__6 - quot__8
        var t89 int32 = t88 + base__4
        var t90 int32 = sum__5 / 2
        var mtmp68 int32 = t89 - t90
        var jp92 bool
        switch mtmp68 {
        case 0:
            jp92 = false
        default:
            jp92 = true
        }
        var t93 bool = !jp92
        jp85 = t93
    }
    var mixed__12 bool = jp85
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int32_to_string(self__5)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv107 string
    var t108 string = _goml_runtime_core_bool_to_string(self__36)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv110 string
    retv110 = self__37
    return retv110
}

func main() {
    main0()
}
