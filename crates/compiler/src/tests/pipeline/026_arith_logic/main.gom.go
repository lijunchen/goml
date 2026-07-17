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
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t72 string = label__0 + t71
    println__T_string(t72)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t75 string = label__2 + t74
    println__T_string(t75)
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
    var jp78 bool
    if true {
        jp78 = false
    } else {
        jp78 = false
    }
    var and_result__9 bool = jp78
    var jp80 bool
    if true {
        jp80 = true
    } else {
        jp80 = false
    }
    var or_result__10 bool = jp80
    var not_result__11 bool = !false
    var t91 bool = !and_result__9
    var jp84 bool
    if t91 {
        var t92 int32 = prod__7 * base__4
        var t93 int32 = sum__5 + t92
        var t94 int32 = prod__7 / 2
        var mtmp64 int32 = t93 - t94
        var jp96 bool
        switch mtmp64 {
        case 0:
            jp96 = false
        default:
            jp96 = true
        }
        jp84 = jp96
    } else {
        jp84 = false
    }
    var jp82 bool
    if jp84 {
        jp82 = true
    } else {
        var t85 int32 = diff__6 - quot__8
        var t86 int32 = t85 + base__4
        var t87 int32 = sum__5 / 2
        var mtmp65 int32 = t86 - t87
        var jp89 bool
        switch mtmp65 {
        case 0:
            jp89 = false
        default:
            jp89 = true
        }
        var t90 bool = !jp89
        jp82 = t90
    }
    var mixed__12 bool = jp82
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__2)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv104 string
    var t105 string = _goml_runtime_core_bool_to_string(self__33)
    retv104 = t105
    return retv104
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv107 string
    retv107 = self__34
    return retv107
}

func main() {
    main0()
}
