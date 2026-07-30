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
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t82 string = label__0 + t81
    println__T_string(t82)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t85 string = label__2 + t84
    println__T_string(t85)
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
    var jp88 bool
    if true {
        jp88 = false
    } else {
        jp88 = false
    }
    var and_result__9 bool = jp88
    var jp90 bool
    if true {
        jp90 = true
    } else {
        jp90 = false
    }
    var or_result__10 bool = jp90
    var not_result__11 bool = !false
    var t101 bool = !and_result__9
    var jp94 bool
    if t101 {
        var t102 int32 = prod__7 * base__4
        var t103 int32 = sum__5 + t102
        var t104 int32 = prod__7 / 2
        var mtmp74 int32 = t103 - t104
        var jp106 bool
        switch mtmp74 {
        case 0:
            jp106 = false
        default:
            jp106 = true
        }
        jp94 = jp106
    } else {
        jp94 = false
    }
    var jp92 bool
    if jp94 {
        jp92 = true
    } else {
        var t95 int32 = diff__6 - quot__8
        var t96 int32 = t95 + base__4
        var t97 int32 = sum__5 / 2
        var mtmp75 int32 = t96 - t97
        var jp99 bool
        switch mtmp75 {
        case 0:
            jp99 = false
        default:
            jp99 = true
        }
        var t100 bool = !jp99
        jp92 = t100
    }
    var mixed__12 bool = jp92
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv111 string
    var t112 string = _goml_runtime_core_int32_to_string(self__6)
    retv111 = t112
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv114 string
    var t115 string = _goml_runtime_core_bool_to_string(self__37)
    retv114 = t115
    return retv114
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv117 string
    retv117 = self__38
    return retv117
}

func main() {
    main0()
}
