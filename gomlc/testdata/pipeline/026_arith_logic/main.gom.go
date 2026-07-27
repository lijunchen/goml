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
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t78 string = label__0 + t77
    println__T_string(t78)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t81 string = label__2 + t80
    println__T_string(t81)
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
    var jp84 bool
    if true {
        jp84 = false
    } else {
        jp84 = false
    }
    var and_result__9 bool = jp84
    var jp86 bool
    if true {
        jp86 = true
    } else {
        jp86 = false
    }
    var or_result__10 bool = jp86
    var not_result__11 bool = !false
    var t97 bool = !and_result__9
    var jp90 bool
    if t97 {
        var t98 int32 = prod__7 * base__4
        var t99 int32 = sum__5 + t98
        var t100 int32 = prod__7 / 2
        var mtmp70 int32 = t99 - t100
        var jp102 bool
        switch mtmp70 {
        case 0:
            jp102 = false
        default:
            jp102 = true
        }
        jp90 = jp102
    } else {
        jp90 = false
    }
    var jp88 bool
    if jp90 {
        jp88 = true
    } else {
        var t91 int32 = diff__6 - quot__8
        var t92 int32 = t91 + base__4
        var t93 int32 = sum__5 / 2
        var mtmp71 int32 = t92 - t93
        var jp95 bool
        switch mtmp71 {
        case 0:
            jp95 = false
        default:
            jp95 = true
        }
        var t96 bool = !jp95
        jp88 = t96
    }
    var mixed__12 bool = jp88
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int32_to_string(self__6)
    retv107 = t108
    return retv107
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv110 string
    var t111 string = _goml_runtime_core_bool_to_string(self__37)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv113 string
    retv113 = self__38
    return retv113
}

func main() {
    main0()
}
