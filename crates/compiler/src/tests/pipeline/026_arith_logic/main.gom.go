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
    var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t21 string = label__0 + t20
    println__T_string(t21)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t23 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t24 string = label__2 + t23
    println__T_string(t24)
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
    var jp27 bool
    if true {
        jp27 = false
    } else {
        jp27 = false
    }
    var and_result__9 bool = jp27
    var jp29 bool
    if true {
        jp29 = true
    } else {
        jp29 = false
    }
    var or_result__10 bool = jp29
    var not_result__11 bool = !false
    var t40 bool = !and_result__9
    var jp33 bool
    if t40 {
        var t41 int32 = prod__7 * base__4
        var t42 int32 = sum__5 + t41
        var t43 int32 = prod__7 / 2
        var mtmp13 int32 = t42 - t43
        var jp45 bool
        switch mtmp13 {
        case 0:
            jp45 = false
        default:
            jp45 = true
        }
        jp33 = jp45
    } else {
        jp33 = false
    }
    var jp31 bool
    if jp33 {
        jp31 = true
    } else {
        var t34 int32 = diff__6 - quot__8
        var t35 int32 = t34 + base__4
        var t36 int32 = sum__5 / 2
        var mtmp14 int32 = t35 - t36
        var jp38 bool
        switch mtmp14 {
        case 0:
            jp38 = false
        default:
            jp38 = true
        }
        var t39 bool = !jp38
        jp31 = t39
    }
    var mixed__12 bool = jp31
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv50 string
    var t51 string = _goml_runtime_core_int32_to_string(self__2)
    retv50 = t51
    return retv50
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv53 string
    var t54 string = _goml_runtime_core_bool_to_string(self__8)
    retv53 = t54
    return retv53
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv56 string
    retv56 = self__9
    return retv56
}

func main() {
    main0()
}
