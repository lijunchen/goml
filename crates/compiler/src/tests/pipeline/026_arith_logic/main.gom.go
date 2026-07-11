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
    var t17 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t18 string = label__0 + t17
    println__T_string(t18)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t20 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t21 string = label__2 + t20
    println__T_string(t21)
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
    var jp24 bool
    if true {
        jp24 = false
    } else {
        jp24 = false
    }
    var and_result__9 bool = jp24
    var jp26 bool
    if true {
        jp26 = true
    } else {
        jp26 = false
    }
    var or_result__10 bool = jp26
    var not_result__11 bool = !false
    var t37 bool = !and_result__9
    var jp30 bool
    if t37 {
        var t38 int32 = prod__7 * base__4
        var t39 int32 = sum__5 + t38
        var t40 int32 = prod__7 / 2
        var mtmp10 int32 = t39 - t40
        var jp42 bool
        switch mtmp10 {
        case 0:
            jp42 = false
        default:
            jp42 = true
        }
        jp30 = jp42
    } else {
        jp30 = false
    }
    var jp28 bool
    if jp30 {
        jp28 = true
    } else {
        var t31 int32 = diff__6 - quot__8
        var t32 int32 = t31 + base__4
        var t33 int32 = sum__5 / 2
        var mtmp11 int32 = t32 - t33
        var jp35 bool
        switch mtmp11 {
        case 0:
            jp35 = false
        default:
            jp35 = true
        }
        var t36 bool = !jp35
        jp28 = t36
    }
    var mixed__12 bool = jp28
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t44 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t44)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv47 string
    var t48 string = _goml_runtime_core_int32_to_string(self__2)
    retv47 = t48
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv50 string
    var t51 string = _goml_runtime_core_bool_to_string(self__8)
    retv50 = t51
    return retv50
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv53 string
    retv53 = self__9
    return retv53
}

func main() {
    main0()
}
