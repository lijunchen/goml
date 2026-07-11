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
    var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t36 string = label__0 + t35
    println__T_string(t36)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t38 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t39 string = label__2 + t38
    println__T_string(t39)
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
    var jp42 bool
    if true {
        jp42 = false
    } else {
        jp42 = false
    }
    var and_result__9 bool = jp42
    var jp44 bool
    if true {
        jp44 = true
    } else {
        jp44 = false
    }
    var or_result__10 bool = jp44
    var not_result__11 bool = !false
    var t55 bool = !and_result__9
    var jp48 bool
    if t55 {
        var t56 int32 = prod__7 * base__4
        var t57 int32 = sum__5 + t56
        var t58 int32 = prod__7 / 2
        var mtmp28 int32 = t57 - t58
        var jp60 bool
        switch mtmp28 {
        case 0:
            jp60 = false
        default:
            jp60 = true
        }
        jp48 = jp60
    } else {
        jp48 = false
    }
    var jp46 bool
    if jp48 {
        jp46 = true
    } else {
        var t49 int32 = diff__6 - quot__8
        var t50 int32 = t49 + base__4
        var t51 int32 = sum__5 / 2
        var mtmp29 int32 = t50 - t51
        var jp53 bool
        switch mtmp29 {
        case 0:
            jp53 = false
        default:
            jp53 = true
        }
        var t54 bool = !jp53
        jp46 = t54
    }
    var mixed__12 bool = jp46
    show_bool("and=", and_result__9)
    show_bool("or=", or_result__10)
    show_bool("not=", not_result__11)
    show_bool("mixed=", mixed__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv65 string
    var t66 string = _goml_runtime_core_int32_to_string(self__2)
    retv65 = t66
    return retv65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv68 string
    var t69 string = _goml_runtime_core_bool_to_string(self__8)
    retv68 = t69
    return retv68
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv71 string
    retv71 = self__9
    return retv71
}

func main() {
    main0()
}
