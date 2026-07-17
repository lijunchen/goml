package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    var sum__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop70:
    for {
        var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t72 bool = t71 < 20
        if t72 {
            var t73 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t74 int32 = t73 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t74)
            var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t80 bool = t79 > 5
            if t80 {
                break Loop_loop70
            } else {
                var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
                var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t78 int32 = t76 + t77
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__0, t78)
                continue
            }
        } else {
            break Loop_loop70
        }
    }
    print__T_string("sum: ")
    var t68 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
    println__T_int32(t68)
    print__T_string("i at break: ")
    var t69 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
    println__T_int32(t69)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv82 *ref_int32_x
    var t83 *ref_int32_x = ref__Ref_5int32(value__200)
    retv82 = t83
    return retv82
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv85 int32
    var t86 int32 = ref_get__Ref_5int32(self__201)
    retv85 = t86
    return retv85
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t90)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv96 string
    retv96 = self__34
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int32_to_string(self__38)
    retv98 = t99
    return retv98
}

func main() {
    main0()
}
