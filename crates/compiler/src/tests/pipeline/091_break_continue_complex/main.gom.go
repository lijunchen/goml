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
    var i__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    Loop_loop85:
    for {
        var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t87 bool = t86 <= 100
        if t87 {
            var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t95 bool = t94 == 50
            if t95 {
                break Loop_loop85
            } else {
                var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
                var t90 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t91 int32 = t89 + t90
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__0, t91)
                var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t93 int32 = t92 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t93)
                continue
            }
        } else {
            break Loop_loop85
        }
    }
    print__T_string("sum up to break: ")
    var t72 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
    println__T_int32(t72)
    var even_sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    Loop_loop75:
    for {
        var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
        var t77 bool = t76 <= 10
        if t77 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t78 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t78)
            var t80 int32 = cur__4 / 2
            var t81 int32 = t80 * 2
            var t82 bool = cur__4 == t81
            if t82 {
                var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
                var t84 int32 = t83 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(even_sum__2, t84)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop75
        }
    }
    print__T_string("even sum: ")
    var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
    println__T_int32(t74)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv97 *ref_int32_x
    var t98 *ref_int32_x = ref__Ref_5int32(value__200)
    retv97 = t98
    return retv97
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv100 int32
    var t101 int32 = ref_get__Ref_5int32(self__201)
    retv100 = t101
    return retv100
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t105)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv111 string
    retv111 = self__34
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv113 string
    var t114 string = _goml_runtime_core_int32_to_string(self__38)
    retv113 = t114
    return retv113
}

func main() {
    main0()
}
