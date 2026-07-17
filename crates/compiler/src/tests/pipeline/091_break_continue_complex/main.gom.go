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
    Loop_loop88:
    for {
        var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
        var t90 bool = t89 <= 100
        if t90 {
            var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
            var t98 bool = t97 == 50
            if t98 {
                break Loop_loop88
            } else {
                var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
                var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t94 int32 = t92 + t93
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__0, t94)
                var t95 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__1)
                var t96 int32 = t95 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__1, t96)
                continue
            }
        } else {
            break Loop_loop88
        }
    }
    print__T_string("sum up to break: ")
    var t75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__0)
    println__T_int32(t75)
    var even_sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var j__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    Loop_loop78:
    for {
        var t79 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
        var t80 bool = t79 <= 10
        if t80 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(j__3)
            var t81 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(j__3, t81)
            var t83 int32 = cur__4 / 2
            var t84 int32 = t83 * 2
            var t85 bool = cur__4 == t84
            if t85 {
                var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
                var t87 int32 = t86 + cur__4
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(even_sum__2, t87)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop78
        }
    }
    print__T_string("even sum: ")
    var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(even_sum__2)
    println__T_int32(t77)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv100 *ref_int32_x
    var t101 *ref_int32_x = ref__Ref_5int32(value__204)
    retv100 = t101
    return retv100
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv103 int32
    var t104 int32 = ref_get__Ref_5int32(self__205)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t108)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv114 string
    retv114 = self__37
    return retv114
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_int32_to_string(self__41)
    retv116 = t117
    return retv116
}

func main() {
    main0()
}
