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

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

func sum_to(limit__0 int32) int32 {
    var retv69 int32
    var acc__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop72:
    for {
        var t73 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var t74 bool = t73 < limit__0
        if t74 {
            var current__3 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
            var t75 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
            var t76 int32 = t75 + current__3
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__1, t76)
            var t77 int32 = current__3 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t77)
            continue
        } else {
            break Loop_loop72
        }
    }
    var t71 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
    retv69 = t71
    return retv69
}

func sum_even(limit__4 int32) int32 {
    var retv79 int32
    var acc__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__6 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var is_even__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop82:
    for {
        var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
        var t84 bool = t83 < limit__4
        if t84 {
            var current__8 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
            var t85 int32 = current__8 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__6, t85)
            var add_now__9 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(is_even__7)
            var t86 bool = !add_now__9
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(is_even__7, t86)
            if add_now__9 {
                var t88 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
                var t89 int32 = t88 + current__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__5, t89)
            } else {}
            continue
        } else {
            break Loop_loop82
        }
    }
    var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
    retv79 = t81
    return retv79
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    print__T_string("sum_to(5)=")
    println__T_int32(first__10)
    print__T_string("sum_even(6)=")
    println__T_int32(evens__11)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv93 *ref_int32_x
    var t94 *ref_int32_x = ref__Ref_5int32(value__201)
    retv93 = t94
    return retv93
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv96 int32
    var t97 int32 = ref_get__Ref_5int32(self__202)
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__201 bool) *ref_bool_x {
    var retv101 *ref_bool_x
    var t102 *ref_bool_x = ref__Ref_4bool(value__201)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__202 *ref_bool_x) bool {
    var retv104 bool
    var t105 bool = ref_get__Ref_4bool(self__202)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__203 *ref_bool_x, value__204 bool) struct{} {
    ref_set__Ref_4bool(self__203, value__204)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t109)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv115 string
    retv115 = self__34
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv117 string
    var t118 string = _goml_runtime_core_int32_to_string(self__38)
    retv117 = t118
    return retv117
}

func main() {
    main0()
}
