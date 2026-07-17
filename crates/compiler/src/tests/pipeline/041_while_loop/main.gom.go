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
    var retv72 int32
    var acc__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop75:
    for {
        var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var t77 bool = t76 < limit__0
        if t77 {
            var current__3 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
            var t78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
            var t79 int32 = t78 + current__3
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__1, t79)
            var t80 int32 = current__3 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t80)
            continue
        } else {
            break Loop_loop75
        }
    }
    var t74 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
    retv72 = t74
    return retv72
}

func sum_even(limit__4 int32) int32 {
    var retv82 int32
    var acc__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__6 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var is_even__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop85:
    for {
        var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
        var t87 bool = t86 < limit__4
        if t87 {
            var current__8 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
            var t88 int32 = current__8 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__6, t88)
            var add_now__9 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(is_even__7)
            var t89 bool = !add_now__9
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(is_even__7, t89)
            if add_now__9 {
                var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
                var t92 int32 = t91 + current__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__5, t92)
            } else {}
            continue
        } else {
            break Loop_loop85
        }
    }
    var t84 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
    retv82 = t84
    return retv82
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv96 *ref_int32_x
    var t97 *ref_int32_x = ref__Ref_5int32(value__204)
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv99 int32
    var t100 int32 = ref_get__Ref_5int32(self__205)
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv104 *ref_bool_x
    var t105 *ref_bool_x = ref__Ref_4bool(value__204)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv107 bool
    var t108 bool = ref_get__Ref_4bool(self__205)
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t112)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv118 string
    retv118 = self__37
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__41)
    retv120 = t121
    return retv120
}

func main() {
    main0()
}
