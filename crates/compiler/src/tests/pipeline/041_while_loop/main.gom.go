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
    var retv15 int32
    var acc__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop18:
    for {
        var t19 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var t20 bool = t19 < limit__0
        if t20 {
            var current__3 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
            var t21 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
            var t22 int32 = t21 + current__3
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__1, t22)
            var t23 int32 = current__3 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t23)
            continue
        } else {
            break Loop_loop18
        }
    }
    var t17 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
    retv15 = t17
    return retv15
}

func sum_even(limit__4 int32) int32 {
    var retv25 int32
    var acc__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__6 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var is_even__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop28:
    for {
        var t29 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
        var t30 bool = t29 < limit__4
        if t30 {
            var current__8 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
            var t31 int32 = current__8 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__6, t31)
            var add_now__9 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(is_even__7)
            var t32 bool = !add_now__9
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(is_even__7, t32)
            if add_now__9 {
                var t34 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
                var t35 int32 = t34 + current__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__5, t35)
            } else {}
            continue
        } else {
            break Loop_loop28
        }
    }
    var t27 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
    retv25 = t27
    return retv25
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv39 *ref_int32_x
    var t40 *ref_int32_x = ref__Ref_5int32(value__102)
    retv39 = t40
    return retv39
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv42 int32
    var t43 int32 = ref_get__Ref_5int32(self__103)
    retv42 = t43
    return retv42
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__102 bool) *ref_bool_x {
    var retv47 *ref_bool_x
    var t48 *ref_bool_x = ref__Ref_4bool(value__102)
    retv47 = t48
    return retv47
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__103 *ref_bool_x) bool {
    var retv50 bool
    var t51 bool = ref_get__Ref_4bool(self__103)
    retv50 = t51
    return retv50
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__104 *ref_bool_x, value__105 bool) struct{} {
    ref_set__Ref_4bool(self__104, value__105)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t55 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t55)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t58 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t58)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv61 string
    retv61 = self__9
    return retv61
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv63 string
    var t64 string = _goml_runtime_core_int32_to_string(self__13)
    retv63 = t64
    return retv63
}

func main() {
    main0()
}
