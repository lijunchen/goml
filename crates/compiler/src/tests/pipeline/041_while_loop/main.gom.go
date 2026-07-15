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
    var retv33 int32
    var acc__1 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop36:
    for {
        var t37 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var t38 bool = t37 < limit__0
        if t38 {
            var current__3 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
            var t39 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
            var t40 int32 = t39 + current__3
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__1, t40)
            var t41 int32 = current__3 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t41)
            continue
        } else {
            break Loop_loop36
        }
    }
    var t35 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__1)
    retv33 = t35
    return retv33
}

func sum_even(limit__4 int32) int32 {
    var retv43 int32
    var acc__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__6 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var is_even__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop46:
    for {
        var t47 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
        var t48 bool = t47 < limit__4
        if t48 {
            var current__8 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__6)
            var t49 int32 = current__8 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__6, t49)
            var add_now__9 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(is_even__7)
            var t50 bool = !add_now__9
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(is_even__7, t50)
            if add_now__9 {
                var t52 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
                var t53 int32 = t52 + current__8
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__5, t53)
            } else {}
            continue
        } else {
            break Loop_loop46
        }
    }
    var t45 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__5)
    retv43 = t45
    return retv43
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv57 *ref_int32_x
    var t58 *ref_int32_x = ref__Ref_5int32(value__140)
    retv57 = t58
    return retv57
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv60 int32
    var t61 int32 = ref_get__Ref_5int32(self__141)
    retv60 = t61
    return retv60
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__140 bool) *ref_bool_x {
    var retv65 *ref_bool_x
    var t66 *ref_bool_x = ref__Ref_4bool(value__140)
    retv65 = t66
    return retv65
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__141 *ref_bool_x) bool {
    var retv68 bool
    var t69 bool = ref_get__Ref_4bool(self__141)
    retv68 = t69
    return retv68
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__142 *ref_bool_x, value__143 bool) struct{} {
    ref_set__Ref_4bool(self__142, value__143)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t73)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv79 string
    retv79 = self__9
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__13)
    retv81 = t82
    return retv81
}

func main() {
    main0()
}
