package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() struct{} {
    if true {
        _goml_runtime_core_string_println("if")
    } else {
        _goml_runtime_core_string_println("else")
    }
    var mtmp65 int = 1
    switch mtmp65 {
    case 1:
        _goml_runtime_core_string_println("match")
    default:
    }
    var index__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop86:
    for {
        var t87 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
        var t88 bool = t87 < 2
        if t88 {
            var t89 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t90 string = _goml_m_inherent_i_int_i_int_i_to__string(t89)
            _goml_runtime_core_string_println(t90)
            var t91 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__0)
            var t92 int = t91 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__0, t92)
            continue
        } else {
            break Loop_loop86
        }
    }
    var values__1 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(values__1, "for")
    var for_source70 *_goml_vec_string = values__1
    var for_limit71 int = vec_len__Vec_6string(for_source70)
    var for_index72 int = 0
    Loop_loop82:
    for {
        var t83 bool = for_index72 < for_limit71
        if t83 {
            var for_item73 string = vec_get__Vec_6string(for_source70, for_index72)
            var t84 int = for_index72 + 1
            for_index72 = t84
            var value__2 string = for_item73
            _goml_runtime_core_string_println(value__2)
            continue
        } else {
            break Loop_loop82
        }
    }
    _goml_runtime_core_string_println("done")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv98 *ref_int_x
    var t99 *ref_int_x = ref__Ref_3int(value__209)
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv101 int
    var t102 int = ref_get__Ref_3int(self__210)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int_to_string(self__5)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv109 *_goml_vec_string
    var t110 *_goml_vec_string = vec_new__Vec_6string()
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func main() {
    main0()
}
