package main

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

func main0() int32 {
    var retv66 int32
    var value__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(value__0, 2)
    var t67 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(value__0)
    retv66 = t67
    return retv66
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv69 *ref_int32_x
    var t70 *ref_int32_x = ref__Ref_5int32(value__209)
    retv69 = t70
    return retv69
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv74 int32
    var t75 int32 = ref_get__Ref_5int32(self__210)
    retv74 = t75
    return retv74
}

func main() {
    main0()
}
