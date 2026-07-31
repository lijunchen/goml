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
    var retv154 int32
    var value__0 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(value__0, 2)
    var t155 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(value__0)
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv157 *ref_int32_x
    var t158 *ref_int32_x = ref__Ref_5int32(value__207)
    retv157 = t158
    return retv157
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv162 int32
    var t163 int32 = ref_get__Ref_5int32(self__208)
    retv162 = t163
    return retv162
}

func main() {
    main0()
}
