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
    var i__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop199:
    for {
        var t205 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t206 bool = t205 < 3
        var jp201 bool
        if t206 {
            jp201 = true
        } else {
            jp201 = false
        }
        if jp201 {
            var t202 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            println__T_int(t202)
            var t203 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t204 int = t203 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t204)
            continue
        } else {
            break Loop_loop199
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop182:
    for {
        var t190 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
        var t191 bool = t190 < 4
        var jp184 bool
        if t191 {
            var t194 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t195 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t194, 1)
            var jp193 bool
            if t195 {
                jp193 = true
            } else {
                var t196 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
                var t197 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t196, 3)
                var t198 bool = !t197
                jp193 = t198
            }
            jp184 = jp193
        } else {
            jp184 = false
        }
        if jp184 {
            var t185 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
            var t186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t187 int = t185 + t186
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__2, t187)
            var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t189 int = t188 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__1, t189)
            continue
        } else {
            break Loop_loop182
        }
    }
    var t167 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t167)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop170:
    for {
        var mtmp159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
        var jp172 bool
        switch mtmp159 {
        case 0:
            jp172 = true
        case 1:
            var t180 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t181 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t180, 0)
            var jp179 bool
            if t181 {
                jp179 = true
            } else {
                jp179 = false
            }
            jp172 = jp179
        case 2:
            jp172 = true
        default:
            jp172 = false
        }
        if jp172 {
            var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t175 int = t173 + t174
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__4, t175)
            var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t177 int = t176 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(k__3, t177)
            continue
        } else {
            break Loop_loop170
        }
    }
    var t169 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t169)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv208 *ref_int_x
    var t209 *ref_int_x = ref__Ref_3int(value__207)
    retv208 = t209
    return retv208
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv211 int
    var t212 int = ref_get__Ref_3int(self__208)
    retv211 = t212
    return retv211
}

func println__T_int(value__1 int) struct{} {
    var t214 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t214)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv219 bool
    var t220 bool = self__59 == other__60
    retv219 = t220
    return retv219
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv222 string
    var t223 string = _goml_runtime_core_int_to_string(self__40)
    retv222 = t223
    return retv222
}

func main() {
    main0()
}
