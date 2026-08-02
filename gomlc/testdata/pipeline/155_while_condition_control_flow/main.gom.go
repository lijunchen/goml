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
    Loop_loop202:
    for {
        var t208 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
        var t209 bool = t208 < 3
        var jp204 bool
        if t209 {
            jp204 = true
        } else {
            jp204 = false
        }
        if jp204 {
            var t205 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            println__T_int(t205)
            var t206 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__0)
            var t207 int = t206 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__0, t207)
            continue
        } else {
            break Loop_loop202
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop185:
    for {
        var t193 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
        var t194 bool = t193 < 4
        var jp187 bool
        if t194 {
            var t197 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t198 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t197, 1)
            var jp196 bool
            if t198 {
                jp196 = true
            } else {
                var t199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
                var t200 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t199, 3)
                var t201 bool = !t200
                jp196 = t201
            }
            jp187 = jp196
        } else {
            jp187 = false
        }
        if jp187 {
            var t188 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
            var t189 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t190 int = t188 + t189
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(total__2, t190)
            var t191 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(j__1)
            var t192 int = t191 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(j__1, t192)
            continue
        } else {
            break Loop_loop185
        }
    }
    var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(total__2)
    println__T_int(t170)
    var k__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var sum__4 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop173:
    for {
        var mtmp162 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
        var jp175 bool
        switch mtmp162 {
        case 0:
            jp175 = true
        case 1:
            var t183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t184 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t183, 0)
            var jp182 bool
            if t184 {
                jp182 = true
            } else {
                jp182 = false
            }
            jp175 = jp182
        case 2:
            jp175 = true
        default:
            jp175 = false
        }
        if jp175 {
            var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
            var t177 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t178 int = t176 + t177
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(sum__4, t178)
            var t179 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(k__3)
            var t180 int = t179 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(k__3, t180)
            continue
        } else {
            break Loop_loop173
        }
    }
    var t172 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(sum__4)
    println__T_int(t172)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv211 *ref_int_x
    var t212 *ref_int_x = ref__Ref_3int(value__207)
    retv211 = t212
    return retv211
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv214 int
    var t215 int = ref_get__Ref_3int(self__208)
    retv214 = t215
    return retv214
}

func println__T_int(value__1 int) struct{} {
    var t217 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv222 bool
    var t223 bool = self__59 == other__60
    retv222 = t223
    return retv222
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv225 string
    var t226 string = _goml_runtime_core_int_to_string(self__40)
    retv225 = t226
    return retv225
}

func main() {
    main0()
}
