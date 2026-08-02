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
        var t208 int
        var inline240 int = ref_get__Ref_3int(i__0)
        t208 = inline240
        var t209 bool = t208 < 3
        var jp204 bool
        if t209 {
            jp204 = true
        } else {
            jp204 = false
        }
        if jp204 {
            var t205 int
            var inline238 int = ref_get__Ref_3int(i__0)
            t205 = inline238
            var inline235 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t205)
            _goml_runtime_core_string_println(inline235)
            var t206 int
            var inline233 int = ref_get__Ref_3int(i__0)
            t206 = inline233
            var t207 int = t206 + 1
            ref_set__Ref_3int(i__0, t207)
            continue
        } else {
            break Loop_loop202
        }
    }
    var j__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var total__2 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop185:
    for {
        var t193 int
        var inline262 int = ref_get__Ref_3int(j__1)
        t193 = inline262
        var t194 bool = t193 < 4
        var jp187 bool
        if t194 {
            var t197 int
            var inline250 int = ref_get__Ref_3int(j__1)
            t197 = inline250
            var t198 bool
            var inline247 int = 1
            var inline248 bool = t197 == inline247
            t198 = inline248
            if t198 {
                jp187 = true
            } else {
                var t199 int
                var inline245 int = ref_get__Ref_3int(j__1)
                t199 = inline245
                var t200 bool
                var inline242 int = 3
                var inline243 bool = t199 == inline242
                t200 = inline243
                var t201 bool = !t200
                jp187 = t201
            }
        } else {
            jp187 = false
        }
        if jp187 {
            var t188 int
            var inline260 int = ref_get__Ref_3int(total__2)
            t188 = inline260
            var t189 int
            var inline258 int = ref_get__Ref_3int(j__1)
            t189 = inline258
            var t190 int = t188 + t189
            ref_set__Ref_3int(total__2, t190)
            var t191 int
            var inline254 int = ref_get__Ref_3int(j__1)
            t191 = inline254
            var t192 int = t191 + 1
            ref_set__Ref_3int(j__1, t192)
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
        var mtmp162 int
        var inline279 int = ref_get__Ref_3int(k__3)
        mtmp162 = inline279
        var jp175 bool
        switch mtmp162 {
        case 0:
            jp175 = true
        case 1:
            var t183 int
            var inline267 int = ref_get__Ref_3int(sum__4)
            t183 = inline267
            var t184 bool
            var inline264 int = 0
            var inline265 bool = t183 == inline264
            t184 = inline265
            if t184 {
                jp175 = true
            } else {
                jp175 = false
            }
        case 2:
            jp175 = true
        default:
            jp175 = false
        }
        if jp175 {
            var t176 int
            var inline277 int = ref_get__Ref_3int(sum__4)
            t176 = inline277
            var t177 int
            var inline275 int = ref_get__Ref_3int(k__3)
            t177 = inline275
            var t178 int = t176 + t177
            ref_set__Ref_3int(sum__4, t178)
            var t179 int
            var inline271 int = ref_get__Ref_3int(k__3)
            t179 = inline271
            var t180 int = t179 + 1
            ref_set__Ref_3int(k__3, t180)
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
    var t212 *ref_int_x = ref__Ref_3int(value__207)
    return t212
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t215 int = ref_get__Ref_3int(self__208)
    return t215
}

func println__T_int(value__1 int) struct{} {
    var t217 string
    var inline281 string = _goml_runtime_core_int_to_string(value__1)
    t217 = inline281
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t226 string = _goml_runtime_core_int_to_string(self__40)
    return t226
}

func main() {
    main0()
}
