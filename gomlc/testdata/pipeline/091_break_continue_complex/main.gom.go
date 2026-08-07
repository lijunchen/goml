package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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
    var sum__0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var i__1 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    Loop_loop199:
    for {
        var t200 int
        var inline251 int = ref_get__Ref_3int(i__1)
        t200 = inline251
        var t201 bool = t200 <= 100
        if t201 {
            var t208 int
            var inline249 int = ref_get__Ref_3int(i__1)
            t208 = inline249
            var t209 bool
            var inline246 int = 50
            var inline247 bool = t208 == inline246
            t209 = inline247
            if t209 {
                break Loop_loop199
            } else {
                var t203 int
                var inline244 int = ref_get__Ref_3int(sum__0)
                t203 = inline244
                var t204 int
                var inline242 int = ref_get__Ref_3int(i__1)
                t204 = inline242
                var t205 int = t203 + t204
                ref_set__Ref_3int(sum__0, t205)
                var t206 int
                var inline238 int = ref_get__Ref_3int(i__1)
                t206 = inline238
                var t207 int = t206 + 1
                ref_set__Ref_3int(i__1, t207)
                continue
            }
        } else {
            break Loop_loop199
        }
    }
    var inline285 string = "sum up to break: "
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
    _goml_runtime_core_string_print(inline286)
    var t186 int
    var inline283 int = ref_get__Ref_3int(sum__0)
    t186 = inline283
    var inline280 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t186)
    _goml_runtime_core_string_println(inline280)
    var even_sum__2 *ref_int_x
    var inline277 int = 0
    var inline278 *ref_int_x = ref__Ref_3int(inline277)
    even_sum__2 = inline278
    var j__3 *ref_int_x
    var inline274 int = 1
    var inline275 *ref_int_x = ref__Ref_3int(inline274)
    j__3 = inline275
    Loop_loop189:
    for {
        var t190 int
        var inline263 int = ref_get__Ref_3int(j__3)
        t190 = inline263
        var t191 bool = t190 <= 10
        if t191 {
            var cur__4 int
            var inline261 int = ref_get__Ref_3int(j__3)
            cur__4 = inline261
            var t192 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t192)
            var t194 int = cur__4 / 2
            var t195 int = t194 * 2
            var t196 bool
            var inline257 bool = cur__4 == t195
            t196 = inline257
            if t196 {
                var t197 int
                var inline255 int = ref_get__Ref_3int(even_sum__2)
                t197 = inline255
                var t198 int = t197 + cur__4
                ref_set__Ref_3int(even_sum__2, t198)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop189
        }
    }
    var inline270 string = "even sum: "
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline270)
    _goml_runtime_core_string_print(inline271)
    var t188 int
    var inline268 int = ref_get__Ref_3int(even_sum__2)
    t188 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
    _goml_runtime_core_string_println(inline265)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t212 *ref_int_x = ref__Ref_3int(value__257)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t231 string = _goml_runtime_core_int_to_string(self__69)
    return t231
}

func main() {
    main0()
}
