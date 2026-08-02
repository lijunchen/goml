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
    var sum__0 *ref_int_x
    var inline239 int = 0
    var inline240 *ref_int_x = ref__Ref_3int(inline239)
    sum__0 = inline240
    var i__1 *ref_int_x
    var inline236 int = 0
    var inline237 *ref_int_x = ref__Ref_3int(inline236)
    i__1 = inline237
    Loop_loop167:
    for {
        var t168 int
        var inline216 int = ref_get__Ref_3int(i__1)
        t168 = inline216
        var t169 bool = t168 < 20
        if t169 {
            var t170 int
            var inline214 int = ref_get__Ref_3int(i__1)
            t170 = inline214
            var t171 int = t170 + 1
            ref_set__Ref_3int(i__1, t171)
            var t176 int
            var inline210 int = ref_get__Ref_3int(i__1)
            t176 = inline210
            var t177 bool = t176 > 5
            if t177 {
                break Loop_loop167
            } else {
                var t173 int
                var inline208 int = ref_get__Ref_3int(sum__0)
                t173 = inline208
                var t174 int
                var inline206 int = ref_get__Ref_3int(i__1)
                t174 = inline206
                var t175 int = t173 + t174
                ref_set__Ref_3int(sum__0, t175)
                continue
            }
        } else {
            break Loop_loop167
        }
    }
    var inline232 string = "sum: "
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline232)
    _goml_runtime_core_string_print(inline233)
    var t165 int
    var inline230 int = ref_get__Ref_3int(sum__0)
    t165 = inline230
    var inline227 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t165)
    _goml_runtime_core_string_println(inline227)
    var inline223 string = "i at break: "
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline223)
    _goml_runtime_core_string_print(inline224)
    var t166 int
    var inline221 int = ref_get__Ref_3int(i__1)
    t166 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t166)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t196 string = _goml_runtime_core_int_to_string(self__40)
    return t196
}

func main() {
    main0()
}
