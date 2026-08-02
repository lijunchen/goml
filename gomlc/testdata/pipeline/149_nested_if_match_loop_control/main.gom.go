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
    var i__0 *ref_int_x
    var inline212 int = 0
    var inline213 *ref_int_x = ref__Ref_3int(inline212)
    i__0 = inline213
    var sum__1 *ref_int_x
    var inline209 int = 0
    var inline210 *ref_int_x = ref__Ref_3int(inline209)
    sum__1 = inline210
    Loop_loop163:
    for {
        var t164 int
        var inline202 int = ref_get__Ref_3int(i__0)
        t164 = inline202
        var t165 bool = t164 < 7
        if t165 {
            var cur__2 int
            var inline200 int = ref_get__Ref_3int(i__0)
            cur__2 = inline200
            var t166 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t166)
            var t170 bool = cur__2 < 5
            if t170 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t168 int
                    var inline196 int = ref_get__Ref_3int(sum__1)
                    t168 = inline196
                    var t169 int = t168 + cur__2
                    ref_set__Ref_3int(sum__1, t169)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop163
                default:
                    var t168 int
                    var inline196 int = ref_get__Ref_3int(sum__1)
                    t168 = inline196
                    var t169 int = t168 + cur__2
                    ref_set__Ref_3int(sum__1, t169)
                    continue
                }
            }
        } else {
            break Loop_loop163
        }
    }
    var t162 int
    var inline207 int = ref_get__Ref_3int(sum__1)
    t162 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t162)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t186 string = _goml_runtime_core_int_to_string(self__40)
    return t186
}

func main() {
    main0()
}
