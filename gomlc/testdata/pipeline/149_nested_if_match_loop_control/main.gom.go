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
    var inline193 int = 0
    var inline194 *ref_int_x = ref__Ref_3int(inline193)
    i__0 = inline194
    var sum__1 *ref_int_x
    var inline190 int = 0
    var inline191 *ref_int_x = ref__Ref_3int(inline190)
    sum__1 = inline191
    Loop_loop144:
    for {
        var t145 int
        var inline183 int = ref_get__Ref_3int(i__0)
        t145 = inline183
        var t146 bool = t145 < 7
        if t146 {
            var cur__2 int
            var inline181 int = ref_get__Ref_3int(i__0)
            cur__2 = inline181
            var t147 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t147)
            var t151 bool = cur__2 < 5
            if t151 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t149 int
                    var inline177 int = ref_get__Ref_3int(sum__1)
                    t149 = inline177
                    var t150 int = t149 + cur__2
                    ref_set__Ref_3int(sum__1, t150)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop144
                default:
                    var t149 int
                    var inline177 int = ref_get__Ref_3int(sum__1)
                    t149 = inline177
                    var t150 int = t149 + cur__2
                    ref_set__Ref_3int(sum__1, t150)
                    continue
                }
            }
        } else {
            break Loop_loop144
        }
    }
    var t143 int
    var inline188 int = ref_get__Ref_3int(sum__1)
    t143 = inline188
    var inline185 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t143)
    _goml_runtime_core_string_println(inline185)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t167 string = _goml_runtime_core_int_to_string(self__69)
    return t167
}

func main() {
    main0()
}
