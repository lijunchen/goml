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
    var inline229 int = 0
    var inline230 *ref_int_x = ref__Ref_3int(inline229)
    i__0 = inline230
    var sum__1 *ref_int_x
    var inline226 int = 0
    var inline227 *ref_int_x = ref__Ref_3int(inline226)
    sum__1 = inline227
    Loop_loop180:
    for {
        var t181 int
        var inline219 int = ref_get__Ref_3int(i__0)
        t181 = inline219
        var t182 bool = t181 < 7
        if t182 {
            var cur__2 int
            var inline217 int = ref_get__Ref_3int(i__0)
            cur__2 = inline217
            var t183 int = cur__2 + 1
            ref_set__Ref_3int(i__0, t183)
            var t187 bool = cur__2 < 5
            if t187 {
                switch cur__2 {
                case 1:
                    continue
                case 3:
                    continue
                default:
                    var t185 int
                    var inline213 int = ref_get__Ref_3int(sum__1)
                    t185 = inline213
                    var t186 int = t185 + cur__2
                    ref_set__Ref_3int(sum__1, t186)
                    continue
                }
            } else {
                switch cur__2 {
                case 5:
                    break Loop_loop180
                default:
                    var t185 int
                    var inline213 int = ref_get__Ref_3int(sum__1)
                    t185 = inline213
                    var t186 int = t185 + cur__2
                    ref_set__Ref_3int(sum__1, t186)
                    continue
                }
            }
        } else {
            break Loop_loop180
        }
    }
    var t179 int
    var inline224 int = ref_get__Ref_3int(sum__1)
    t179 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t179)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t203 string = _goml_runtime_core_int_to_string(self__67)
    return t203
}

func main() {
    main0()
}
