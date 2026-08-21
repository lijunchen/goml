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

type Ordering int32

func main0() struct{} {
    var i__0 *ref_int_x
    var inline470 int = 0
    var inline471 *ref_int_x = ref__Ref_3int(inline470)
    i__0 = inline471
    Loop_loop418:
    for {
        var t419 int
        var inline464 int = ref_get__Ref_3int(i__0)
        t419 = inline464
        var t420 bool = t419 < 10
        if t420 {
            var t421 int
            var inline462 int = ref_get__Ref_3int(i__0)
            t421 = inline462
            var t422 int = t421 + 1
            ref_set__Ref_3int(i__0, t422)
            var t425 int
            var inline458 int = ref_get__Ref_3int(i__0)
            t425 = inline458
            var t426 bool = t425 == 5
            if t426 {
                continue
            } else {
                var t424 int
                var inline456 int = ref_get__Ref_3int(i__0)
                t424 = inline456
                var inline453 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t424)
                _goml_runtime_core_string_println(inline453)
                continue
            }
        } else {
            break Loop_loop418
        }
    }
    var inline466 string = "done"
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline466)
    _goml_runtime_core_string_println(inline467)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t443 string = _goml_runtime_core_int_to_string(self__151)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
