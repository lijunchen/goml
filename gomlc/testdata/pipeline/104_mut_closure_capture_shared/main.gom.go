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

type closure_env_inc_0 struct {
    x_0 *ref_int_x
}

type Ordering int32

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var t413 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var inc__1 func() int = func() int {
        return _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(t413)
    }
    inc__1()
    var t414 int = ref_get__Ref_3int(x__0)
    var t415 string
    var inline439 string = _goml_runtime_core_int_to_string(t414)
    t415 = inline439
    var inline436 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline436)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env411 closure_env_inc_0) int {
    var x__0 *ref_int_x = env411.x_0
    var t432 int = ref_get__Ref_3int(x__0)
    var t433 int = t432 + 1
    ref_set__Ref_3int(x__0, t433)
    var t434 int = ref_get__Ref_3int(x__0)
    return t434
}

func main() {
    main0()
}
