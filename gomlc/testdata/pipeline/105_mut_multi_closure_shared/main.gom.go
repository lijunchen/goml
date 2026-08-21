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

type closure_env_get_1 struct {
    x_0 *ref_int_x
}

type Ordering int32

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var t417 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var inc__1 func() int = func() int {
        return _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(t417)
    }
    var t418 closure_env_get_1 = closure_env_get_1{
        x_0: x__0,
    }
    var get__2 func() int = func() int {
        return _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(t418)
    }
    inc__1()
    var t419 int = get__2()
    var t420 string
    var inline447 string = _goml_runtime_core_int_to_string(t419)
    t420 = inline447
    var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline444)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env414 closure_env_inc_0) int {
    var x__0 *ref_int_x = env414.x_0
    var t437 int = ref_get__Ref_3int(x__0)
    var t438 int = t437 + 1
    ref_set__Ref_3int(x__0, t438)
    var t439 int = ref_get__Ref_3int(x__0)
    return t439
}

func _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(env415 closure_env_get_1) int {
    var x__0 *ref_int_x = env415.x_0
    var t442 int = ref_get__Ref_3int(x__0)
    return t442
}

func main() {
    main0()
}
