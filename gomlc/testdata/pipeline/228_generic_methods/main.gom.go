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

type Box__int struct {
    value int
}

type Box__string struct {
    value string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type Ordering int32

func main0() struct{} {
    var t417 closure_env_main_0 = closure_env_main_0{}
    var t418 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t417, p0)
    }
    var text__6 Box__string
    var inline481 int = 42
    var inline482 string = t418(inline481)
    var inline483 Box__string = Box__string{
        value: inline482,
    }
    text__6 = inline483
    var t419 string = text__6.value
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline478)
    var t420 closure_env_main_1 = closure_env_main_1{}
    var t421 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t420, p0)
    }
    var explicit__9 Box__string
    var inline474 int = 7
    var inline475 string = t421(inline474)
    var inline476 Box__string = Box__string{
        value: inline475,
    }
    explicit__9 = inline476
    var t422 string = explicit__9.value
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline471)
    var t423 closure_env_main_2 = closure_env_main_2{}
    var t424 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t423, p0)
    }
    var static_call__12 Box__string
    var inline467 int = 9
    var inline468 string = t424(inline467)
    var inline469 Box__string = Box__string{
        value: inline468,
    }
    static_call__12 = inline469
    var t425 string = static_call__12.value
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline464)
    var rendered__13 string
    var inline460 int = 5
    var inline461 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline460)
    var inline462 string = "value:" + inline461
    rendered__13 = inline462
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t446 string = _goml_runtime_core_int_to_string(self__151)
    return t446
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env413 closure_env_main_0, value__5 int) string {
    var inline488 string = _goml_runtime_core_int_to_string(value__5)
    return inline488
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env414 closure_env_main_1, value__8 int) string {
    var inline490 string = _goml_runtime_core_int_to_string(value__8)
    return inline490
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env415 closure_env_main_2, value__11 int) string {
    var inline492 string = _goml_runtime_core_int_to_string(value__11)
    return inline492
}

func main() {
    main0()
}
