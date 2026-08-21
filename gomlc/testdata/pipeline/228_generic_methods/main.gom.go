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
    var t420 closure_env_main_0 = closure_env_main_0{}
    var t421 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t420, p0)
    }
    var text__6 Box__string
    var inline484 int = 42
    var inline485 string = t421(inline484)
    var inline486 Box__string = Box__string{
        value: inline485,
    }
    text__6 = inline486
    var t422 string = text__6.value
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline481)
    var t423 closure_env_main_1 = closure_env_main_1{}
    var t424 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t423, p0)
    }
    var explicit__9 Box__string
    var inline477 int = 7
    var inline478 string = t424(inline477)
    var inline479 Box__string = Box__string{
        value: inline478,
    }
    explicit__9 = inline479
    var t425 string = explicit__9.value
    var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline474)
    var t426 closure_env_main_2 = closure_env_main_2{}
    var t427 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t426, p0)
    }
    var static_call__12 Box__string
    var inline470 int = 9
    var inline471 string = t427(inline470)
    var inline472 Box__string = Box__string{
        value: inline471,
    }
    static_call__12 = inline472
    var t428 string = static_call__12.value
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline467)
    var rendered__13 string
    var inline463 int = 5
    var inline464 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline463)
    var inline465 string = "value:" + inline464
    rendered__13 = inline465
    var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline460)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t449 string = _goml_runtime_core_int_to_string(self__151)
    return t449
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env416 closure_env_main_0, value__5 int) string {
    var inline491 string = _goml_runtime_core_int_to_string(value__5)
    return inline491
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env417 closure_env_main_1, value__8 int) string {
    var inline493 string = _goml_runtime_core_int_to_string(value__8)
    return inline493
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env418 closure_env_main_2, value__11 int) string {
    var inline495 string = _goml_runtime_core_int_to_string(value__11)
    return inline495
}

func main() {
    main0()
}
