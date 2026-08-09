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

func main0() struct{} {
    var t181 closure_env_main_0 = closure_env_main_0{}
    var t182 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t181, p0)
    }
    var text__6 Box__string
    var inline245 int = 42
    var inline246 string = t182(inline245)
    var inline247 Box__string = Box__string{
        value: inline246,
    }
    text__6 = inline247
    var t183 string = text__6.value
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline242)
    var t184 closure_env_main_1 = closure_env_main_1{}
    var t185 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t184, p0)
    }
    var explicit__9 Box__string
    var inline238 int = 7
    var inline239 string = t185(inline238)
    var inline240 Box__string = Box__string{
        value: inline239,
    }
    explicit__9 = inline240
    var t186 string = explicit__9.value
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline235)
    var t187 closure_env_main_2 = closure_env_main_2{}
    var t188 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t187, p0)
    }
    var static_call__12 Box__string
    var inline231 int = 9
    var inline232 string = t188(inline231)
    var inline233 Box__string = Box__string{
        value: inline232,
    }
    static_call__12 = inline233
    var t189 string = static_call__12.value
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline228)
    var rendered__13 string
    var inline224 int = 5
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline224)
    var inline226 string = "value:" + inline225
    rendered__13 = inline226
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t210 string = _goml_runtime_core_int_to_string(self__69)
    return t210
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env177 closure_env_main_0, value__5 int) string {
    var inline252 string = _goml_runtime_core_int_to_string(value__5)
    return inline252
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env178 closure_env_main_1, value__8 int) string {
    var inline254 string = _goml_runtime_core_int_to_string(value__8)
    return inline254
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env179 closure_env_main_2, value__11 int) string {
    var inline256 string = _goml_runtime_core_int_to_string(value__11)
    return inline256
}

func main() {
    main0()
}
