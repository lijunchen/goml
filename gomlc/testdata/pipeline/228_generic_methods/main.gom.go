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
    var t191 closure_env_main_0 = closure_env_main_0{}
    var t192 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t191, p0)
    }
    var text__6 Box__string
    var inline255 int = 42
    var inline256 string = t192(inline255)
    var inline257 Box__string = Box__string{
        value: inline256,
    }
    text__6 = inline257
    var t193 string = text__6.value
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline252)
    var t194 closure_env_main_1 = closure_env_main_1{}
    var t195 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t194, p0)
    }
    var explicit__9 Box__string
    var inline248 int = 7
    var inline249 string = t195(inline248)
    var inline250 Box__string = Box__string{
        value: inline249,
    }
    explicit__9 = inline250
    var t196 string = explicit__9.value
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline245)
    var t197 closure_env_main_2 = closure_env_main_2{}
    var t198 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t197, p0)
    }
    var static_call__12 Box__string
    var inline241 int = 9
    var inline242 string = t198(inline241)
    var inline243 Box__string = Box__string{
        value: inline242,
    }
    static_call__12 = inline243
    var t199 string = static_call__12.value
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline238)
    var rendered__13 string
    var inline234 int = 5
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline234)
    var inline236 string = "value:" + inline235
    rendered__13 = inline236
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t220 string = _goml_runtime_core_int_to_string(self__67)
    return t220
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env187 closure_env_main_0, value__5 int) string {
    var inline262 string = _goml_runtime_core_int_to_string(value__5)
    return inline262
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env188 closure_env_main_1, value__8 int) string {
    var inline264 string = _goml_runtime_core_int_to_string(value__8)
    return inline264
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env189 closure_env_main_2, value__11 int) string {
    var inline266 string = _goml_runtime_core_int_to_string(value__11)
    return inline266
}

func main() {
    main0()
}
