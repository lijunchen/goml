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
    var t196 closure_env_main_0 = closure_env_main_0{}
    var t197 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t196, p0)
    }
    var text__6 Box__string
    var inline260 int = 42
    var inline261 string = t197(inline260)
    var inline262 Box__string = Box__string{
        value: inline261,
    }
    text__6 = inline262
    var t198 string = text__6.value
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline257)
    var t199 closure_env_main_1 = closure_env_main_1{}
    var t200 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t199, p0)
    }
    var explicit__9 Box__string
    var inline253 int = 7
    var inline254 string = t200(inline253)
    var inline255 Box__string = Box__string{
        value: inline254,
    }
    explicit__9 = inline255
    var t201 string = explicit__9.value
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline250)
    var t202 closure_env_main_2 = closure_env_main_2{}
    var t203 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t202, p0)
    }
    var static_call__12 Box__string
    var inline246 int = 9
    var inline247 string = t203(inline246)
    var inline248 Box__string = Box__string{
        value: inline247,
    }
    static_call__12 = inline248
    var t204 string = static_call__12.value
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline243)
    var rendered__13 string
    var inline239 int = 5
    var inline240 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline239)
    var inline241 string = "value:" + inline240
    rendered__13 = inline241
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t225 string = _goml_runtime_core_int_to_string(self__67)
    return t225
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env192 closure_env_main_0, value__5 int) string {
    var inline267 string = _goml_runtime_core_int_to_string(value__5)
    return inline267
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env193 closure_env_main_1, value__8 int) string {
    var inline269 string = _goml_runtime_core_int_to_string(value__8)
    return inline269
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env194 closure_env_main_2, value__11 int) string {
    var inline271 string = _goml_runtime_core_int_to_string(value__11)
    return inline271
}

func main() {
    main0()
}
