package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_f_0 struct {}

func early(x__0 int32) int32 {
    var t160 bool = x__0 < 0
    if t160 {
        return 0
    } else {
        var t159 bool
        var inline195 int32 = 0
        var inline196 bool = x__0 == inline195
        t159 = inline196
        if t159 {
            return 1
        } else {
            var t158 int32 = x__0 + 2
            return t158
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t167 int32 = early(-1)
    var inline253 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t167)
    _goml_runtime_core_string_println(inline253)
    var inline249 string = "e0: "
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
    _goml_runtime_core_string_print(inline250)
    var t168 int32 = early(0)
    var inline246 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t168)
    _goml_runtime_core_string_println(inline246)
    var inline242 string = "e3: "
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline242)
    _goml_runtime_core_string_print(inline243)
    var t169 int32 = early(3)
    var inline239 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t169)
    _goml_runtime_core_string_println(inline239)
    var inline235 string = "c7: "
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
    _goml_runtime_core_string_print(inline236)
    var t170 int32
    var inline231 int32 = 7
    var inline232 closure_env_f_0 = closure_env_f_0{}
    var inline233 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline232, inline231)
    t170 = inline233
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t170)
    _goml_runtime_core_string_println(inline228)
    var inline224 string = "c2: "
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline224)
    _goml_runtime_core_string_print(inline225)
    var t171 int32
    var inline220 int32 = 2
    var inline221 closure_env_f_0 = closure_env_f_0{}
    var inline222 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline221, inline220)
    t171 = inline222
    var inline217 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t171)
    _goml_runtime_core_string_println(inline217)
    var inline212 bool = true
    if inline212 {
        var inline207 bool = false
        if inline207 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline207 bool = false
        if inline207 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__31 string) struct{} {
    var t176 string
    t176 = value__31
    _goml_runtime_core_string_println(t176)
    return struct{}{}
}

func print__T_string(value__30 string) struct{} {
    var t179 string
    t179 = value__30
    _goml_runtime_core_string_print(t179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t188 string = _goml_runtime_core_int32_to_string(self__72)
    return t188
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env153 closure_env_f_0, y__2 int32) int32 {
    var t193 bool = y__2 > 5
    if t193 {
        return y__2
    } else {
        var t192 int32 = y__2 + 10
        return t192
    }
}

func main() {
    main0()
}
