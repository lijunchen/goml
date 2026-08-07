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
    var t196 bool = x__0 < 0
    if t196 {
        return 0
    } else {
        var t195 bool
        var inline231 int32 = 0
        var inline232 bool = x__0 == inline231
        t195 = inline232
        if t195 {
            return 1
        } else {
            var t194 int32 = x__0 + 2
            return t194
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t203 int32 = early(-1)
    var inline289 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
    _goml_runtime_core_string_println(inline289)
    var inline285 string = "e0: "
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
    _goml_runtime_core_string_print(inline286)
    var t204 int32 = early(0)
    var inline282 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t204)
    _goml_runtime_core_string_println(inline282)
    var inline278 string = "e3: "
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline278)
    _goml_runtime_core_string_print(inline279)
    var t205 int32 = early(3)
    var inline275 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
    _goml_runtime_core_string_println(inline275)
    var inline271 string = "c7: "
    var inline272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline271)
    _goml_runtime_core_string_print(inline272)
    var t206 int32
    var inline267 int32 = 7
    var inline268 closure_env_f_0 = closure_env_f_0{}
    var inline269 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline268, inline267)
    t206 = inline269
    var inline264 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t206)
    _goml_runtime_core_string_println(inline264)
    var inline260 string = "c2: "
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline260)
    _goml_runtime_core_string_print(inline261)
    var t207 int32
    var inline256 int32 = 2
    var inline257 closure_env_f_0 = closure_env_f_0{}
    var inline258 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline257, inline256)
    t207 = inline258
    var inline253 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
    _goml_runtime_core_string_println(inline253)
    var inline248 bool = true
    if inline248 {
        var inline243 bool = false
        if inline243 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline243 bool = false
        if inline243 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__31 string) struct{} {
    var t212 string
    t212 = value__31
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func print__T_string(value__30 string) struct{} {
    var t215 string
    t215 = value__30
    _goml_runtime_core_string_print(t215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t224 string = _goml_runtime_core_int32_to_string(self__72)
    return t224
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env189 closure_env_f_0, y__2 int32) int32 {
    var t229 bool = y__2 > 5
    if t229 {
        return y__2
    } else {
        var t228 int32 = y__2 + 10
        return t228
    }
}

func main() {
    main0()
}
