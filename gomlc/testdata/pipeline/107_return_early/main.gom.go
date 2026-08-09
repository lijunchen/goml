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
        var t195 bool = x__0 == 0
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
    var inline283 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
    _goml_runtime_core_string_println(inline283)
    var inline279 string = "e0: "
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline279)
    _goml_runtime_core_string_print(inline280)
    var t204 int32 = early(0)
    var inline276 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t204)
    _goml_runtime_core_string_println(inline276)
    var inline272 string = "e3: "
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline272)
    _goml_runtime_core_string_print(inline273)
    var t205 int32 = early(3)
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
    _goml_runtime_core_string_println(inline269)
    var inline265 string = "c7: "
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline265)
    _goml_runtime_core_string_print(inline266)
    var t206 int32
    var inline261 int32 = 7
    var inline262 closure_env_f_0 = closure_env_f_0{}
    var inline263 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline262, inline261)
    t206 = inline263
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t206)
    _goml_runtime_core_string_println(inline258)
    var inline254 string = "c2: "
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
    _goml_runtime_core_string_print(inline255)
    var t207 int32
    var inline250 int32 = 2
    var inline251 closure_env_f_0 = closure_env_f_0{}
    var inline252 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline251, inline250)
    t207 = inline252
    var inline247 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
    _goml_runtime_core_string_println(inline247)
    var inline242 bool = true
    if inline242 {
        var inline237 bool = false
        if inline237 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline237 bool = false
        if inline237 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__31 string) struct{} {
    var t209 string
    t209 = value__31
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func print__T_string(value__30 string) struct{} {
    var t212 string
    t212 = value__30
    _goml_runtime_core_string_print(t212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__72)
    return t221
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env189 closure_env_f_0, y__2 int32) int32 {
    var t226 bool = y__2 > 5
    if t226 {
        return y__2
    } else {
        var t225 int32 = y__2 + 10
        return t225
    }
}

func main() {
    main0()
}
