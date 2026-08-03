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
    var t201 bool = x__0 < 0
    if t201 {
        return 0
    } else {
        var t200 bool
        var inline236 int32 = 0
        var inline237 bool = x__0 == inline236
        t200 = inline237
        if t200 {
            return 1
        } else {
            var t199 int32 = x__0 + 2
            return t199
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t208 int32 = early(-1)
    var inline294 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
    _goml_runtime_core_string_println(inline294)
    var inline290 string = "e0: "
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline290)
    _goml_runtime_core_string_print(inline291)
    var t209 int32 = early(0)
    var inline287 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
    _goml_runtime_core_string_println(inline287)
    var inline283 string = "e3: "
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline283)
    _goml_runtime_core_string_print(inline284)
    var t210 int32 = early(3)
    var inline280 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t210)
    _goml_runtime_core_string_println(inline280)
    var inline276 string = "c7: "
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline276)
    _goml_runtime_core_string_print(inline277)
    var t211 int32
    var inline272 int32 = 7
    var inline273 closure_env_f_0 = closure_env_f_0{}
    var inline274 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline273, inline272)
    t211 = inline274
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t211)
    _goml_runtime_core_string_println(inline269)
    var inline265 string = "c2: "
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline265)
    _goml_runtime_core_string_print(inline266)
    var t212 int32
    var inline261 int32 = 2
    var inline262 closure_env_f_0 = closure_env_f_0{}
    var inline263 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline262, inline261)
    t212 = inline263
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
    _goml_runtime_core_string_println(inline258)
    var inline253 bool = true
    if inline253 {
        var inline248 bool = false
        if inline248 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline248 bool = false
        if inline248 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__31 string) struct{} {
    var t217 string
    t217 = value__31
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func print__T_string(value__30 string) struct{} {
    var t220 string
    t220 = value__30
    _goml_runtime_core_string_print(t220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t229 string = _goml_runtime_core_int32_to_string(self__72)
    return t229
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env194 closure_env_f_0, y__2 int32) int32 {
    var t234 bool = y__2 > 5
    if t234 {
        return y__2
    } else {
        var t233 int32 = y__2 + 10
        return t233
    }
}

func main() {
    main0()
}
