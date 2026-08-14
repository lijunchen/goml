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
    var t206 bool = x__0 < 0
    if t206 {
        return 0
    } else {
        var t205 bool = x__0 == 0
        if t205 {
            return 1
        } else {
            var t204 int32 = x__0 + 2
            return t204
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t214 int32 = early(-1)
    println__T_int32(t214)
    var inline287 string = "e0: "
    var inline288 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline287)
    _goml_runtime_core_string_print(inline288)
    var t215 int32 = early(0)
    var inline284 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t215)
    _goml_runtime_core_string_println(inline284)
    var inline280 string = "e3: "
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline280)
    _goml_runtime_core_string_print(inline281)
    var t216 int32 = early(3)
    var inline277 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t216)
    _goml_runtime_core_string_println(inline277)
    var inline273 string = "c7: "
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline273)
    _goml_runtime_core_string_print(inline274)
    var t217 int32
    var inline268 int32 = 7
    var inline269 closure_env_f_0 = closure_env_f_0{}
    var inline270 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline269, p0)
    }
    var inline271 int32 = inline270(inline268)
    t217 = inline271
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
    _goml_runtime_core_string_println(inline265)
    var inline261 string = "c2: "
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline261)
    _goml_runtime_core_string_print(inline262)
    var t218 int32
    var inline256 int32 = 2
    var inline257 closure_env_f_0 = closure_env_f_0{}
    var inline258 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline257, p0)
    }
    var inline259 int32 = inline258(inline256)
    t218 = inline259
    var inline253 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t218)
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

func println__T_string(value__1 string) struct{} {
    var t220 string
    t220 = value__1
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t223 string
    t223 = value__0
    _goml_runtime_core_string_print(t223)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t226 string
    var inline293 string = _goml_runtime_core_int32_to_string(value__1)
    t226 = inline293
    _goml_runtime_core_string_println(t226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__70)
    return t232
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env199 closure_env_f_0, y__2 int32) int32 {
    var t237 bool = y__2 > 5
    if t237 {
        return y__2
    } else {
        var t236 int32 = y__2 + 10
        return t236
    }
}

func main() {
    main0()
}
