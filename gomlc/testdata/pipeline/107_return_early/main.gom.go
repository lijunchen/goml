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
    var t204 int32 = early(-1)
    println__T_int32(t204)
    var inline277 string = "e0: "
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline277)
    _goml_runtime_core_string_print(inline278)
    var t205 int32 = early(0)
    var inline274 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
    _goml_runtime_core_string_println(inline274)
    var inline270 string = "e3: "
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline270)
    _goml_runtime_core_string_print(inline271)
    var t206 int32 = early(3)
    var inline267 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t206)
    _goml_runtime_core_string_println(inline267)
    var inline263 string = "c7: "
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline263)
    _goml_runtime_core_string_print(inline264)
    var t207 int32
    var inline258 int32 = 7
    var inline259 closure_env_f_0 = closure_env_f_0{}
    var inline260 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline259, p0)
    }
    var inline261 int32 = inline260(inline258)
    t207 = inline261
    var inline255 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
    _goml_runtime_core_string_println(inline255)
    var inline251 string = "c2: "
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline251)
    _goml_runtime_core_string_print(inline252)
    var t208 int32
    var inline246 int32 = 2
    var inline247 closure_env_f_0 = closure_env_f_0{}
    var inline248 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline247, p0)
    }
    var inline249 int32 = inline248(inline246)
    t208 = inline249
    var inline243 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
    _goml_runtime_core_string_println(inline243)
    var inline238 bool = true
    if inline238 {
        var inline233 bool = false
        if inline233 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline233 bool = false
        if inline233 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t210 string
    t210 = value__1
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t213 string
    t213 = value__0
    _goml_runtime_core_string_print(t213)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t216 string
    var inline283 string = _goml_runtime_core_int32_to_string(value__1)
    t216 = inline283
    _goml_runtime_core_string_println(t216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__70)
    return t222
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env189 closure_env_f_0, y__2 int32) int32 {
    var t227 bool = y__2 > 5
    if t227 {
        return y__2
    } else {
        var t226 int32 = y__2 + 10
        return t226
    }
}

func main() {
    main0()
}
