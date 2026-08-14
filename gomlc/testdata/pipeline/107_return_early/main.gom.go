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
    var t211 bool = x__0 < 0
    if t211 {
        return 0
    } else {
        var t210 bool = x__0 == 0
        if t210 {
            return 1
        } else {
            var t209 int32 = x__0 + 2
            return t209
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t219 int32 = early(-1)
    println__T_int32(t219)
    var inline292 string = "e0: "
    var inline293 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline292)
    _goml_runtime_core_string_print(inline293)
    var t220 int32 = early(0)
    var inline289 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t220)
    _goml_runtime_core_string_println(inline289)
    var inline285 string = "e3: "
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
    _goml_runtime_core_string_print(inline286)
    var t221 int32 = early(3)
    var inline282 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t221)
    _goml_runtime_core_string_println(inline282)
    var inline278 string = "c7: "
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline278)
    _goml_runtime_core_string_print(inline279)
    var t222 int32
    var inline273 int32 = 7
    var inline274 closure_env_f_0 = closure_env_f_0{}
    var inline275 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline274, p0)
    }
    var inline276 int32 = inline275(inline273)
    t222 = inline276
    var inline270 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t222)
    _goml_runtime_core_string_println(inline270)
    var inline266 string = "c2: "
    var inline267 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline266)
    _goml_runtime_core_string_print(inline267)
    var t223 int32
    var inline261 int32 = 2
    var inline262 closure_env_f_0 = closure_env_f_0{}
    var inline263 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline262, p0)
    }
    var inline264 int32 = inline263(inline261)
    t223 = inline264
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t223)
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

func println__T_string(value__1 string) struct{} {
    var t225 string
    t225 = value__1
    _goml_runtime_core_string_println(t225)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t228 string
    t228 = value__0
    _goml_runtime_core_string_print(t228)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t231 string
    var inline298 string = _goml_runtime_core_int32_to_string(value__1)
    t231 = inline298
    _goml_runtime_core_string_println(t231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t237 string = _goml_runtime_core_int32_to_string(self__70)
    return t237
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env204 closure_env_f_0, y__2 int32) int32 {
    var t242 bool = y__2 > 5
    if t242 {
        return y__2
    } else {
        var t241 int32 = y__2 + 10
        return t241
    }
}

func main() {
    main0()
}
