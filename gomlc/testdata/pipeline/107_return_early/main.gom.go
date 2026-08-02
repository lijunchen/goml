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
    var t179 bool = x__0 < 0
    if t179 {
        return 0
    } else {
        var t178 bool
        var inline214 int32 = 0
        var inline215 bool = x__0 == inline214
        t178 = inline215
        if t178 {
            return 1
        } else {
            var t177 int32 = x__0 + 2
            return t177
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t186 int32 = early(-1)
    var inline272 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t186)
    _goml_runtime_core_string_println(inline272)
    var inline268 string = "e0: "
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline268)
    _goml_runtime_core_string_print(inline269)
    var t187 int32 = early(0)
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t187)
    _goml_runtime_core_string_println(inline265)
    var inline261 string = "e3: "
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline261)
    _goml_runtime_core_string_print(inline262)
    var t188 int32 = early(3)
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t188)
    _goml_runtime_core_string_println(inline258)
    var inline254 string = "c7: "
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
    _goml_runtime_core_string_print(inline255)
    var t189 int32
    var inline250 int32 = 7
    var inline251 closure_env_f_0 = closure_env_f_0{}
    var inline252 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline251, inline250)
    t189 = inline252
    var inline247 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t189)
    _goml_runtime_core_string_println(inline247)
    var inline243 string = "c2: "
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline243)
    _goml_runtime_core_string_print(inline244)
    var t190 int32
    var inline239 int32 = 2
    var inline240 closure_env_f_0 = closure_env_f_0{}
    var inline241 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline240, inline239)
    t190 = inline241
    var inline236 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t190)
    _goml_runtime_core_string_println(inline236)
    var inline231 bool = true
    if inline231 {
        var inline226 bool = false
        if inline226 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline226 bool = false
        if inline226 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t195 string
    t195 = value__1
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t198 string
    t198 = value__0
    _goml_runtime_core_string_print(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t207 string = _goml_runtime_core_int32_to_string(self__43)
    return t207
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env172 closure_env_f_0, y__2 int32) int32 {
    var t212 bool = y__2 > 5
    if t212 {
        return y__2
    } else {
        var t211 int32 = y__2 + 10
        return t211
    }
}

func main() {
    main0()
}
