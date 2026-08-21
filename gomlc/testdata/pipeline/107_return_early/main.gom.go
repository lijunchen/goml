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

type Ordering int32

func early(x__0 int32) int32 {
    var t435 bool = x__0 < 0
    if t435 {
        return 0
    } else {
        var t434 bool = x__0 == 0
        if t434 {
            return 1
        } else {
            var t433 int32 = x__0 + 2
            return t433
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t443 int32 = early(-1)
    println__T_int32(t443)
    var inline516 string = "e0: "
    var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline516)
    _goml_runtime_core_string_print(inline517)
    var t444 int32 = early(0)
    var inline513 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t444)
    _goml_runtime_core_string_println(inline513)
    var inline509 string = "e3: "
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline509)
    _goml_runtime_core_string_print(inline510)
    var t445 int32 = early(3)
    var inline506 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t445)
    _goml_runtime_core_string_println(inline506)
    var inline502 string = "c7: "
    var inline503 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline502)
    _goml_runtime_core_string_print(inline503)
    var t446 int32
    var inline497 int32 = 7
    var inline498 closure_env_f_0 = closure_env_f_0{}
    var inline499 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline498, p0)
    }
    var inline500 int32 = inline499(inline497)
    t446 = inline500
    var inline494 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t446)
    _goml_runtime_core_string_println(inline494)
    var inline490 string = "c2: "
    var inline491 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline490)
    _goml_runtime_core_string_print(inline491)
    var t447 int32
    var inline485 int32 = 2
    var inline486 closure_env_f_0 = closure_env_f_0{}
    var inline487 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline486, p0)
    }
    var inline488 int32 = inline487(inline485)
    t447 = inline488
    var inline482 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t447)
    _goml_runtime_core_string_println(inline482)
    var inline477 bool = true
    if inline477 {
        var inline472 bool = false
        if inline472 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline472 bool = false
        if inline472 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t449 string
    t449 = value__1
    _goml_runtime_core_string_println(t449)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t452 string
    t452 = value__0
    _goml_runtime_core_string_print(t452)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t455 string
    var inline522 string = _goml_runtime_core_int32_to_string(value__1)
    t455 = inline522
    _goml_runtime_core_string_println(t455)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t461 string = _goml_runtime_core_int32_to_string(self__154)
    return t461
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env428 closure_env_f_0, y__2 int32) int32 {
    var t466 bool = y__2 > 5
    if t466 {
        return y__2
    } else {
        var t465 int32 = y__2 + 10
        return t465
    }
}

func main() {
    main0()
}
