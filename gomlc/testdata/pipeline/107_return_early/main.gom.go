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
    var t432 bool = x__0 < 0
    if t432 {
        return 0
    } else {
        var t431 bool = x__0 == 0
        if t431 {
            return 1
        } else {
            var t430 int32 = x__0 + 2
            return t430
        }
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t440 int32 = early(-1)
    println__T_int32(t440)
    var inline513 string = "e0: "
    var inline514 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline513)
    _goml_runtime_core_string_print(inline514)
    var t441 int32 = early(0)
    var inline510 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t441)
    _goml_runtime_core_string_println(inline510)
    var inline506 string = "e3: "
    var inline507 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline506)
    _goml_runtime_core_string_print(inline507)
    var t442 int32 = early(3)
    var inline503 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t442)
    _goml_runtime_core_string_println(inline503)
    var inline499 string = "c7: "
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline499)
    _goml_runtime_core_string_print(inline500)
    var t443 int32
    var inline494 int32 = 7
    var inline495 closure_env_f_0 = closure_env_f_0{}
    var inline496 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline495, p0)
    }
    var inline497 int32 = inline496(inline494)
    t443 = inline497
    var inline491 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t443)
    _goml_runtime_core_string_println(inline491)
    var inline487 string = "c2: "
    var inline488 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline487)
    _goml_runtime_core_string_print(inline488)
    var t444 int32
    var inline482 int32 = 2
    var inline483 closure_env_f_0 = closure_env_f_0{}
    var inline484 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline483, p0)
    }
    var inline485 int32 = inline484(inline482)
    t444 = inline485
    var inline479 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t444)
    _goml_runtime_core_string_println(inline479)
    var inline474 bool = true
    if inline474 {
        var inline469 bool = false
        if inline469 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    } else {
        println__T_string("after")
        var inline469 bool = false
        if inline469 {
            return struct{}{}
        } else {
            println__T_string("after")
            return struct{}{}
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t446 string
    t446 = value__1
    _goml_runtime_core_string_println(t446)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t449 string
    t449 = value__0
    _goml_runtime_core_string_print(t449)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t452 string
    var inline519 string = _goml_runtime_core_int32_to_string(value__1)
    t452 = inline519
    _goml_runtime_core_string_println(t452)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t458 string = _goml_runtime_core_int32_to_string(self__154)
    return t458
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env425 closure_env_f_0, y__2 int32) int32 {
    var t463 bool = y__2 > 5
    if t463 {
        return y__2
    } else {
        var t462 int32 = y__2 + 10
        return t462
    }
}

func main() {
    main0()
}
