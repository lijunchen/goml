package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type UserId struct {
    _0 int32
}

type Box__string struct {
    _0 string
}

type Ordering int32

func main0() struct{} {
    var raw__4 int32 = 40
    var id__5 UserId = UserId{
        _0: raw__4,
    }
    var inline464 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(raw__4)
    _goml_runtime_core_string_println(inline464)
    var t430 UserId
    var inline455 UserId = id__5
    var inline456 UserId = inline455
    var inline457 int32 = inline456._0
    var inline458 int32 = 1
    var inline459 int32 = inline457 + inline458
    var inline460 UserId = UserId{
        _0: inline459,
    }
    inline455 = inline460
    t430 = inline455
    var t431 int32
    var inline452 int32 = t430._0
    t431 = inline452
    var inline448 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t431)
    _goml_runtime_core_string_println(inline448)
    var x420 string = "wrapped"
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x420)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t441 string = _goml_runtime_core_int32_to_string(self__154)
    return t441
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
