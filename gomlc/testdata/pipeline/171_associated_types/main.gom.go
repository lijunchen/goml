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

type Number struct {
    value int32
}

type Box__string struct {
    value string
}

type Ordering int32

func _goml_m_trait__impl_i_Provider_i_Number_i_get(self__0 Number) int32 {
    var t416 int32 = self__0.value
    return t416
}

func main0() struct{} {
    var t418 Number = Number{
        value: 42,
    }
    var t419 int32
    var inline474 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t418)
    t419 = inline474
    var t420 string
    var inline472 string = _goml_runtime_core_int32_to_string(t419)
    t420 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline469)
    var t421 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline467 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t421)
    value__4 = inline467
    var t422 string
    var inline465 string = _goml_runtime_core_int32_to_string(value__4)
    t422 = inline465
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline462)
    var t423 Box__string = Box__string{
        value: "generic",
    }
    var t424 string
    var inline460 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t423)
    t424 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline457)
    var t426 int32
    var inline455 int32 = 11
    t426 = inline455
    var t427 string
    var inline453 string = _goml_runtime_core_int32_to_string(t426)
    t427 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline450)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t448 string = self__1.value
    return t448
}

func main() {
    main0()
}
