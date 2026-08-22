package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Prefix struct {
    value string
}

type Ordering int32

func main0() struct{} {
    var direct__3 string
    var inline456 string = "ok"
    var inline457 string = "direct:"
    var inline458 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline456)
    var inline459 string = inline457 + inline458
    direct__3 = inline459
    var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline453)
    var t416 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline451 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t416, 11)
    generic__4 = inline451
    var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline448)
    var ufcs__5 string
    var inline443 int = 12
    var inline444 string = "ufcs:"
    var inline445 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline443)
    var inline446 string = inline444 + inline445
    ufcs__5 = inline446
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t431 string = self__0.value
    var t432 string
    var inline468 string = _goml_runtime_core_int_to_string(value__1)
    t432 = inline468
    var t433 string = t431 + t432
    return t433
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t438 string = _goml_runtime_core_int_to_string(self__151)
    return t438
}

func main() {
    main0()
}
