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
    var inline453 string = "ok"
    var inline454 string = "direct:"
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline453)
    var inline456 string = inline454 + inline455
    direct__3 = inline456
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline450)
    var t413 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline448 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t413, 11)
    generic__4 = inline448
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline445)
    var ufcs__5 string
    var inline440 int = 12
    var inline441 string = "ufcs:"
    var inline442 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline440)
    var inline443 string = inline441 + inline442
    ufcs__5 = inline443
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t428 string = self__0.value
    var t429 string
    var inline465 string = _goml_runtime_core_int_to_string(value__1)
    t429 = inline465
    var t430 string = t428 + t429
    return t430
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t435 string = _goml_runtime_core_int_to_string(self__151)
    return t435
}

func main() {
    main0()
}
