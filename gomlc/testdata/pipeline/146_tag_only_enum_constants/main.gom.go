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

type Light int32

const (
    Light_Red Light = 0
    Yellow Light = 1
    Green Light = 2
)

type Paint int32

const (
    Paint_Red Paint = 0
    Blue Paint = 1
)

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t167 int32
    switch light__2 {
    case Light_Red:
        t167 = 10
    case Yellow:
        t167 = 20
    case Green:
        t167 = 30
    default:
        panic("non-exhaustive match")
    }
    var t168 string
    var inline206 string = _goml_runtime_core_int32_to_string(t167)
    t168 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t168)
    _goml_runtime_core_string_println(inline203)
    var t169 int32
    switch paint__3 {
    case Paint_Red:
        t169 = 1
    case Blue:
        t169 = 2
    default:
        panic("non-exhaustive match")
    }
    var t170 string
    var inline200 string = _goml_runtime_core_int32_to_string(t169)
    t170 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t170)
    _goml_runtime_core_string_println(inline197)
    var t171 int32
    t171 = 30
    var t172 string
    var inline194 string = _goml_runtime_core_int32_to_string(t171)
    t172 = inline194
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline191)
    var t173 int32
    t173 = 2
    var t174 string
    var inline188 string = _goml_runtime_core_int32_to_string(t173)
    t174 = inline188
    var inline185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline185)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
