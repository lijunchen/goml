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

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    return 10
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    return 20
}

func main0() struct{} {
    var t162 S = S{}
    var t163 int32
    var inline195 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t162)
    t163 = inline195
    var t164 string
    var inline193 string = _goml_runtime_core_int32_to_string(t163)
    t164 = inline193
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline190)
    var t165 S = S{}
    var t166 int32
    var inline188 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t165)
    t166 = inline188
    var t167 string
    var inline186 string = _goml_runtime_core_int32_to_string(t166)
    t167 = inline186
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline183)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
