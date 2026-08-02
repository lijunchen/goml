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
    var retv158 int32
    retv158 = 10
    return retv158
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv160 int32
    retv160 = 20
    return retv160
}

func main0() struct{} {
    var t162 S = S{}
    var t163 int32 = pick_a__T_S(t162)
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t163)
    println__T_string(t164)
    var t165 S = S{}
    var t166 int32 = pick_b__T_S(t165)
    var t167 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t166)
    println__T_string(t167)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv172 int32
    var t173 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int32_to_string(self__6)
    retv175 = t176
    return retv175
}

func pick_b__T_S(x__3 S) int32 {
    var retv178 int32
    var t179 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv178 = t179
    return retv178
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv181 string
    retv181 = self__38
    return retv181
}

func main() {
    main0()
}
