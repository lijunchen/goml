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
    var retv155 int32
    retv155 = 10
    return retv155
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv157 int32
    retv157 = 20
    return retv157
}

func main0() struct{} {
    var t159 S = S{}
    var t160 int32 = pick_a__T_S(t159)
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t160)
    println__T_string(t161)
    var t162 S = S{}
    var t163 int32 = pick_b__T_S(t162)
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t163)
    println__T_string(t164)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv169 int32
    var t170 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv172 string
    var t173 string = _goml_runtime_core_int32_to_string(self__6)
    retv172 = t173
    return retv172
}

func pick_b__T_S(x__3 S) int32 {
    var retv175 int32
    var t176 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv178 string
    retv178 = self__38
    return retv178
}

func main() {
    main0()
}
