package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_15AlwaysDifferent(arr [2]AlwaysDifferent, index int) AlwaysDifferent {
    return arr[index]
}

type Tuple2_15AlwaysDifferent_3int struct {
    _0 AlwaysDifferent
    _1 int
}

type AlwaysDifferent struct {
    value int32
}

func main0() struct{} {
    var t166 bool
    t166 = false
    var inline216 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t166)
    _goml_runtime_core_string_println(inline216)
    var t167 bool
    t167 = false
    var t168 bool = !t167
    var inline212 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t168)
    _goml_runtime_core_string_println(inline212)
    var t187 bool
    t187 = false
    var jp172 bool
    if t187 {
        var t188 int = 2
        var t189 int = 2
        var inline201 bool = t188 == t189
        jp172 = inline201
    } else {
        jp172 = false
    }
    var inline208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp172)
    _goml_runtime_core_string_println(inline208)
    var t173 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t174 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t173, t174}
    var t175 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t176 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t175, t176}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t181 bool
    t181 = false
    var jp178 bool
    if t181 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp178 = false
    } else {
        jp178 = false
    }
    var inline204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp178)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t199 string = _goml_runtime_core_bool_to_string(self__37)
    return t199
}

func main() {
    main0()
}
