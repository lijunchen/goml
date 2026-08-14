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

type AlwaysDifferent struct {
    value int32
}

func main0() struct{} {
    var t198 bool
    t198 = false
    var inline248 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t198)
    _goml_runtime_core_string_println(inline248)
    var t199 bool
    t199 = false
    var t200 bool = !t199
    var inline244 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t200)
    _goml_runtime_core_string_println(inline244)
    var t219 bool
    t219 = false
    var jp204 bool
    if t219 {
        var t220 int = 2
        var t221 int = 2
        var inline233 bool = t220 == t221
        jp204 = inline233
    } else {
        jp204 = false
    }
    var inline240 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp204)
    _goml_runtime_core_string_println(inline240)
    var t205 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t206 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t205, t206}
    var t207 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t208 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t207, t208}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t213 bool
    t213 = false
    var jp210 bool
    if t213 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp210 = false
    } else {
        jp210 = false
    }
    var inline236 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp210)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t231 string = _goml_runtime_core_bool_to_string(self__64)
    return t231
}

func main() {
    main0()
}
