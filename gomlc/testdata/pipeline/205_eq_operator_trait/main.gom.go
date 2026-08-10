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
    var t183 bool
    t183 = false
    var inline233 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t183)
    _goml_runtime_core_string_println(inline233)
    var t184 bool
    t184 = false
    var t185 bool = !t184
    var inline229 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    _goml_runtime_core_string_println(inline229)
    var t204 bool
    t204 = false
    var jp189 bool
    if t204 {
        var t205 int = 2
        var t206 int = 2
        var inline218 bool = t205 == t206
        jp189 = inline218
    } else {
        jp189 = false
    }
    var inline225 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp189)
    _goml_runtime_core_string_println(inline225)
    var t190 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t191 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t190, t191}
    var t192 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t193 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t192, t193}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t198 bool
    t198 = false
    var jp195 bool
    if t198 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp195 = false
    } else {
        jp195 = false
    }
    var inline221 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp195)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t216 string = _goml_runtime_core_bool_to_string(self__64)
    return t216
}

func main() {
    main0()
}
