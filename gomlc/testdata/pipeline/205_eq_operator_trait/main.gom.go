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
    var t193 bool
    t193 = false
    var inline243 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t193)
    _goml_runtime_core_string_println(inline243)
    var t194 bool
    t194 = false
    var t195 bool = !t194
    var inline239 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t195)
    _goml_runtime_core_string_println(inline239)
    var t214 bool
    t214 = false
    var jp199 bool
    if t214 {
        var t215 int = 2
        var t216 int = 2
        var inline228 bool = t215 == t216
        jp199 = inline228
    } else {
        jp199 = false
    }
    var inline235 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp199)
    _goml_runtime_core_string_println(inline235)
    var t200 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t201 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t200, t201}
    var t202 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t203 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t202, t203}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t208 bool
    t208 = false
    var jp205 bool
    if t208 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp205 = false
    } else {
        jp205 = false
    }
    var inline231 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp205)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t226 string = _goml_runtime_core_bool_to_string(self__64)
    return t226
}

func main() {
    main0()
}
