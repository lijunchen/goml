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
    var t147 bool
    t147 = false
    var inline197 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t147)
    _goml_runtime_core_string_println(inline197)
    var t148 bool
    t148 = false
    var t149 bool = !t148
    var inline193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t149)
    _goml_runtime_core_string_println(inline193)
    var t168 bool
    t168 = false
    var jp153 bool
    if t168 {
        var t169 int = 2
        var t170 int = 2
        var inline182 bool = t169 == t170
        jp153 = inline182
    } else {
        jp153 = false
    }
    var inline189 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp153)
    _goml_runtime_core_string_println(inline189)
    var t154 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t155 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t154, t155}
    var t156 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t157 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t156, t157}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t162 bool
    t162 = false
    var jp159 bool
    if t162 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp159 = false
    } else {
        jp159 = false
    }
    var inline185 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp159)
    _goml_runtime_core_string_println(inline185)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t180 string = _goml_runtime_core_bool_to_string(self__66)
    return t180
}

func main() {
    main0()
}
