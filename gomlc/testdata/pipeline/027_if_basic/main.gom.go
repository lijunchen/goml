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

func main0() struct{} {
    var yes__3 int32
    var inline189 bool = true
    var inline190 int32 = 10
    var inline191 int32 = 99
    if inline189 {
        yes__3 = inline190
    } else {
        yes__3 = inline191
    }
    var no__4 int32
    var inline185 bool = false
    var inline186 int32 = 10
    var inline187 int32 = 99
    if inline185 {
        no__4 = inline186
    } else {
        no__4 = inline187
    }
    var t162 string
    var inline183 string = _goml_runtime_core_int32_to_string(yes__3)
    t162 = inline183
    var t163 string = "yes=" + t162
    var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline180)
    var t164 string
    var inline178 string = _goml_runtime_core_int32_to_string(no__4)
    t164 = inline178
    var t165 string = "no=" + t164
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline175)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
