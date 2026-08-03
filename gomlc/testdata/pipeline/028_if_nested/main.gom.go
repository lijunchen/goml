package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t146 bool = x__0 < 0
    if t146 {
        return "negative"
    } else {
        var t149 bool = 0 < x__0
        if t149 {
            return "positive"
        } else {
            return "zero"
        }
    }
}

func main0() struct{} {
    var first__4 string = classify(-42)
    var second__5 string = classify(0)
    var third__6 string = classify(17)
    var shape1__7 string
    var inline200 int32 = 1
    var inline201 int32 = 2
    var inline202 int32 = 3
    var inline203 bool = inline200 < inline201
    if inline203 {
        var inline204 bool = inline201 < inline202
        if inline204 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline205 bool = inline200 < inline202
        if inline205 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline193 int32 = 3
    var inline194 int32 = 2
    var inline195 int32 = 1
    var inline196 bool = inline193 < inline194
    if inline196 {
        var inline197 bool = inline194 < inline195
        if inline197 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline198 bool = inline193 < inline195
        if inline198 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline186 int32 = 2
    var inline187 int32 = 3
    var inline188 int32 = 2
    var inline189 bool = inline186 < inline187
    if inline189 {
        var inline190 bool = inline187 < inline188
        if inline190 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline191 bool = inline186 < inline188
        if inline191 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline183)
    var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline180)
    var inline177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline177)
    var inline174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline174)
    var inline171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline171)
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
