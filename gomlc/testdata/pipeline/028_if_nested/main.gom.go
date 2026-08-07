package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t182 bool = x__0 < 0
    if t182 {
        return "negative"
    } else {
        var t185 bool = 0 < x__0
        if t185 {
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
    var inline236 int32 = 1
    var inline237 int32 = 2
    var inline238 int32 = 3
    var inline239 bool = inline236 < inline237
    if inline239 {
        var inline240 bool = inline237 < inline238
        if inline240 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline241 bool = inline236 < inline238
        if inline241 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline229 int32 = 3
    var inline230 int32 = 2
    var inline231 int32 = 1
    var inline232 bool = inline229 < inline230
    if inline232 {
        var inline233 bool = inline230 < inline231
        if inline233 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline234 bool = inline229 < inline231
        if inline234 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline222 int32 = 2
    var inline223 int32 = 3
    var inline224 int32 = 2
    var inline225 bool = inline222 < inline223
    if inline225 {
        var inline226 bool = inline223 < inline224
        if inline226 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline227 bool = inline222 < inline224
        if inline227 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline219)
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline216)
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline213)
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline210)
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline207)
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
