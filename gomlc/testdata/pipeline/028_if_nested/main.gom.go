package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t192 bool = x__0 < 0
    if t192 {
        return "negative"
    } else {
        var t195 bool = 0 < x__0
        if t195 {
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
    var inline246 int32 = 1
    var inline247 int32 = 2
    var inline248 int32 = 3
    var inline249 bool = inline246 < inline247
    if inline249 {
        var inline250 bool = inline247 < inline248
        if inline250 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline251 bool = inline246 < inline248
        if inline251 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline239 int32 = 3
    var inline240 int32 = 2
    var inline241 int32 = 1
    var inline242 bool = inline239 < inline240
    if inline242 {
        var inline243 bool = inline240 < inline241
        if inline243 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline244 bool = inline239 < inline241
        if inline244 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline232 int32 = 2
    var inline233 int32 = 3
    var inline234 int32 = 2
    var inline235 bool = inline232 < inline233
    if inline235 {
        var inline236 bool = inline233 < inline234
        if inline236 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline237 bool = inline232 < inline234
        if inline237 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline229)
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline226)
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline223)
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline220)
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline217)
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
