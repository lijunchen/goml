package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t197 bool = x__0 < 0
    if t197 {
        return "negative"
    } else {
        var t200 bool = 0 < x__0
        if t200 {
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
    var inline251 int32 = 1
    var inline252 int32 = 2
    var inline253 int32 = 3
    var inline254 bool = inline251 < inline252
    if inline254 {
        var inline255 bool = inline252 < inline253
        if inline255 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline256 bool = inline251 < inline253
        if inline256 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline244 int32 = 3
    var inline245 int32 = 2
    var inline246 int32 = 1
    var inline247 bool = inline244 < inline245
    if inline247 {
        var inline248 bool = inline245 < inline246
        if inline248 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline249 bool = inline244 < inline246
        if inline249 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline237 int32 = 2
    var inline238 int32 = 3
    var inline239 int32 = 2
    var inline240 bool = inline237 < inline238
    if inline240 {
        var inline241 bool = inline238 < inline239
        if inline241 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline242 bool = inline237 < inline239
        if inline242 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline234)
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline231)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline228)
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline225)
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline222)
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
