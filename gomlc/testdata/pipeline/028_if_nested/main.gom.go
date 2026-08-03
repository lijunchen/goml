package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t187 bool = x__0 < 0
    if t187 {
        return "negative"
    } else {
        var t190 bool = 0 < x__0
        if t190 {
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
    var inline241 int32 = 1
    var inline242 int32 = 2
    var inline243 int32 = 3
    var inline244 bool = inline241 < inline242
    if inline244 {
        var inline245 bool = inline242 < inline243
        if inline245 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline246 bool = inline241 < inline243
        if inline246 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline234 int32 = 3
    var inline235 int32 = 2
    var inline236 int32 = 1
    var inline237 bool = inline234 < inline235
    if inline237 {
        var inline238 bool = inline235 < inline236
        if inline238 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline239 bool = inline234 < inline236
        if inline239 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline227 int32 = 2
    var inline228 int32 = 3
    var inline229 int32 = 2
    var inline230 bool = inline227 < inline228
    if inline230 {
        var inline231 bool = inline228 < inline229
        if inline231 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline232 bool = inline227 < inline229
        if inline232 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline224)
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline221)
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline218)
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline215)
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline212)
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
