package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var t165 bool = x__0 < 0
    if t165 {
        return "negative"
    } else {
        var t168 bool = 0 < x__0
        if t168 {
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
    var inline219 int32 = 1
    var inline220 int32 = 2
    var inline221 int32 = 3
    var inline222 bool = inline219 < inline220
    if inline222 {
        var inline223 bool = inline220 < inline221
        if inline223 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline224 bool = inline219 < inline221
        if inline224 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline212 int32 = 3
    var inline213 int32 = 2
    var inline214 int32 = 1
    var inline215 bool = inline212 < inline213
    if inline215 {
        var inline216 bool = inline213 < inline214
        if inline216 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline217 bool = inline212 < inline214
        if inline217 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline205 int32 = 2
    var inline206 int32 = 3
    var inline207 int32 = 2
    var inline208 bool = inline205 < inline206
    if inline208 {
        var inline209 bool = inline206 < inline207
        if inline209 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline210 bool = inline205 < inline207
        if inline210 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline202)
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline199)
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline196)
    var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline193)
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline190)
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
