package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func classify(x__0 int32) string {
    var t421 bool = x__0 < 0
    if t421 {
        return "negative"
    } else {
        var t424 bool = 0 < x__0
        if t424 {
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
    var inline475 int32 = 1
    var inline476 int32 = 2
    var inline477 int32 = 3
    var inline478 bool = inline475 < inline476
    if inline478 {
        var inline479 bool = inline476 < inline477
        if inline479 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline480 bool = inline475 < inline477
        if inline480 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline468 int32 = 3
    var inline469 int32 = 2
    var inline470 int32 = 1
    var inline471 bool = inline468 < inline469
    if inline471 {
        var inline472 bool = inline469 < inline470
        if inline472 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline473 bool = inline468 < inline470
        if inline473 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline461 int32 = 2
    var inline462 int32 = 3
    var inline463 int32 = 2
    var inline464 bool = inline461 < inline462
    if inline464 {
        var inline465 bool = inline462 < inline463
        if inline465 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline466 bool = inline461 < inline463
        if inline466 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline458 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline458)
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline455)
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline452)
    var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline449)
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline446)
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline443)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
