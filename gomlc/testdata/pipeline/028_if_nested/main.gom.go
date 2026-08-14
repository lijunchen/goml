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
    var t418 bool = x__0 < 0
    if t418 {
        return "negative"
    } else {
        var t421 bool = 0 < x__0
        if t421 {
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
    var inline472 int32 = 1
    var inline473 int32 = 2
    var inline474 int32 = 3
    var inline475 bool = inline472 < inline473
    if inline475 {
        var inline476 bool = inline473 < inline474
        if inline476 {
            shape1__7 = "ascending"
        } else {
            shape1__7 = "peak"
        }
    } else {
        var inline477 bool = inline472 < inline474
        if inline477 {
            shape1__7 = "valley"
        } else {
            shape1__7 = "flat"
        }
    }
    var shape2__8 string
    var inline465 int32 = 3
    var inline466 int32 = 2
    var inline467 int32 = 1
    var inline468 bool = inline465 < inline466
    if inline468 {
        var inline469 bool = inline466 < inline467
        if inline469 {
            shape2__8 = "ascending"
        } else {
            shape2__8 = "peak"
        }
    } else {
        var inline470 bool = inline465 < inline467
        if inline470 {
            shape2__8 = "valley"
        } else {
            shape2__8 = "flat"
        }
    }
    var shape3__9 string
    var inline458 int32 = 2
    var inline459 int32 = 3
    var inline460 int32 = 2
    var inline461 bool = inline458 < inline459
    if inline461 {
        var inline462 bool = inline459 < inline460
        if inline462 {
            shape3__9 = "ascending"
        } else {
            shape3__9 = "peak"
        }
    } else {
        var inline463 bool = inline458 < inline460
        if inline463 {
            shape3__9 = "valley"
        } else {
            shape3__9 = "flat"
        }
    }
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(first__4)
    _goml_runtime_core_string_println(inline455)
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(second__5)
    _goml_runtime_core_string_println(inline452)
    var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(third__6)
    _goml_runtime_core_string_println(inline449)
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape1__7)
    _goml_runtime_core_string_println(inline446)
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape2__8)
    _goml_runtime_core_string_println(inline443)
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(shape3__9)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
