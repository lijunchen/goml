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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Tuple2_5Color_5Color struct {
    _0 Color
    _1 Color
}

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() bool {
    var retv159 bool
    var a__0 Tuple2_5Color_5Color = Tuple2_5Color_5Color{
        _0: Blue,
        _1: Blue,
    }
    var x155 Color = a__0._0
    var x156 Color = a__0._1
    var jp161 bool
    switch x156 {
    case Red:
        var jp163 bool
        switch x155 {
        case Red:
            jp163 = true
        default:
            jp163 = false
        }
        jp161 = jp163
    case Green:
        var jp165 bool
        switch x155 {
        case Red:
            jp165 = true
        default:
            jp165 = false
        }
        jp161 = jp165
    case Blue:
        var jp167 bool
        switch x155 {
        case Blue:
            print__T_bool(true)
            jp167 = false
        default:
            jp167 = false
        }
        jp161 = jp167
    default:
        panic("non-exhaustive match")
    }
    retv159 = jp161
    return retv159
}

func print__T_bool(value__0 bool) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv172 string
    var t173 string = _goml_runtime_core_bool_to_string(self__37)
    retv172 = t173
    return retv172
}

func main() {
    main0()
}
