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

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var mtmp136 Option__int32
    if primary__2 {
        var inline194 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp136 = inline194
    } else {
        mtmp136 = Option__int32_None{}
    }
    var jp157 int32
    switch mtmp136.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x137 int32 = mtmp136.(Option__int32_Some)._0
        jp157 = x137
        var mtmp138 Option__int32
        if secondary__3 {
            var inline192 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp138 = inline192
        } else {
            mtmp138 = Option__int32_None{}
        }
        var jp159 string
        switch mtmp138.(type) {
        case Option__int32_None:
            jp159 = "extra=none"
        case Option__int32_Some:
            var x139 int32 = mtmp138.(Option__int32_Some)._0
            var t165 string
            var inline188 string = _goml_runtime_core_int32_to_string(x139)
            t165 = inline188
            var t166 string = "extra=" + t165
            jp159 = t166
        default:
            panic("non-exhaustive match")
        }
        var t160 string
        var inline190 string = _goml_runtime_core_int32_to_string(jp157)
        t160 = inline190
        var t161 string = "value=" + t160
        var t162 string = t161 + ","
        var t163 string = t162 + jp159
        var t164 Option__string = Option__string_Some{
            _0: t163,
        }
        return t164
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t173 Option__string = mixed(true, true)
    var t174 string
    switch t173.(type) {
    case Option__string_None:
        t174 = "none"
    case Option__string_Some:
        var inline213 string = t173.(Option__string_Some)._0
        var inline215 string = "some=" + inline213
        t174 = inline215
    default:
        panic("non-exhaustive match")
    }
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline210)
    var t175 Option__string = mixed(true, false)
    var t176 string
    switch t175.(type) {
    case Option__string_None:
        t176 = "none"
    case Option__string_Some:
        var inline206 string = t175.(Option__string_Some)._0
        var inline208 string = "some=" + inline206
        t176 = inline208
    default:
        panic("non-exhaustive match")
    }
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline203)
    var t177 Option__string = mixed(false, true)
    var t178 string
    switch t177.(type) {
    case Option__string_None:
        t178 = "none"
    case Option__string_Some:
        var inline199 string = t177.(Option__string_Some)._0
        var inline201 string = "some=" + inline199
        t178 = inline201
    default:
        panic("non-exhaustive match")
    }
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
