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

type Mode int32

const (
    Take Mode = 0
    Skip Mode = 1
)

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var jp203 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp187 Option__int32
            if inner_flag__3 {
                var inline235 Option__int32 = Some{
                    _0: 8,
                }
                mtmp187 = inline235
            } else {
                mtmp187 = None{}
            }
            var jp208 int32
            switch mtmp187.(type) {
            case None:
                return None{}
            case Some:
                var x188 int32 = mtmp187.(Some)._0
                jp208 = x188
                var t209 int32 = jp208 + 1
                jp203 = t209
                var t204 Option__int32 = Some{
                    _0: jp203,
                }
                return t204
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp203 = 20
            var t204 Option__int32 = Some{
                _0: jp203,
            }
            return t204
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp189 Option__int32
        if inner_flag__3 {
            var inline237 Option__int32 = Some{
                _0: 8,
            }
            mtmp189 = inline237
        } else {
            mtmp189 = None{}
        }
        var jp211 int32
        switch mtmp189.(type) {
        case None:
            return None{}
        case Some:
            var x190 int32 = mtmp189.(Some)._0
            jp211 = x190
            var t212 int32 = jp211 + 2
            jp203 = t212
            var t204 Option__int32 = Some{
                _0: jp203,
            }
            return t204
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t220 Option__int32 = nested(true, Take, true)
    var t221 string
    switch t220.(type) {
    case None:
        t221 = "none"
    case Some:
        var inline260 int32 = t220.(Some)._0
        var inline262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline260)
        var inline263 string = "some=" + inline262
        t221 = inline263
    default:
        panic("non-exhaustive match")
    }
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline257)
    var t222 Option__int32 = nested(true, Skip, false)
    var t223 string
    switch t222.(type) {
    case None:
        t223 = "none"
    case Some:
        var inline252 int32 = t222.(Some)._0
        var inline254 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline252)
        var inline255 string = "some=" + inline254
        t223 = inline255
    default:
        panic("non-exhaustive match")
    }
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline249)
    var t224 Option__int32 = nested(false, Take, false)
    var t225 string
    switch t224.(type) {
    case None:
        t225 = "none"
    case Some:
        var inline244 int32 = t224.(Some)._0
        var inline246 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline244)
        var inline247 string = "some=" + inline246
        t225 = inline247
    default:
        panic("non-exhaustive match")
    }
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t228 string = _goml_runtime_core_int32_to_string(self__33)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
