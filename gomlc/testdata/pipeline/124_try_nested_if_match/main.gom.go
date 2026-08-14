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
    var jp198 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp182 Option__int32
            if inner_flag__3 {
                var inline230 Option__int32 = Some{
                    _0: 8,
                }
                mtmp182 = inline230
            } else {
                mtmp182 = None{}
            }
            var jp203 int32
            switch mtmp182.(type) {
            case None:
                return None{}
            case Some:
                var x183 int32 = mtmp182.(Some)._0
                jp203 = x183
                var t204 int32 = jp203 + 1
                jp198 = t204
                var t199 Option__int32 = Some{
                    _0: jp198,
                }
                return t199
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp198 = 20
            var t199 Option__int32 = Some{
                _0: jp198,
            }
            return t199
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp184 Option__int32
        if inner_flag__3 {
            var inline232 Option__int32 = Some{
                _0: 8,
            }
            mtmp184 = inline232
        } else {
            mtmp184 = None{}
        }
        var jp206 int32
        switch mtmp184.(type) {
        case None:
            return None{}
        case Some:
            var x185 int32 = mtmp184.(Some)._0
            jp206 = x185
            var t207 int32 = jp206 + 2
            jp198 = t207
            var t199 Option__int32 = Some{
                _0: jp198,
            }
            return t199
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t215 Option__int32 = nested(true, Take, true)
    var t216 string
    switch t215.(type) {
    case None:
        t216 = "none"
    case Some:
        var inline255 int32 = t215.(Some)._0
        var inline257 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline255)
        var inline258 string = "some=" + inline257
        t216 = inline258
    default:
        panic("non-exhaustive match")
    }
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline252)
    var t217 Option__int32 = nested(true, Skip, false)
    var t218 string
    switch t217.(type) {
    case None:
        t218 = "none"
    case Some:
        var inline247 int32 = t217.(Some)._0
        var inline249 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline247)
        var inline250 string = "some=" + inline249
        t218 = inline250
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline244)
    var t219 Option__int32 = nested(false, Take, false)
    var t220 string
    switch t219.(type) {
    case None:
        t220 = "none"
    case Some:
        var inline239 int32 = t219.(Some)._0
        var inline241 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline239)
        var inline242 string = "some=" + inline241
        t220 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t223 string = _goml_runtime_core_int32_to_string(self__33)
    return t223
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
