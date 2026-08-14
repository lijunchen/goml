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

type Ordering int32

type Option__int32 interface {
    isOption__int32()
}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type None struct {}

func (_ None) isOption__int32() {}

func maybe_value(flag__0 bool) Option__int32 {
    if flag__0 {
        var t416 Option__int32 = Some{
            _0: 41,
        }
        return t416
    } else {
        return None{}
    }
}

func main0() struct{} {
    var t429 Option__int32
    var inline471 bool = true
    var inline472 Option__int32 = maybe_value(inline471)
    var inline474 int32
    switch inline472.(type) {
    case Some:
        var inline478 int32 = inline472.(Some)._0
        inline474 = inline478
        var inline476 int32 = inline474 + 1
        var inline477 Option__int32 = Some{
            _0: inline476,
        }
        t429 = inline477
        var t430 string
        switch t429.(type) {
        case Some:
            var inline467 int32 = t429.(Some)._0
            var inline469 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline467)
            t430 = inline469
        case None:
            t430 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
        _goml_runtime_core_string_println(inline464)
        var t431 Option__int32
        var inline454 bool = false
        var inline455 Option__int32 = maybe_value(inline454)
        var inline457 int32
        switch inline455.(type) {
        case Some:
            var inline461 int32 = inline455.(Some)._0
            inline457 = inline461
            var inline459 int32 = inline457 + 1
            var inline460 Option__int32 = Some{
                _0: inline459,
            }
            t431 = inline460
            var t432 string
            switch t431.(type) {
            case Some:
                var inline450 int32 = t431.(Some)._0
                var inline452 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline450)
                t432 = inline452
            case None:
                t432 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        case None:
            t431 = None{}
            var t432 string
            switch t431.(type) {
            case Some:
                var inline450 int32 = t431.(Some)._0
                var inline452 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline450)
                t432 = inline452
            case None:
                t432 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case None:
        t429 = None{}
        var t430 string
        switch t429.(type) {
        case Some:
            var inline467 int32 = t429.(Some)._0
            var inline469 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline467)
            t430 = inline469
        case None:
            t430 = "none"
        default:
            panic("non-exhaustive match")
        }
        var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
        _goml_runtime_core_string_println(inline464)
        var t431 Option__int32
        var inline454 bool = false
        var inline455 Option__int32 = maybe_value(inline454)
        var inline457 int32
        switch inline455.(type) {
        case Some:
            var inline461 int32 = inline455.(Some)._0
            inline457 = inline461
            var inline459 int32 = inline457 + 1
            var inline460 Option__int32 = Some{
                _0: inline459,
            }
            t431 = inline460
            var t432 string
            switch t431.(type) {
            case Some:
                var inline450 int32 = t431.(Some)._0
                var inline452 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline450)
                t432 = inline452
            case None:
                t432 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        case None:
            t431 = None{}
            var t432 string
            switch t431.(type) {
            case Some:
                var inline450 int32 = t431.(Some)._0
                var inline452 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline450)
                t432 = inline452
            case None:
                t432 = "none"
            default:
                panic("non-exhaustive match")
            }
            var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline447)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t436 string = _goml_runtime_core_int32_to_string(self__33)
    return t436
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
