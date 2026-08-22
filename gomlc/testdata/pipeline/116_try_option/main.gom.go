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

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func maybe_value(flag__0 bool) Option__i32 {
    if flag__0 {
        var t420 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 4,
        }
        return t420
    } else {
        return Option__i32{
            _tag: 0,
        }
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t423 int32 = a__1 + b__2
    return t423
}

func main0() struct{} {
    var t437 Option__i32
    var inline482 bool = true
    var inline483 Option__i32 = maybe_value(inline482)
    var inline485 int32
    switch inline483._tag {
    case 0:
        t437 = Option__i32{
            _tag: 0,
        }
        var t438 string
        switch t437._tag {
        case 0:
            t438 = "none"
        case 1:
            var inline477 int32 = t437._v1_0
            var inline479 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline477)
            var inline480 string = "some=" + inline479
            t438 = inline480
        default:
            panic("non-exhaustive match")
        }
        var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
        _goml_runtime_core_string_println(inline474)
        var t439 Option__i32
        var inline465 bool = false
        var inline466 Option__i32 = maybe_value(inline465)
        var inline468 int32
        switch inline466._tag {
        case 0:
            t439 = Option__i32{
                _tag: 0,
            }
            var t440 string
            switch t439._tag {
            case 0:
                t440 = "none"
            case 1:
                var inline460 int32 = t439._v1_0
                var inline462 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline460)
                var inline463 string = "some=" + inline462
                t440 = inline463
            default:
                panic("non-exhaustive match")
            }
            var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
            _goml_runtime_core_string_println(inline457)
            return struct{}{}
        case 1:
            var inline471 int32 = inline466._v1_0
            inline468 = inline471
            var inline469 int32 = add(inline468, 2)
            var inline470 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: inline469,
            }
            t439 = inline470
            var t440 string
            switch t439._tag {
            case 0:
                t440 = "none"
            case 1:
                var inline460 int32 = t439._v1_0
                var inline462 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline460)
                var inline463 string = "some=" + inline462
                t440 = inline463
            default:
                panic("non-exhaustive match")
            }
            var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
            _goml_runtime_core_string_println(inline457)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline488 int32 = inline483._v1_0
        inline485 = inline488
        var inline486 int32 = add(inline485, 2)
        var inline487 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: inline486,
        }
        t437 = inline487
        var t438 string
        switch t437._tag {
        case 0:
            t438 = "none"
        case 1:
            var inline477 int32 = t437._v1_0
            var inline479 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline477)
            var inline480 string = "some=" + inline479
            t438 = inline480
        default:
            panic("non-exhaustive match")
        }
        var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
        _goml_runtime_core_string_println(inline474)
        var t439 Option__i32
        var inline465 bool = false
        var inline466 Option__i32 = maybe_value(inline465)
        var inline468 int32
        switch inline466._tag {
        case 0:
            t439 = Option__i32{
                _tag: 0,
            }
            var t440 string
            switch t439._tag {
            case 0:
                t440 = "none"
            case 1:
                var inline460 int32 = t439._v1_0
                var inline462 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline460)
                var inline463 string = "some=" + inline462
                t440 = inline463
            default:
                panic("non-exhaustive match")
            }
            var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
            _goml_runtime_core_string_println(inline457)
            return struct{}{}
        case 1:
            var inline471 int32 = inline466._v1_0
            inline468 = inline471
            var inline469 int32 = add(inline468, 2)
            var inline470 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: inline469,
            }
            t439 = inline470
            var t440 string
            switch t439._tag {
            case 0:
                t440 = "none"
            case 1:
                var inline460 int32 = t439._v1_0
                var inline462 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline460)
                var inline463 string = "some=" + inline462
                t440 = inline463
            default:
                panic("non-exhaustive match")
            }
            var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
            _goml_runtime_core_string_println(inline457)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t443 string = _goml_runtime_core_int32_to_string(self__33)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
