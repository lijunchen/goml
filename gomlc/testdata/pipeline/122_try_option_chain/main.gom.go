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

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_total(flag__2 bool) Option__int32 {
    var mtmp408 Option__int32
    if flag__2 {
        var inline459 Option__int32 = Some{
            _0: 3,
        }
        mtmp408 = inline459
    } else {
        mtmp408 = None{}
    }
    var jp430 int32
    switch mtmp408.(type) {
    case None:
        return None{}
    case Some:
        var x409 int32 = mtmp408.(Some)._0
        jp430 = x409
        var mtmp410 Option__int32
        var inline455 bool = jp430 > 0
        if inline455 {
            var inline456 int32 = jp430 * 2
            var inline457 Option__int32 = Some{
                _0: inline456,
            }
            mtmp410 = inline457
        } else {
            mtmp410 = None{}
        }
        var jp432 int32
        switch mtmp410.(type) {
        case None:
            return None{}
        case Some:
            var x411 int32 = mtmp410.(Some)._0
            jp432 = x411
            var t433 int32 = jp430 + jp432
            var t434 Option__int32 = Some{
                _0: t433,
            }
            return t434
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t442 Option__int32 = maybe_total(true)
    var t443 string
    switch t442.(type) {
    case None:
        t443 = "none"
    case Some:
        var inline474 int32 = t442.(Some)._0
        var inline476 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline474)
        var inline477 string = "some=" + inline476
        t443 = inline477
    default:
        panic("non-exhaustive match")
    }
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline471)
    var t444 Option__int32 = maybe_total(false)
    var t445 string
    switch t444.(type) {
    case None:
        t445 = "none"
    case Some:
        var inline466 int32 = t444.(Some)._0
        var inline468 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline466)
        var inline469 string = "some=" + inline468
        t445 = inline469
    default:
        panic("non-exhaustive match")
    }
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline463)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t448 string = _goml_runtime_core_int32_to_string(self__33)
    return t448
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
