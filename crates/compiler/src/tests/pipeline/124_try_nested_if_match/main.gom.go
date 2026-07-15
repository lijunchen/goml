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

func maybe_num(flag__0 bool) Option__int32 {
    var retv31 Option__int32
    var jp33 Option__int32
    if flag__0 {
        var t34 Option__int32 = Some{
            _0: 8,
        }
        jp33 = t34
    } else {
        jp33 = None{}
    }
    retv31 = jp33
    return retv31
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var retv36 Option__int32
    var jp38 int32
    if top__1 {
        var jp41 int32
        switch mode__2 {
        case Take:
            var mtmp22 Option__int32 = maybe_num(inner_flag__3)
            var jp43 int32
            switch mtmp22.(type) {
            case None:
                retv36 = None{}
                return retv36
            case Some:
                var x23 int32 = mtmp22.(Some)._0
                var try_value__13 int32 = x23
                jp43 = try_value__13
                var inner__4 int32 = jp43
                var t44 int32 = inner__4 + 1
                jp41 = t44
                jp38 = jp41
                var value__6 int32 = jp38
                var t39 Option__int32 = Some{
                    _0: value__6,
                }
                retv36 = t39
                return retv36
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp41 = 20
            jp38 = jp41
            var value__6 int32 = jp38
            var t39 Option__int32 = Some{
                _0: value__6,
            }
            retv36 = t39
            return retv36
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp24 Option__int32 = maybe_num(inner_flag__3)
        var jp46 int32
        switch mtmp24.(type) {
        case None:
            retv36 = None{}
            return retv36
        case Some:
            var x25 int32 = mtmp24.(Some)._0
            var try_value__24 int32 = x25
            jp46 = try_value__24
            var inner__5 int32 = jp46
            var t47 int32 = inner__5 + 2
            jp38 = t47
            var value__6 int32 = jp38
            var t39 Option__int32 = Some{
                _0: value__6,
            }
            retv36 = t39
            return retv36
        default:
            panic("non-exhaustive match")
        }
    }
}

func show(opt__7 Option__int32) string {
    var retv49 string
    var jp51 string
    switch opt__7.(type) {
    case None:
        jp51 = "none"
    case Some:
        var x26 int32 = opt__7.(Some)._0
        var value__8 int32 = x26
        var t52 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t53 string = "some=" + t52
        jp51 = t53
    default:
        panic("non-exhaustive match")
    }
    retv49 = jp51
    return retv49
}

func main0() struct{} {
    var t55 Option__int32 = nested(true, Take, true)
    var t56 string = show(t55)
    println__T_string(t56)
    var t57 Option__int32 = nested(true, Skip, false)
    var t58 string = show(t57)
    println__T_string(t58)
    var t59 Option__int32 = nested(false, Take, false)
    var t60 string = show(t59)
    println__T_string(t60)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv62 string
    var t63 string = _goml_runtime_core_int32_to_string(self__2)
    retv62 = t63
    return retv62
}

func println__T_string(value__1 string) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv68 string
    retv68 = self__9
    return retv68
}

func main() {
    main0()
}
