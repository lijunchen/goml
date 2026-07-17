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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func step(i__0 int32) Option__int32 {
    var retv68 Option__int32
    var t71 bool = i__0 == 2
    var jp70 Option__int32
    if t71 {
        jp70 = None{}
    } else {
        var t72 int32 = i__0 + 10
        var t73 Option__int32 = Some{
            _0: t72,
        }
        jp70 = t73
    }
    retv68 = jp70
    return retv68
}

func accumulate(limit__1 int32) Option__int32 {
    var retv75 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop79:
    for {
        var t80 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t81 bool = t80 < limit__1
        if t81 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t82 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t82)
            var t88 bool = cur__4 == 1
            if t88 {
                continue
            } else {
                var mtmp60 Option__int32 = step(cur__4)
                var jp85 int32
                switch mtmp60.(type) {
                case None:
                    retv75 = None{}
                    return retv75
                case Some:
                    var x61 int32 = mtmp60.(Some)._0
                    var try_value__43 int32 = x61
                    jp85 = try_value__43
                    var value__5 int32 = jp85
                    var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t87 int32 = t86 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t87)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop79
        }
    }
    var t77 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t78 Option__int32 = Some{
        _0: t77,
    }
    retv75 = t78
    return retv75
}

func show(opt__6 Option__int32) string {
    var retv90 string
    var jp92 string
    switch opt__6.(type) {
    case None:
        jp92 = "none"
    case Some:
        var x64 int32 = opt__6.(Some)._0
        var value__7 int32 = x64
        var t93 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t94 string = "some=" + t93
        jp92 = t94
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func main0() struct{} {
    var t96 Option__int32 = accumulate(2)
    var t97 string = show(t96)
    println__T_string(t97)
    var t98 Option__int32 = accumulate(4)
    var t99 string = show(t98)
    println__T_string(t99)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv101 *ref_int32_x
    var t102 *ref_int32_x = ref__Ref_5int32(value__201)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv104 int32
    var t105 int32 = ref_get__Ref_5int32(self__202)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv109 string
    var t110 string = _goml_runtime_core_int32_to_string(self__2)
    retv109 = t110
    return retv109
}

func println__T_string(value__1 string) struct{} {
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv115 string
    retv115 = self__34
    return retv115
}

func main() {
    main0()
}
