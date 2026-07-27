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
    var retv74 Option__int32
    var t77 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(i__0, 2)
    var jp76 Option__int32
    if t77 {
        jp76 = None{}
    } else {
        var t78 int32 = i__0 + 10
        var t79 Option__int32 = Some{
            _0: t78,
        }
        jp76 = t79
    }
    retv74 = jp76
    return retv74
}

func accumulate(limit__1 int32) Option__int32 {
    var retv81 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop85:
    for {
        var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t87 bool = t86 < limit__1
        if t87 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t88 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t88)
            var t94 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 1)
            if t94 {
                continue
            } else {
                var mtmp66 Option__int32 = step(cur__4)
                var jp91 int32
                switch mtmp66.(type) {
                case None:
                    retv81 = None{}
                    return retv81
                case Some:
                    var x67 int32 = mtmp66.(Some)._0
                    var try_value__43 int32 = x67
                    jp91 = try_value__43
                    var value__5 int32 = jp91
                    var t92 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t93 int32 = t92 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t93)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop85
        }
    }
    var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t84 Option__int32 = Some{
        _0: t83,
    }
    retv81 = t84
    return retv81
}

func show(opt__6 Option__int32) string {
    var retv96 string
    var jp98 string
    switch opt__6.(type) {
    case None:
        jp98 = "none"
    case Some:
        var x70 int32 = opt__6.(Some)._0
        var value__7 int32 = x70
        var t99 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t100 string = "some=" + t99
        jp98 = t100
    default:
        panic("non-exhaustive match")
    }
    retv96 = jp98
    return retv96
}

func main0() struct{} {
    var t102 Option__int32 = accumulate(2)
    var t103 string = show(t102)
    println__T_string(t103)
    var t104 Option__int32 = accumulate(4)
    var t105 string = show(t104)
    println__T_string(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv107 bool
    var t108 bool = self__65 == other__66
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv110 *ref_int32_x
    var t111 *ref_int32_x = ref__Ref_5int32(value__209)
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv113 int32
    var t114 int32 = ref_get__Ref_5int32(self__210)
    retv113 = t114
    return retv113
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int32_to_string(self__6)
    retv118 = t119
    return retv118
}

func println__T_string(value__1 string) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv124 string
    retv124 = self__38
    return retv124
}

func main() {
    main0()
}
