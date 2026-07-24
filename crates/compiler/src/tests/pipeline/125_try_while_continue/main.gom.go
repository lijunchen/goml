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
    var retv71 Option__int32
    var t74 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(i__0, 2)
    var jp73 Option__int32
    if t74 {
        jp73 = None{}
    } else {
        var t75 int32 = i__0 + 10
        var t76 Option__int32 = Some{
            _0: t75,
        }
        jp73 = t76
    }
    retv71 = jp73
    return retv71
}

func accumulate(limit__1 int32) Option__int32 {
    var retv78 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop82:
    for {
        var t83 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t84 bool = t83 < limit__1
        if t84 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t85 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t85)
            var t91 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 1)
            if t91 {
                continue
            } else {
                var mtmp63 Option__int32 = step(cur__4)
                var jp88 int32
                switch mtmp63.(type) {
                case None:
                    retv78 = None{}
                    return retv78
                case Some:
                    var x64 int32 = mtmp63.(Some)._0
                    var try_value__43 int32 = x64
                    jp88 = try_value__43
                    var value__5 int32 = jp88
                    var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t90 int32 = t89 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t90)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop82
        }
    }
    var t80 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t81 Option__int32 = Some{
        _0: t80,
    }
    retv78 = t81
    return retv78
}

func show(opt__6 Option__int32) string {
    var retv93 string
    var jp95 string
    switch opt__6.(type) {
    case None:
        jp95 = "none"
    case Some:
        var x67 int32 = opt__6.(Some)._0
        var value__7 int32 = x67
        var t96 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t97 string = "some=" + t96
        jp95 = t97
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t99 Option__int32 = accumulate(2)
    var t100 string = show(t99)
    println__T_string(t100)
    var t101 Option__int32 = accumulate(4)
    var t102 string = show(t101)
    println__T_string(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv104 bool
    var t105 bool = self__61 == other__62
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv107 *ref_int32_x
    var t108 *ref_int32_x = ref__Ref_5int32(value__204)
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv110 int32
    var t111 int32 = ref_get__Ref_5int32(self__205)
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv115 string
    var t116 string = _goml_runtime_core_int32_to_string(self__5)
    retv115 = t116
    return retv115
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv121 string
    retv121 = self__37
    return retv121
}

func main() {
    main0()
}
