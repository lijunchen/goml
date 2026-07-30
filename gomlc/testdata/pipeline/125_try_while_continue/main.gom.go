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
    var retv78 Option__int32
    var t81 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(i__0, 2)
    var jp80 Option__int32
    if t81 {
        jp80 = None{}
    } else {
        var t82 int32 = i__0 + 10
        var t83 Option__int32 = Some{
            _0: t82,
        }
        jp80 = t83
    }
    retv78 = jp80
    return retv78
}

func accumulate(limit__1 int32) Option__int32 {
    var retv85 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop89:
    for {
        var t90 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t91 bool = t90 < limit__1
        if t91 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t92 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t92)
            var t98 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(cur__4, 1)
            if t98 {
                continue
            } else {
                var mtmp70 Option__int32 = step(cur__4)
                var jp95 int32
                switch mtmp70.(type) {
                case None:
                    retv85 = None{}
                    return retv85
                case Some:
                    var x71 int32 = mtmp70.(Some)._0
                    var try_value__43 int32 = x71
                    jp95 = try_value__43
                    var value__5 int32 = jp95
                    var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t97 int32 = t96 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t97)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop89
        }
    }
    var t87 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t88 Option__int32 = Some{
        _0: t87,
    }
    retv85 = t88
    return retv85
}

func show(opt__6 Option__int32) string {
    var retv100 string
    var jp102 string
    switch opt__6.(type) {
    case None:
        jp102 = "none"
    case Some:
        var x74 int32 = opt__6.(Some)._0
        var value__7 int32 = x74
        var t103 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t104 string = "some=" + t103
        jp102 = t104
    default:
        panic("non-exhaustive match")
    }
    retv100 = jp102
    return retv100
}

func main0() struct{} {
    var t106 Option__int32 = accumulate(2)
    var t107 string = show(t106)
    println__T_string(t107)
    var t108 Option__int32 = accumulate(4)
    var t109 string = show(t108)
    println__T_string(t109)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv111 bool
    var t112 bool = self__65 == other__66
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv114 *ref_int32_x
    var t115 *ref_int32_x = ref__Ref_5int32(value__207)
    retv114 = t115
    return retv114
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv117 int32
    var t118 int32 = ref_get__Ref_5int32(self__208)
    retv117 = t118
    return retv117
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int32_to_string(self__6)
    retv122 = t123
    return retv122
}

func println__T_string(value__1 string) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func main() {
    main0()
}
