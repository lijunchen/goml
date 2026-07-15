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
    var retv32 Option__int32
    var t35 bool = i__0 == 2
    var jp34 Option__int32
    if t35 {
        jp34 = None{}
    } else {
        var t36 int32 = i__0 + 10
        var t37 Option__int32 = Some{
            _0: t36,
        }
        jp34 = t37
    }
    retv32 = jp34
    return retv32
}

func accumulate(limit__1 int32) Option__int32 {
    var retv39 Option__int32
    var sum__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var i__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop43:
    for {
        var t44 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
        var t45 bool = t44 < limit__1
        if t45 {
            var cur__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__3)
            var t46 int32 = cur__4 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__3, t46)
            var t52 bool = cur__4 == 1
            if t52 {
                continue
            } else {
                var mtmp24 Option__int32 = step(cur__4)
                var jp49 int32
                switch mtmp24.(type) {
                case None:
                    retv39 = None{}
                    return retv39
                case Some:
                    var x25 int32 = mtmp24.(Some)._0
                    var try_value__43 int32 = x25
                    jp49 = try_value__43
                    var value__5 int32 = jp49
                    var t50 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
                    var t51 int32 = t50 + value__5
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(sum__2, t51)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop43
        }
    }
    var t41 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(sum__2)
    var t42 Option__int32 = Some{
        _0: t41,
    }
    retv39 = t42
    return retv39
}

func show(opt__6 Option__int32) string {
    var retv54 string
    var jp56 string
    switch opt__6.(type) {
    case None:
        jp56 = "none"
    case Some:
        var x28 int32 = opt__6.(Some)._0
        var value__7 int32 = x28
        var t57 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t58 string = "some=" + t57
        jp56 = t58
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func main0() struct{} {
    var t60 Option__int32 = accumulate(2)
    var t61 string = show(t60)
    println__T_string(t61)
    var t62 Option__int32 = accumulate(4)
    var t63 string = show(t62)
    println__T_string(t63)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv65 *ref_int32_x
    var t66 *ref_int32_x = ref__Ref_5int32(value__140)
    retv65 = t66
    return retv65
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv68 int32
    var t69 int32 = ref_get__Ref_5int32(self__141)
    retv68 = t69
    return retv68
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__2)
    retv73 = t74
    return retv73
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv79 string
    retv79 = self__9
    return retv79
}

func main() {
    main0()
}
