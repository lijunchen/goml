package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func _goml_m_range(start__0 int32, end__1 int32) int32 {
    var retv72 int32
    var t73 int32 = start__0 + end__1
    retv72 = t73
    return retv72
}

func main0() struct{} {
    var t75 FnIterator__int = __goml_builtin_range(1, 4)
    var for_iter64 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t75)
    Loop_loop78:
    for {
        if true {
            var for_next65 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter64)
            switch for_next65.(type) {
            case None:
                break Loop_loop78
            case Some:
                var x66 int = for_next65.(Some)._0
                var value__2 int = x66
                println__T_int(value__2)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop78
        }
    }
    var t77 int32 = _goml_m_range(10, 20)
    println__T_int32(t77)
    return struct{}{}
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv81 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t82 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__222,
        end_1: end__221,
    }
    var t83 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t82)
    })
    retv81 = t83
    return retv81
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv85 FnIterator__int
    retv85 = self__109
    return retv85
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv87 Option__int
    var t88 func() Option__int = self__102.next_fn
    var t89 Option__int = t88()
    retv87 = t89
    return retv87
}

func println__T_int(value__1 int) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv97 *ref_int_x
    var t98 *ref_int_x = ref__Ref_3int(value__209)
    retv97 = t98
    return retv97
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv100 int
    var t101 int = ref_get__Ref_3int(self__210)
    retv100 = t101
    return retv100
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv105 FnIterator__int
    var t106 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv105 = t106
    return retv105
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv108 string
    var t109 string = _goml_runtime_core_int_to_string(self__40)
    retv108 = t109
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv111 string
    var t112 string = _goml_runtime_core_int32_to_string(self__43)
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env70 closure_env_goml_builtin_range_0) Option__int {
    var retv120 Option__int
    var current__222 *ref_int_x = env70.current_0
    var end__221 int = env70.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t123 bool = value__223 < end__221
    var jp122 Option__int
    if t123 {
        var t124 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t124)
        var t125 Option__int = Some{
            _0: value__223,
        }
        jp122 = t125
    } else {
        jp122 = None{}
    }
    retv120 = jp122
    return retv120
}

func main() {
    main0()
}
