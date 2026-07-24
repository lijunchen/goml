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

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int32_x
    end_1 int32
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

func _goml_m_range(start__0 int32, end__1 int32) int32 {
    var retv69 int32
    var t70 int32 = start__0 + end__1
    retv69 = t70
    return retv69
}

func main0() struct{} {
    var t72 FnIterator__int32 = __goml_builtin_range(1, 4)
    var for_iter61 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(t72)
    Loop_loop75:
    for {
        if true {
            var for_next62 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter61)
            switch for_next62.(type) {
            case None:
                break Loop_loop75
            case Some:
                var x63 int32 = for_next62.(Some)._0
                var value__2 int32 = x63
                println__T_int32(value__2)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop75
        }
    }
    var t74 int32 = _goml_m_range(10, 20)
    println__T_int32(t74)
    return struct{}{}
}

func __goml_builtin_range(start__208 int32, end__209 int32) FnIterator__int32 {
    var retv78 FnIterator__int32
    var current__210 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__208)
    var t79 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__210,
        end_1: end__209,
    }
    var t80 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t79)
    })
    retv78 = t80
    return retv78
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int32_i_into__iter(self__104 FnIterator__int32) FnIterator__int32 {
    var retv82 FnIterator__int32
    retv82 = self__104
    return retv82
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__97 FnIterator__int32) Option__int32 {
    var retv84 Option__int32
    var t85 func() Option__int32 = self__97.next_fn
    var t86 Option__int32 = t85()
    retv84 = t86
    return retv84
}

func println__T_int32(value__1 int32) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv91 *ref_int32_x
    var t92 *ref_int32_x = ref__Ref_5int32(value__204)
    retv91 = t92
    return retv91
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv94 int32
    var t95 int32 = ref_get__Ref_5int32(self__205)
    retv94 = t95
    return retv94
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__96 func() Option__int32) FnIterator__int32 {
    var retv99 FnIterator__int32
    var t100 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__96,
    }
    retv99 = t100
    return retv99
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv102 string
    var t103 string = _goml_runtime_core_int32_to_string(self__41)
    retv102 = t103
    return retv102
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env67 closure_env_goml_builtin_range_0) Option__int32 {
    var retv111 Option__int32
    var current__210 *ref_int32_x = env67.current_0
    var end__209 int32 = env67.end_1
    var value__211 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__210)
    var t114 bool = value__211 < end__209
    var jp113 Option__int32
    if t114 {
        var t115 int32 = value__211 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__210, t115)
        var t116 Option__int32 = Some{
            _0: value__211,
        }
        jp113 = t116
    } else {
        jp113 = None{}
    }
    retv111 = jp113
    return retv111
}

func main() {
    main0()
}
