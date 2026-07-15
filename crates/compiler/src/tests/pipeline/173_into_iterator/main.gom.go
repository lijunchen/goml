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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
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

type Numbers struct {
    values *_goml_vec_int32
    conversions *ref_int32_x
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 *_goml_vec_int32
}

type closure_env_inherent_Slice_Slice_T_iter_T_int32_1 struct {
    index_0 *ref_int32_x
    len_1 int32
    self_2 []int32
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

func _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(self__0 Numbers) FnIterator__int32 {
    var retv43 FnIterator__int32
    var t44 *ref_int32_x = self__0.conversions
    var t45 *ref_int32_x = self__0.conversions
    var t46 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t45)
    var t47 int32 = t46 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t44, t47)
    var t48 *_goml_vec_int32 = self__0.values
    var t49 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t48)
    retv43 = t49
    return retv43
}

func make_numbers(builds__1 *ref_int32_x, conversions__2 *ref_int32_x) Numbers {
    var retv51 Numbers
    var t52 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__1)
    var t53 int32 = t52 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(builds__1, t53)
    var values__3 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__3, 3)
    var t54 Numbers = Numbers{
        values: values__3,
        conversions: conversions__2,
    }
    retv51 = t54
    return retv51
}

func main0() struct{} {
    var builds__7 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var conversions__8 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t56 Numbers = make_numbers(builds__7, conversions__8)
    var t57 int32 = sum__S_Numbers(t56)
    println__T_int32(t57)
    var t58 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(builds__7)
    println__T_int32(t58)
    var t59 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(conversions__8)
    println__T_int32(t59)
    var values__9 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__9, 30)
    var t60 int32 = _goml_m_sum____S__Vec_l_int32_r_(values__9)
    println__T_int32(t60)
    var t61 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(values__9, 1, 3)
    var t62 int32 = _goml_m_sum____S__Slice_l_int32_r_(t61)
    println__T_int32(t62)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv64 int32
    var t65 int32 = ref_get__Ref_5int32(self__141)
    retv64 = t65
    return retv64
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__112 *_goml_vec_int32) FnIterator__int32 {
    var retv69 FnIterator__int32
    var index__113 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__114 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__112)
    var t70 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__113,
        len_1: len__114,
        self_2: self__112,
    }
    var t71 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t70)
    })
    retv69 = t71
    return retv69
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv73 *_goml_vec_int32
    var t74 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv73 = t74
    return retv73
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__97 *_goml_vec_int32, elem__98 int32) struct{} {
    vec_push__Vec_5int32(self__97, elem__98)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv78 *ref_int32_x
    var t79 *ref_int32_x = ref__Ref_5int32(value__140)
    retv78 = t79
    return retv78
}

func println__T_int32(value__1 int32) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func sum__S_Numbers(source__4 Numbers) int32 {
    var retv84 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter27 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Numbers_i_into__iter(source__4)
    Loop_loop87:
    for {
        if true {
            var for_next28 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter27)
            switch for_next28.(type) {
            case None:
                break Loop_loop87
            case Some:
                var x29 int32 = for_next28.(Some)._0
                var value__6 int32 = x29
                var t89 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t90 int32 = t89 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t90)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop87
        }
    }
    var t86 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv84 = t86
    return retv84
}

func _goml_m_sum____S__Vec_l_int32_r_(source__4 *_goml_vec_int32) int32 {
    var retv92 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter27 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(source__4)
    Loop_loop95:
    for {
        if true {
            var for_next28 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter27)
            switch for_next28.(type) {
            case None:
                break Loop_loop95
            case Some:
                var x29 int32 = for_next28.(Some)._0
                var value__6 int32 = x29
                var t97 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t98 int32 = t97 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t98)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop95
        }
    }
    var t94 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv92 = t94
    return retv92
}

func _goml_m_sum____S__Slice_l_int32_r_(source__4 []int32) int32 {
    var retv100 int32
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var for_iter27 FnIterator__int32 = _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(source__4)
    Loop_loop103:
    for {
        if true {
            var for_next28 Option__int32 = _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(for_iter27)
            switch for_next28.(type) {
            case None:
                break Loop_loop103
            case Some:
                var x29 int32 = for_next28.(Some)._0
                var value__6 int32 = x29
                var t105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t106 int32 = t105 + value__6
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t106)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop103
        }
    }
    var t102 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    retv100 = t102
    return retv100
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__109 *_goml_vec_int32, start__110 int32, end__111 int32) []int32 {
    var retv108 []int32
    var t109 []int32 = self__109.items[start__110:end__111]
    retv108 = t109
    return retv108
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__108 *_goml_vec_int32) int32 {
    var retv111 int32
    var t112 int32 = vec_len__Vec_5int32(self__108)
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__103 *_goml_vec_int32, index__104 int32) int32 {
    var retv114 int32
    var t115 int32 = vec_get__Vec_5int32(self__103, index__104)
    retv114 = t115
    return retv114
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__71 func() Option__int32) FnIterator__int32 {
    var retv117 FnIterator__int32
    var t118 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__71,
    }
    retv117 = t118
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__13)
    retv120 = t121
    return retv120
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int32_i_next(self__72 FnIterator__int32) Option__int32 {
    var retv123 Option__int32
    var t124 func() Option__int32 = self__72.next_fn
    var t125 Option__int32 = t124()
    retv123 = t125
    return retv123
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_int32_r__i_into__iter(self__117 *_goml_vec_int32) FnIterator__int32 {
    var retv127 FnIterator__int32
    var t128 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__117)
    retv127 = t128
    return retv127
}

func _goml_m_trait__impl_i_IntoIterator_i_Slice_l_int32_r__i_into__iter(self__129 []int32) FnIterator__int32 {
    var retv130 FnIterator__int32
    var t131 FnIterator__int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__129)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_iter____T__int32(self__124 []int32) FnIterator__int32 {
    var retv133 FnIterator__int32
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__126 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__124)
    var t134 closure_env_inherent_Slice_Slice_T_iter_T_int32_1 = closure_env_inherent_Slice_Slice_T_iter_T_int32_1{
        index_0: index__125,
        len_1: len__126,
        self_2: self__124,
    }
    var t135 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(t134)
    })
    retv133 = t135
    return retv133
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__120 []int32) int32 {
    var retv137 int32
    var t138 int32 = int32(len(self__120))
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__118 []int32, index__119 int32) int32 {
    var retv140 int32
    var t141 int32 = self__118[index__119]
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env40 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var retv149 Option__int32
    var index__113 *ref_int32_x = env40.index_0
    var len__114 int32 = env40.len_1
    var self__112 *_goml_vec_int32 = env40.self_2
    var current__115 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__113)
    var t152 bool = current__115 < len__114
    var jp151 Option__int32
    if t152 {
        var value__116 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__112, current__115)
        var t153 int32 = current__115 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__113, t153)
        var t154 Option__int32 = Some{
            _0: value__116,
        }
        jp151 = t154
    } else {
        jp151 = None{}
    }
    retv149 = jp151
    return retv149
}

func _goml_m_inherent_i_closure__en_h229258edbe561187b6c6c2ae0c5f71a3_nt32__1_i_apply(env41 closure_env_inherent_Slice_Slice_T_iter_T_int32_1) Option__int32 {
    var retv156 Option__int32
    var index__125 *ref_int32_x = env41.index_0
    var len__126 int32 = env41.len_1
    var self__124 []int32 = env41.self_2
    var current__127 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
    var t159 bool = current__127 < len__126
    var jp158 Option__int32
    if t159 {
        var value__128 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_get____T__int32(self__124, current__127)
        var t160 int32 = current__127 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t160)
        var t161 Option__int32 = Some{
            _0: value__128,
        }
        jp158 = t161
    } else {
        jp158 = None{}
    }
    retv156 = jp158
    return retv156
}

func main() {
    main0()
}
