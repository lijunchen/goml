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

type closure_env_cons_0 struct {}

type List__int32 interface {
    isList__int32()
}

type Nil struct {}

func (_ Nil) isList__int32() {}

type Cons struct {
    _0 int32
    _1 List__int32
}

func (_ Cons) isList__int32() {}

func prepend_with(make__0 func(int32, List__int32) List__int32, value__1 int32, values__2 List__int32) List__int32 {
    var retv156 List__int32
    var t157 List__int32 = make__0(value__1, values__2)
    retv156 = t157
    return retv156
}

func sum(values__3 List__int32) int32 {
    var retv159 int32
    var jp161 int32
    switch values__3.(type) {
    case Nil:
        jp161 = 0
    case Cons:
        var x152 int32 = values__3.(Cons)._0
        var x153 List__int32 = values__3.(Cons)._1
        var rest__5 List__int32 = x153
        var value__4 int32 = x152
        var t162 int32 = sum(rest__5)
        var t163 int32 = value__4 + t162
        jp161 = t163
    default:
        panic("non-exhaustive match")
    }
    retv159 = jp161
    return retv159
}

func main0() struct{} {
    var cons__6 closure_env_cons_0 = closure_env_cons_0{}
    var t165 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 2, Nil{})
    var values__7 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 1, t165)
    var t166 int32 = sum(values__7)
    var t167 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t166)
    println__T_string(t167)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv173 string
    var t174 string = _goml_runtime_core_int32_to_string(self__6)
    retv173 = t174
    return retv173
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv176 string
    retv176 = self__38
    return retv176
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env154 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var retv178 List__int32
    var t179 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    retv178 = t179
    return retv178
}

func main() {
    main0()
}
