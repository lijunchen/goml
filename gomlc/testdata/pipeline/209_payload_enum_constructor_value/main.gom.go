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
    var t160 List__int32 = make__0(value__1, values__2)
    return t160
}

func sum(values__3 List__int32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x155 int32 = values__3.(Cons)._0
        var x156 List__int32 = values__3.(Cons)._1
        var t165 int32 = sum(x156)
        var t166 int32 = x155 + t165
        return t166
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var cons__6 closure_env_cons_0 = closure_env_cons_0{}
    var t168 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 2, Nil{})
    var values__7 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 1, t168)
    var t169 int32 = sum(values__7)
    var t170 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t169)
    println__T_string(t170)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t177 string = _goml_runtime_core_int32_to_string(self__6)
    return t177
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env157 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var t182 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t182
}

func main() {
    main0()
}
