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
    var t141 List__int32 = make__0(value__1, values__2)
    return t141
}

func sum(values__3 List__int32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x136 int32 = values__3.(Cons)._0
        var x137 List__int32 = values__3.(Cons)._1
        var t146 int32 = sum(x137)
        var t147 int32 = x136 + t146
        return t147
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var cons__6 closure_env_cons_0 = closure_env_cons_0{}
    var t149 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 2, Nil{})
    var values__7 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 1, t149)
    var t150 int32 = sum(values__7)
    var t151 string
    var inline168 string = _goml_runtime_core_int32_to_string(t150)
    t151 = inline168
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t151)
    _goml_runtime_core_string_println(inline165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env138 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var t163 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t163
}

func main() {
    main0()
}
