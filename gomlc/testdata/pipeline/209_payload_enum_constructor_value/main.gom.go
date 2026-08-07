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
    var t177 List__int32 = make__0(value__1, values__2)
    return t177
}

func sum(values__3 List__int32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x172 int32 = values__3.(Cons)._0
        var x173 List__int32 = values__3.(Cons)._1
        var t182 int32 = sum(x173)
        var t183 int32 = x172 + t182
        return t183
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var cons__6 closure_env_cons_0 = closure_env_cons_0{}
    var t185 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 2, Nil{})
    var values__7 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 1, t185)
    var t186 int32 = sum(values__7)
    var t187 string
    var inline204 string = _goml_runtime_core_int32_to_string(t186)
    t187 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env174 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var t199 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t199
}

func main() {
    main0()
}
