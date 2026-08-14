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

func sum(values__3 List__int32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x182 int32 = values__3.(Cons)._0
        var x183 List__int32 = values__3.(Cons)._1
        var t192 int32 = sum(x183)
        var t193 int32 = x182 + t192
        return t193
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t195 closure_env_cons_0 = closure_env_cons_0{}
    var cons__6 func(int32, List__int32) List__int32 = func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(t195, p0, p1)
    }
    var t196 List__int32
    var inline220 int32 = 2
    var inline221 List__int32 = cons__6(inline220, Nil{})
    t196 = inline221
    var values__7 List__int32
    var inline217 int32 = 1
    var inline218 List__int32 = cons__6(inline217, t196)
    values__7 = inline218
    var t197 int32 = sum(values__7)
    var t198 string
    var inline215 string = _goml_runtime_core_int32_to_string(t197)
    t198 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env184 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var t210 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t210
}

func main() {
    main0()
}
