package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_main_0 struct {}

type Maybe__int interface {
    isMaybe__int()
}

type Maybe__int_None struct {}

func (_ Maybe__int_None) isMaybe__int() {}

type Maybe__int_Some struct {
    _0 int
}

func (_ Maybe__int_Some) isMaybe__int() {}

type Maybe__string interface {
    isMaybe__string()
}

type Maybe__string_None struct {}

func (_ Maybe__string_None) isMaybe__string() {}

type Maybe__string_Some struct {
    _0 string
}

func (_ Maybe__string_Some) isMaybe__string() {}

func main0() struct{} {
    var value__3 Maybe__int = Maybe__int_Some{
        _0: 3,
    }
    var t160 closure_env_main_0 = closure_env_main_0{}
    var mtmp156 Maybe__string = _goml_m_inherent_i_Maybe_i_Maybe_l_T_r__i_map____T__int____U__string(value__3, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t160, p0)
    })
    switch mtmp156.(type) {
    case Maybe__string_None:
        println__T_string("none")
        return struct{}{}
    case Maybe__string_Some:
        var x157 string = mtmp156.(Maybe__string_Some)._0
        println__T_string(x157)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t166 string = _goml_runtime_core_int_to_string(self__5)
    return t166
}

func _goml_m_inherent_i_Maybe_i_Maybe_l_T_r__i_map____T__int____U__string(self__0 Maybe__int, map_fn__1 func(int) string) Maybe__string {
    switch self__0.(type) {
    case Maybe__int_None:
        return Maybe__string_None{}
    case Maybe__int_Some:
        var x155 int = self__0.(Maybe__int_Some)._0
        var t171 string = map_fn__1(x155)
        var t172 Maybe__string = Maybe__string_Some{
            _0: t171,
        }
        return t172
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t174)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env158 closure_env_main_0, item__4 int) string {
    var t180 string = _goml_m_inherent_i_int_i_int_i_to__string(item__4)
    return t180
}

func main() {
    main0()
}
