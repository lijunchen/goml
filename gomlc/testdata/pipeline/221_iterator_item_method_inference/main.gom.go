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

type Values struct {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_Iterator_i_Values_i_next(self__0 Values) Option__int32 {
    return None{}
}

func main0() struct{} {
    var t163 Values = Values{}
    var for_iter155 Values = _goml_m_trait__impl_i_IntoIterator_i_Values_i_into__iter(t163)
    Loop_loop165:
    for {
        var for_next156 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Values_i_next(for_iter155)
        switch for_next156.(type) {
        case None:
            break Loop_loop165
        case Some:
            var x157 int32 = for_next156.(Some)._0
            var t167 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
            _goml_runtime_core_string_println(t167)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Values_i_into__iter(self__109 Values) Values {
    return self__109
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t172 string = _goml_runtime_core_int32_to_string(self__6)
    return t172
}

func main() {
    main0()
}
