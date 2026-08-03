package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__0 {
        var t148 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t149 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t148,
        }
        return t149
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp136 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline173 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline174 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline173,
        }
        mtmp136 = inline174
    } else {
        mtmp136 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp153 Tuple2_6string_6string
    switch mtmp136.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x137 Tuple2_6string_6string = mtmp136.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp153 = x137
        var x139 string = jp153._0
        var x140 string = jp153._1
        var t154 string = x139 + "|"
        var t155 string = t154 + x140
        var t156 Option__string = Option__string_Some{
            _0: t155,
        }
        return t156
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t163 Option__string = describe(true)
    var t164 string
    switch t163.(type) {
    case Option__string_None:
        t164 = "none"
    case Option__string_Some:
        var inline201 string = t163.(Option__string_Some)._0
        var inline203 string = "some " + inline201
        t164 = inline203
    default:
        panic("non-exhaustive match")
    }
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline198)
    var t165 Option__string
    var inline183 bool = false
    var inline184 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline183)
    var inline186 Tuple2_6string_6string
    switch inline184.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t165 = Option__string_None{}
        var t166 string
        switch t165.(type) {
        case Option__string_None:
            t166 = "none"
        case Option__string_Some:
            var inline179 string = t165.(Option__string_Some)._0
            var inline181 string = "some " + inline179
            t166 = inline181
        default:
            panic("non-exhaustive match")
        }
        var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
        _goml_runtime_core_string_println(inline176)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline195 Tuple2_6string_6string = inline184.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline186 = inline195
        var inline188 string = inline186._0
        var inline189 string = inline186._1
        var inline192 string = inline188 + "|"
        var inline193 string = inline192 + inline189
        var inline194 Option__string = Option__string_Some{
            _0: inline193,
        }
        t165 = inline194
        var t166 string
        switch t165.(type) {
        case Option__string_None:
            t166 = "none"
        case Option__string_Some:
            var inline179 string = t165.(Option__string_Some)._0
            var inline181 string = "some " + inline179
            t166 = inline181
        default:
            panic("non-exhaustive match")
        }
        var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
        _goml_runtime_core_string_println(inline176)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
