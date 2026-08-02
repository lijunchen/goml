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
        var t167 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t168 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t167,
        }
        return t168
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp155 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline192 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline193 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline192,
        }
        mtmp155 = inline193
    } else {
        mtmp155 = _goml_m_Option_____o_string_c_string_q__None{}
    }
    var jp172 Tuple2_6string_6string
    switch mtmp155.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x156 Tuple2_6string_6string = mtmp155.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp172 = x156
        var x158 string = jp172._0
        var x159 string = jp172._1
        var t173 string = x158 + "|"
        var t174 string = t173 + x159
        var t175 Option__string = Option__string_Some{
            _0: t174,
        }
        return t175
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t182 Option__string = describe(true)
    var t183 string
    switch t182.(type) {
    case Option__string_None:
        t183 = "none"
    case Option__string_Some:
        var inline220 string = t182.(Option__string_Some)._0
        var inline222 string = "some " + inline220
        t183 = inline222
    default:
        panic("non-exhaustive match")
    }
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline217)
    var t184 Option__string
    var inline202 bool = false
    var inline203 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline202)
    var inline205 Tuple2_6string_6string
    switch inline203.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t184 = Option__string_None{}
        var t185 string
        switch t184.(type) {
        case Option__string_None:
            t185 = "none"
        case Option__string_Some:
            var inline198 string = t184.(Option__string_Some)._0
            var inline200 string = "some " + inline198
            t185 = inline200
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
        _goml_runtime_core_string_println(inline195)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline214 Tuple2_6string_6string = inline203.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline205 = inline214
        var inline207 string = inline205._0
        var inline208 string = inline205._1
        var inline211 string = inline207 + "|"
        var inline212 string = inline211 + inline208
        var inline213 Option__string = Option__string_Some{
            _0: inline212,
        }
        t184 = inline213
        var t185 string
        switch t184.(type) {
        case Option__string_None:
            t185 = "none"
        case Option__string_Some:
            var inline198 string = t184.(Option__string_Some)._0
            var inline200 string = "some " + inline198
            t185 = inline200
        default:
            panic("non-exhaustive match")
        }
        var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
        _goml_runtime_core_string_println(inline195)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
