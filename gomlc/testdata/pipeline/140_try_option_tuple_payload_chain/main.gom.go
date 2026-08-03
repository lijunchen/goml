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
            _0: "left",
            _1: "right",
        }
        var t149 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: t148,
        }
        return t149
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline176 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline177 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q__Some{
            _0: inline176,
        }
        return inline177
    } else {
        return _goml_m_Option_____o_string_c_string_q__None{}
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp136 _goml_m_Option_____o_string_c_string_q_
    var inline179 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp136 = inline179
    var jp156 Tuple2_6string_6string
    switch mtmp136.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        return Option__string_None{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var x137 Tuple2_6string_6string = mtmp136.(_goml_m_Option_____o_string_c_string_q__Some)._0
        jp156 = x137
        var x139 string = jp156._0
        var x140 string = jp156._1
        var t157 string = x139 + ":"
        var t158 string = t157 + x140
        var t159 Option__string = Option__string_Some{
            _0: t158,
        }
        return t159
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t166 Option__string = describe(true)
    var t167 string
    switch t166.(type) {
    case Option__string_None:
        t167 = "none"
    case Option__string_Some:
        var inline206 string = t166.(Option__string_Some)._0
        var inline208 string = "some " + inline206
        t167 = inline208
    default:
        panic("non-exhaustive match")
    }
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline203)
    var t168 Option__string
    var inline188 bool = false
    var inline189 _goml_m_Option_____o_string_c_string_q_ = pair(inline188)
    var inline191 Tuple2_6string_6string
    switch inline189.(type) {
    case _goml_m_Option_____o_string_c_string_q__None:
        t168 = Option__string_None{}
        var t169 string
        switch t168.(type) {
        case Option__string_None:
            t169 = "none"
        case Option__string_Some:
            var inline184 string = t168.(Option__string_Some)._0
            var inline186 string = "some " + inline184
            t169 = inline186
        default:
            panic("non-exhaustive match")
        }
        var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
        _goml_runtime_core_string_println(inline181)
        return struct{}{}
    case _goml_m_Option_____o_string_c_string_q__Some:
        var inline200 Tuple2_6string_6string = inline189.(_goml_m_Option_____o_string_c_string_q__Some)._0
        inline191 = inline200
        var inline193 string = inline191._0
        var inline194 string = inline191._1
        var inline197 string = inline193 + ":"
        var inline198 string = inline197 + inline194
        var inline199 Option__string = Option__string_Some{
            _0: inline198,
        }
        t168 = inline199
        var t169 string
        switch t168.(type) {
        case Option__string_None:
            t169 = "none"
        case Option__string_Some:
            var inline184 string = t168.(Option__string_Some)._0
            var inline186 string = "some " + inline184
            t169 = inline186
        default:
            panic("non-exhaustive match")
        }
        var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
        _goml_runtime_core_string_println(inline181)
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
