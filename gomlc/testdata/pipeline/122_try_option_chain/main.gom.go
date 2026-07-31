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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func maybe_seed(flag__0 bool) Option__int32 {
    var retv160 Option__int32
    var jp162 Option__int32
    if flag__0 {
        var t163 Option__int32 = Some{
            _0: 3,
        }
        jp162 = t163
    } else {
        jp162 = None{}
    }
    retv160 = jp162
    return retv160
}

func maybe_double(value__1 int32) Option__int32 {
    var retv165 Option__int32
    var t168 bool = value__1 > 0
    var jp167 Option__int32
    if t168 {
        var t169 int32 = value__1 * 2
        var t170 Option__int32 = Some{
            _0: t169,
        }
        jp167 = t170
    } else {
        jp167 = None{}
    }
    retv165 = jp167
    return retv165
}

func maybe_total(flag__2 bool) Option__int32 {
    var retv172 Option__int32
    var mtmp152 Option__int32 = maybe_seed(flag__2)
    var jp174 int32
    switch mtmp152.(type) {
    case None:
        retv172 = None{}
        return retv172
    case Some:
        var x153 int32 = mtmp152.(Some)._0
        var try_value__22 int32 = x153
        jp174 = try_value__22
        var a__3 int32 = jp174
        var mtmp154 Option__int32 = maybe_double(a__3)
        var jp176 int32
        switch mtmp154.(type) {
        case None:
            retv172 = None{}
            return retv172
        case Some:
            var x155 int32 = mtmp154.(Some)._0
            var try_value__26 int32 = x155
            jp176 = try_value__26
            var b__4 int32 = jp176
            var t177 int32 = a__3 + b__4
            var t178 Option__int32 = Some{
                _0: t177,
            }
            retv172 = t178
            return retv172
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__5 Option__int32) string {
    var retv180 string
    var jp182 string
    switch opt__5.(type) {
    case None:
        jp182 = "none"
    case Some:
        var x156 int32 = opt__5.(Some)._0
        var value__6 int32 = x156
        var t183 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t184 string = "some=" + t183
        jp182 = t184
    default:
        panic("non-exhaustive match")
    }
    retv180 = jp182
    return retv180
}

func main0() struct{} {
    var t186 Option__int32 = maybe_total(true)
    var t187 string = show(t186)
    println__T_string(t187)
    var t188 Option__int32 = maybe_total(false)
    var t189 string = show(t188)
    println__T_string(t189)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv191 string
    var t192 string = _goml_runtime_core_int32_to_string(self__6)
    retv191 = t192
    return retv191
}

func println__T_string(value__1 string) struct{} {
    var t194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv197 string
    retv197 = self__38
    return retv197
}

func main() {
    main0()
}
