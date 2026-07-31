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

func maybe_value(flag__0 bool) Option__int32 {
    var retv158 Option__int32
    var jp160 Option__int32
    if flag__0 {
        var t161 Option__int32 = Some{
            _0: 4,
        }
        jp160 = t161
    } else {
        jp160 = None{}
    }
    retv158 = jp160
    return retv158
}

func add(a__1 int32, b__2 int32) int32 {
    var retv163 int32
    var t164 int32 = a__1 + b__2
    retv163 = t164
    return retv163
}

func plus_two(flag__3 bool) Option__int32 {
    var retv166 Option__int32
    var mtmp152 Option__int32 = maybe_value(flag__3)
    var jp168 int32
    switch mtmp152.(type) {
    case None:
        retv166 = None{}
        return retv166
    case Some:
        var x153 int32 = mtmp152.(Some)._0
        var try_value__15 int32 = x153
        jp168 = try_value__15
        var t169 int32 = add(jp168, 2)
        var t170 Option__int32 = Some{
            _0: t169,
        }
        retv166 = t170
        return retv166
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__4 Option__int32) string {
    var retv172 string
    var jp174 string
    switch opt__4.(type) {
    case None:
        jp174 = "none"
    case Some:
        var x154 int32 = opt__4.(Some)._0
        var value__5 int32 = x154
        var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t176 string = "some=" + t175
        jp174 = t176
    default:
        panic("non-exhaustive match")
    }
    retv172 = jp174
    return retv172
}

func main0() struct{} {
    var t178 Option__int32 = plus_two(true)
    var t179 string = show(t178)
    println__T_string(t179)
    var t180 Option__int32 = plus_two(false)
    var t181 string = show(t180)
    println__T_string(t181)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv183 string
    var t184 string = _goml_runtime_core_int32_to_string(self__6)
    retv183 = t184
    return retv183
}

func println__T_string(value__1 string) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv189 string
    retv189 = self__38
    return retv189
}

func main() {
    main0()
}
