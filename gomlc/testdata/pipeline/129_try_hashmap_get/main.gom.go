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

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    indices map[string]int
    entries []hashmap_string_int32_x_entry
    len int
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_lookup__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero int32
        return zero, false
    }
    var entry hashmap_string_int32_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Option__int32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int32{
        _tag: 0,
    }
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int32_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type Ordering int32

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func fetch(flag__0 bool) Option__int32 {
    var m__1 *hashmap_string_int32_x
    var inline463 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline463
    if flag__0 {
        var inline456 string = "a"
        var inline457 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline456, inline457)
    } else {}
    var mtmp410 Option__int32
    var inline460 string = "a"
    var inline461 Option__int32 = hashmap_get__HashMap_6string_5int32(m__1, inline460)
    mtmp410 = inline461
    var jp419 int32
    switch mtmp410._tag {
    case 0:
        return Option__int32{
            _tag: 0,
        }
    case 1:
        var x411 int32 = mtmp410._v1_0
        jp419 = x411
        var t420 int32 = jp419 + 1
        var t421 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: t420,
        }
        return t421
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t429 Option__int32 = fetch(true)
    var t430 string
    switch t429._tag {
    case 0:
        t430 = "none"
    case 1:
        var inline478 int32 = t429._v1_0
        var inline480 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline478)
        var inline481 string = "some=" + inline480
        t430 = inline481
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline475)
    var t431 Option__int32 = fetch(false)
    var t432 string
    switch t431._tag {
    case 0:
        t432 = "none"
    case 1:
        var inline470 int32 = t431._v1_0
        var inline472 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline470)
        var inline473 string = "some=" + inline472
        t432 = inline473
    default:
        panic("non-exhaustive match")
    }
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline467)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t443 string = _goml_runtime_core_int32_to_string(self__33)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
