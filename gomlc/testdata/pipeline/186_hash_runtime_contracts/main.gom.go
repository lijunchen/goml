package main

import (
    _goml_os "os"
    _goml_reflect "reflect"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_CollisionKey_x struct {
    value CollisionKey
}

func ref__Ref_12CollisionKey(value CollisionKey) *ref_CollisionKey_x {
    return &ref_CollisionKey_x{
        value: value,
    }
}

func ref_set__Ref_12CollisionKey(reference *ref_CollisionKey_x, value CollisionKey) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_12CollisionKey(a *ref_CollisionKey_x, b *ref_CollisionKey_x) bool {
    return a == b
}

func ptr_hash__Ref_12CollisionKey(reference *ref_CollisionKey_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type hashmap_CollisionKey_int32_x_entry struct {
    active bool
    key CollisionKey
    value int32
}

type hashmap_CollisionKey_int32_x struct {
    buckets map[uint64][]hashmap_CollisionKey_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_12CollisionKey_5int32() *hashmap_CollisionKey_int32_x {
    return &hashmap_CollisionKey_int32_x{
        buckets: make(map[uint64][]hashmap_CollisionKey_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_12CollisionKey_5int32(m, key)
    if ok {
        return Option__i32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__i32{
        _tag: 0,
    }
}

func hashmap_set__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_CollisionKey_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_CollisionKey_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(entry.key, key) {
            var zero hashmap_CollisionKey_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

type hashmap_Ref_12CollisionKey_string_x_entry struct {
    active bool
    key *ref_CollisionKey_x
    value string
}

type hashmap_Ref_12CollisionKey_string_x struct {
    buckets map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_18Ref_12CollisionKey_6string() *hashmap_Ref_12CollisionKey_string_x {
    return &hashmap_Ref_12CollisionKey_string_x{
        buckets: make(map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_12CollisionKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_18Ref_12CollisionKey_6string(m, key)
    if ok {
        return Option__string{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__string{
        _tag: 0,
    }
}

func hashmap_set__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_12CollisionKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_12CollisionKey_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_12CollisionKey_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type FloatKey struct {
    value float64
}

type CollisionKey struct {
    value int32
}

type Ordering int32

const (
    Less Ordering = 0
    Equal Ordering = 1
    Greater Ordering = 2
)

type Option__Ordering struct {
    _tag int32
    _v1_0 Ordering
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t1345 int32 = self__5.value
    var t1346 int32 = other__6.value
    var t1347 bool = t1345 == t1346
    return t1347
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_int(value__10 Option__i32) struct{} {
    switch value__10._tag {
    case 0:
        var inline2367 string = "none"
        var inline2368 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2367)
        _goml_runtime_core_string_println(inline2368)
        return struct{}{}
    case 1:
        var x799 int32 = value__10._v1_0
        var inline2371 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x799)
        _goml_runtime_core_string_println(inline2371)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_comparison_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t1359 bool = zero32__12 == negative_zero32__13
    var t1360 string
    var inline2411 string = _goml_runtime_core_bool_to_string(t1359)
    t1360 = inline2411
    var inline2408 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1360)
    _goml_runtime_core_string_println(inline2408)
    var zero64__14 float64 = 0
    var negative_zero64__15 float64 = -zero64__14
    var t1361 bool = zero64__14 == negative_zero64__15
    var t1362 string
    var inline2406 string = _goml_runtime_core_bool_to_string(t1361)
    t1362 = inline2406
    var inline2403 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1362)
    _goml_runtime_core_string_println(inline2403)
    var t1365 bool
    var inline2401 bool = _goml_m_trait__impl_i_PartialEq_i_f64_i_eq(zero64__14, negative_zero64__15)
    t1365 = inline2401
    var t1366 string
    var inline2397 string = _goml_runtime_core_bool_to_string(t1365)
    t1366 = inline2397
    var inline2394 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1366)
    _goml_runtime_core_string_println(inline2394)
    var nan__16 float64 = zero64__14 / zero64__14
    var t1367 bool = nan__16 == nan__16
    var t1368 string
    var inline2392 string = _goml_runtime_core_bool_to_string(t1367)
    t1368 = inline2392
    var inline2389 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1368)
    _goml_runtime_core_string_println(inline2389)
    var t1369 Option__Ordering
    var inline2382 bool = nan__16 < nan__16
    if inline2382 {
        var inline2383 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        t1369 = inline2383
    } else {
        var inline2384 bool = nan__16 > nan__16
        if inline2384 {
            var inline2385 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            t1369 = inline2385
        } else {
            var inline2386 bool = nan__16 == nan__16
            if inline2386 {
                var inline2387 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Equal,
                }
                t1369 = inline2387
            } else {
                t1369 = Option__Ordering{
                    _tag: 0,
                }
            }
        }
    }
    var t1370 bool
    var inline2379 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t1369)
    var inline2380 bool = !inline2379
    t1370 = inline2380
    var t1371 string
    var inline2377 string = _goml_runtime_core_bool_to_string(t1370)
    t1371 = inline2377
    var inline2374 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1371)
    _goml_runtime_core_string_println(inline2374)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__17 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hc55bb71e9219d0c59c91622ae099ea85_onKey____V__i32()
    var t1373 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t1373, 10)
    var t1374 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t1374, 20)
    var t1375 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t1375, 30)
    var t1376 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h1189b7c51290244a02a1a6d496e4da69_onKey____V__i32(values__17, t1376)
    var t1377 CollisionKey = CollisionKey{
        value: 1,
    }
    var t1378 Option__i32 = _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(values__17, t1377)
    print_opt_int(t1378)
    var t1379 CollisionKey = CollisionKey{
        value: 2,
    }
    var t1380 Option__i32 = _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(values__17, t1379)
    print_opt_int(t1380)
    var t1381 CollisionKey = CollisionKey{
        value: 3,
    }
    var t1382 Option__i32 = _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(values__17, t1381)
    print_opt_int(t1382)
    var t1383 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t1383, 40)
    var t1384 int = _goml_m_inherent_i_HashMap_i_H_h282dac09c2296c58cbcd9cfca496474b_onKey____V__i32(values__17)
    println__T_isize(t1384)
    var t1385 CollisionKey = CollisionKey{
        value: 4,
    }
    var t1386 Option__i32
    var inline2459 Option__i32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t1385)
    t1386 = inline2459
    print_opt_int(t1386)
    var t1387 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline2456 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__17, t1387, inline2456)
    var t1388 int
    var inline2454 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1388 = inline2454
    var inline2451 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t1388)
    _goml_runtime_core_string_println(inline2451)
    var t1389 CollisionKey = CollisionKey{
        value: 4,
    }
    var t1390 Option__i32
    var inline2449 Option__i32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t1389)
    t1390 = inline2449
    switch t1390._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2445 int32 = t1390._v1_0
        println__T_i32(inline2445)
    default:
        panic("non-exhaustive match")
    }
    var t1391 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t1391)
    var t1392 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t1392)
    var t1393 int
    var inline2438 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1393 = inline2438
    var inline2435 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t1393)
    _goml_runtime_core_string_println(inline2435)
    var index__18 *ref_int32_x
    var inline2432 int32 = 0
    var inline2433 *ref_int32_x = ref__Ref_5int32(inline2432)
    index__18 = inline2433
    Loop_loop1396:
    for {
        var t1397 int32
        var inline2425 int32 = ref_get__Ref_5int32(index__18)
        t1397 = inline2425
        var t1398 bool = t1397 < 2000
        if t1398 {
            var t1399 int32
            var inline2423 int32 = ref_get__Ref_5int32(index__18)
            t1399 = inline2423
            var t1400 int32 = 1000 + t1399
            var key__19 CollisionKey = CollisionKey{
                value: t1400,
            }
            var t1401 int32
            var inline2421 int32 = ref_get__Ref_5int32(index__18)
            t1401 = inline2421
            hashmap_set__HashMap_12CollisionKey_5int32(values__17, key__19, t1401)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__17, key__19)
            var t1402 int32
            var inline2415 int32 = ref_get__Ref_5int32(index__18)
            t1402 = inline2415
            var t1403 int32 = t1402 + 1
            ref_set__Ref_5int32(index__18, t1403)
            continue
        } else {
            break Loop_loop1396
        }
    }
    var t1395 int
    var inline2430 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1395 = inline2430
    var inline2427 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t1395)
    _goml_runtime_core_string_println(inline2427)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__20 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t1405 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__21 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t1405)
    var t1406 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t1406)
    var inline2501 string = "identity"
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(values__20, key__21, inline2501)
    var t1407 bool
    var inline2499 bool = ptr_eq__Ref_12CollisionKey(key__21, key__21)
    t1407 = inline2499
    var inline2496 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1407)
    _goml_runtime_core_string_println(inline2496)
    var t1408 bool
    var inline2494 bool = ptr_eq__Ref_12CollisionKey(key__21, equal_value__23)
    t1408 = inline2494
    var inline2491 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1408)
    _goml_runtime_core_string_println(inline2491)
    var t1409 uint64
    var inline2489 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t1409 = inline2489
    var t1410 uint64
    var inline2487 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t1410 = inline2487
    var t1411 bool = t1409 == t1410
    var inline2484 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1411)
    _goml_runtime_core_string_println(inline2484)
    var t1412 Option__string
    var inline2482 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t1412 = inline2482
    switch t1412._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2478 string = t1412._v1_0
        println__T_string(inline2478)
    default:
        panic("non-exhaustive match")
    }
    var t1413 Option__string
    var inline2475 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, equal_value__23)
    t1413 = inline2475
    switch t1413._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2471 string = t1413._v1_0
        println__T_string(inline2471)
    default:
        panic("non-exhaustive match")
    }
    var t1414 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__21, t1414)
    var t1415 Option__string
    var inline2466 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t1415 = inline2466
    switch t1415._tag {
    case 0:
        println__T_string("none")
        return struct{}{}
    case 1:
        var inline2462 string = t1415._v1_0
        println__T_string(inline2462)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    float_comparison_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_f64_i_eq(self__458 float64, other__459 float64) bool {
    var t1803 bool = self__458 == other__459
    return t1803
}

func println__T_string(value__1 string) struct{} {
    var t1827 string
    t1827 = value__1
    _goml_runtime_core_string_println(t1827)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t1830 string
    var inline3005 string = __goml_builtin_int32_to_string(value__1)
    t1830 = inline3005
    _goml_runtime_core_string_println(t1830)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1834 string = _goml_runtime_core_bool_to_string(self__401)
    return t1834
}

func _goml_m_inherent_i_HashMap_i_H_hc55bb71e9219d0c59c91622ae099ea85_onKey____V__i32() *hashmap_CollisionKey_int32_x {
    var t1841 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t1841
}

func _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(self__675 *hashmap_CollisionKey_int32_x, key__676 CollisionKey, value__677 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__675, key__676, value__677)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h1189b7c51290244a02a1a6d496e4da69_onKey____V__i32(self__678 *hashmap_CollisionKey_int32_x, key__679 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__678, key__679)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(self__673 *hashmap_CollisionKey_int32_x, key__674 CollisionKey) Option__i32 {
    var t1848 Option__i32 = hashmap_get__HashMap_12CollisionKey_5int32(self__673, key__674)
    return t1848
}

func println__T_isize(value__1 int) struct{} {
    var t1850 string
    var inline3008 string = __goml_builtin_int_to_string(value__1)
    t1850 = inline3008
    _goml_runtime_core_string_println(t1850)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h282dac09c2296c58cbcd9cfca496474b_onKey____V__i32(self__680 *hashmap_CollisionKey_int32_x) int {
    var t1854 int = hashmap_len__HashMap_12CollisionKey_5int32(self__680)
    return t1854
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t1865 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t1865
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__684 CollisionKey) *ref_CollisionKey_x {
    var t1868 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__684)
    return t1868
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(self__473 *ref_CollisionKey_x, other__474 *ref_CollisionKey_x) bool {
    var t1876 bool = ptr_eq__Ref_12CollisionKey(self__473, other__474)
    return t1876
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__475 *ref_CollisionKey_x) uint64 {
    var t1879 uint64 = ptr_hash__Ref_12CollisionKey(self__475)
    return t1879
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline3012 int64 = int64(int32(self__407))
    var inline3013 string = signed_decimal_string(inline3012)
    return inline3013
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(self__718 Option__Ordering) bool {
    switch self__718._tag {
    case 0:
        return false
    case 1:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline3015 int64 = int64(int(self__404))
    var inline3016 string = signed_decimal_string(inline3015)
    return inline3016
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t1899 int64 = int64(int32(value__225))
    var inline3018 bool = t1899 < 0
    if inline3018 {
        var inline3019 uint64 = uint64(int64(t1899))
        var inline3020 uint64 = 0 - inline3019
        var inline3021 string = decimal_string(inline3020)
        var inline3022 string = "-" + inline3021
        return inline3022
    } else {
        var inline3023 uint64 = uint64(int64(t1899))
        var inline3024 string = decimal_string(inline3023)
        return inline3024
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1903 int64 = int64(int(value__222))
    var inline3026 bool = t1903 < 0
    if inline3026 {
        var inline3027 uint64 = uint64(int64(t1903))
        var inline3028 uint64 = 0 - inline3027
        var inline3029 string = decimal_string(inline3028)
        var inline3030 string = "-" + inline3029
        return inline3030
    } else {
        var inline3031 uint64 = uint64(int64(t1903))
        var inline3032 string = decimal_string(inline3031)
        return inline3032
    }
}

func signed_decimal_string(value__214 int64) string {
    var t1909 bool = value__214 < 0
    if t1909 {
        var t1910 uint64 = uint64(int64(value__214))
        var t1911 uint64 = 0 - t1910
        var t1912 string = decimal_string(t1911)
        var t1913 string = "-" + t1912
        return t1913
    } else {
        var t1914 uint64 = uint64(int64(value__214))
        var t1915 string = decimal_string(t1914)
        return t1915
    }
}

func decimal_string(value__208 uint64) string {
    var t1938 bool = value__208 == 0
    if t1938 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1931:
        for {
            var t1932 bool = remaining__210 > 0
            if t1932 {
                var t1933_rhs uint64 = 10
                var t1933 uint64 = remaining__210 % t1933_rhs
                var t1934 uint8 = uint8(uint64(t1933))
                var t1935 uint8 = t1934 + 48
                vec_push__Vec_5uint8(reversed__209, t1935)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1936 uint64 = compound_old353 / compound_value354
                remaining__210 = t1936
                continue
            } else {
                break Loop_loop1931
            }
        }
        var t1920 int
        var inline3042 int = vec_len__Vec_5uint8(reversed__209)
        t1920 = inline3042
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1920)
        var offset__212 int = 0
        Loop_loop1922:
        for {
            var t1923 int
            var inline3040 int = vec_len__Vec_5uint8(reversed__209)
            t1923 = inline3040
            var t1924 bool = offset__212 < t1923
            if t1924 {
                var t1925 int
                var inline3038 int = vec_len__Vec_5uint8(reversed__209)
                t1925 = inline3038
                var t1926 int = t1925 - offset__212
                var t1927 int = t1926 - 1
                var t1928 uint8 = vec_get__Vec_5uint8(reversed__209, t1927)
                vec_push__Vec_5uint8(bytes__211, t1928)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1929 int = compound_old358 + compound_value359
                offset__212 = t1929
                continue
            } else {
                break Loop_loop1922
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
