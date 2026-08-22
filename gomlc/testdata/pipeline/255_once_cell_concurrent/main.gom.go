package main

import (
    _goml_os "os"
    _goml_sync "sync"
    _goml_runtime_pkg "runtime"
)

func _goml_once_cell_goroutine_id() uint64 {
    var buffer []uint8 = make([]uint8, 64)
    var length int = _goml_runtime_pkg.Stack(buffer, false)
    var index int = 10
    var result uint64 = 0
    for {
        if index >= length {
            break
        }
        if buffer[index] < 48 || buffer[index] > 57 {
            break
        }
        result = result * 10 + uint64(buffer[index] - 48)
        index = index + 1
    }
    return result
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

func once_cell_new__OnceCell__isize() *OnceCell__isize {
    var cell *OnceCell__isize = &OnceCell__isize{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__isize(cell *OnceCell__isize, init func() int) int {
    cell.mutex.Lock()
    for {
        if cell.state == 2 {
            cell.mutex.Unlock()
            return cell.value
        }
        var goroutine uint64 = _goml_once_cell_goroutine_id()
        if cell.state == 1 {
            if cell.owner == goroutine {
                cell.mutex.Unlock()
                panic("recursive OnceCell initialization: " + cell.name)
            }
            cell.cond.Wait()
            continue
        }
        cell.state = 1
        cell.owner = goroutine
        cell.mutex.Unlock()
        var initialized int = init()
        cell.mutex.Lock()
        cell.value = initialized
        cell.state = 2
        cell.owner = 0
        cell.cond.Broadcast()
        cell.mutex.Unlock()
        return initialized
    }
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
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

type closure_env_main_0 struct {}

type closure_env_main_1 struct {
    results_0 chan int
}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {
    results_0 chan int
}

type OnceCell__isize struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value int
    name string
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

var VALUE *OnceCell__isize = func() *OnceCell__isize {
    var cell *OnceCell__isize = once_cell_new__OnceCell__isize()
    cell.name = "VALUE"
    return cell
}()

func main0() struct{} {
    var results__0 chan int
    var inline879 int = 2
    var inline880 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline879)
    results__0 = inline880
    var t803 closure_env_main_1 = closure_env_main_1{
        results_0: results__0,
    }
    var t804 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t803)
    }
    go t804()
    var t805 closure_env_main_3 = closure_env_main_3{
        results_0: results__0,
    }
    var t806 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t805)
    }
    go t806()
    var t807 Option__isize
    var inline872 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline873 int = inline872._0
    var inline874 bool = inline872._1
    if inline874 {
        var inline877 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline873,
        }
        t807 = inline877
    } else {
        t807 = Option__isize{
            _tag: 0,
        }
    }
    var first__1 int
    var inline868 int = 0
    switch t807._tag {
    case 0:
        first__1 = inline868
    case 1:
        var inline869 int = t807._v1_0
        first__1 = inline869
    default:
        panic("non-exhaustive match")
    }
    var t808 Option__isize
    var inline861 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline862 int = inline861._0
    var inline863 bool = inline861._1
    if inline863 {
        var inline866 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline862,
        }
        t808 = inline866
    } else {
        t808 = Option__isize{
            _tag: 0,
        }
    }
    var second__2 int
    var inline857 int = 0
    switch t808._tag {
    case 0:
        second__2 = inline857
    case 1:
        var inline858 int = t808._v1_0
        second__2 = inline858
    default:
        panic("non-exhaustive match")
    }
    var t809 bool = first__1 == second__2
    var t810 string
    var inline855 string = _goml_runtime_core_bool_to_string(t809)
    t810 = inline855
    var inline852 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline852)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env798 closure_env_main_0) int {
    return 41
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env799 closure_env_main_1) struct{} {
    var results__0 chan int = env799.results_0
    var t840 closure_env_main_0 = closure_env_main_0{}
    var t841 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t840)
    }
    var t842 int
    var inline885 int = once_cell_get_or_init__OnceCell__isize(VALUE, t841)
    t842 = inline885
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t842)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env800 closure_env_main_2) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env801 closure_env_main_3) struct{} {
    var results__0 chan int = env801.results_0
    var t847 closure_env_main_2 = closure_env_main_2{}
    var t848 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t847)
    }
    var t849 int
    var inline889 int = once_cell_get_or_init__OnceCell__isize(VALUE, t848)
    t849 = inline889
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t849)
    return struct{}{}
}

func main() {
    main0()
}
