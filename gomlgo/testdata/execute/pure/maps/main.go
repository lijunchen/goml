package main

func main() {
	values := map[string]int{"one": 1, "two": 2}
	values["three"] = 3
	values["one"] += 4
	missing, ok := values["missing"]
	present, presentOK := values["two"]
	println(len(values), values["one"], missing, ok, present, presentOK)
	delete(values, "two")
	println(len(values), values["two"])
	clear(values)
	println(len(values))
	var nilMap map[string]int
	value, nilOK := nilMap["key"]
	println(len(nilMap), value, nilOK, nilMap == nil)
	bytes := []byte{1, 2, 3}
	clear(bytes)
	println(bytes[0], bytes[1], bytes[2])
	ranged := map[string]int{"a": 1, "b": 2, "c": 3}
	sum := 0
	for _, item := range ranged {
		sum += item
	}
	count := 0
	for range ranged {
		count++
	}
	println(sum, count)
	array := [1]int{7}
	slice := array[:]
	pointers := map[*int]int{&array[0]: 9}
	println(&array[0] == &slice[0], pointers[&slice[0]])
	nested := map[string][]int{"x": []int{1, 2}}
	nested["x"][1] = 8
	counts := map[string]int{}
	counts["x"]++
	println(nested["x"][1], counts["x"])
	interfaces := map[any]int{1: 2, "x": 3}
	println(interfaces[1], interfaces["x"])
	zero := 0.0
	nans := map[float64]int{zero / zero: 1, zero / zero: 2, zero / zero: 3}
	nanCount := 0
	for range nans {
		nanCount++
	}
	println(len(nans), nanCount)
}
