package main

func caseValue(counter *int) int {
	(*counter)++
	return *counter
}

func main() {
	counter := 0
	switch 2 {
	default:
		println(0)
	case caseValue(&counter), caseValue(&counter):
		println(counter)
	case caseValue(&counter):
		println(99)
	}
	total := 0
	index := 0
loop:
	for index < 6 {
		index++
		if index == 2 {
			continue
		}
		if index == 5 {
			break
		}
		total += index
	}
	if total == 8 {
		goto done
	}
	goto loop
done:
	labelTotal := 0
outer:
	for row := 0; row < 3; row++ {
		for column := 0; column < 3; column++ {
			if column == 1 {
				continue outer
			}
			labelTotal++
		}
	}
	println(counter, total, labelTotal)
	switch 1 {
	case 1:
		println(3)
		fallthrough
	case 2:
		println(4)
	}
}
