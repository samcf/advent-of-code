package main

import (
	"bufio"
	"fmt"
	"os"
)

func a() int {
	sigfi := 12
	masks := 1
	lines := bufio.NewScanner(os.Stdin)
	count := make([]int, sigfi)
	for lines.Scan() {
		masks += 1
		value := lines.Bytes()
		for i := 0; i < len(value); i += 1 {
			if value[i] == 49 {
				count[i] += 1
			}
		}
	}

	r := 0
	for i := 0; i < len(count); i += 1 {
		if count[i] > (masks / 2) {
			r = r | (1 << ((sigfi - 1) - i))
		}
	}

	return r * (^r & ((1 << sigfi) - 1))
}

func main() {
	fmt.Printf("Part A: %d\n", a())
}
