package fp.ch2_GettingStarted

import annotation._

object Chapter2 {
	// Excercise_1
	def fib(n: Int) = {
		@tailrec
		def inner(prev: Int, cur: Int, cnt: Int): Int = {
			if (cnt == n) cur
			else inner(cur, cur + prev, cnt + 1)
		}
		inner(0, 1, 0)
	}

	// Excercise_2
	def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
		@tailrec
		def inner(prev: Int, cur: Int, acc: Boolean): Boolean = {
			if (cur == as.length) acc
			else inner(prev + 1, cur + 1, acc && gt(as(prev), as(cur)))
		}
		if (as.length == 0) true
		else inner(0, 1, true) 
	}

	// Excercise_3(hard)
	def partial[A,B,C](a: A, f: (A, B) => C): B => C = y => f(a, y)

	// Excercise_4(hard)
	def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)

	// Excercise_5(optional)
	def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b) 

	// Excercise_6	
	def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}