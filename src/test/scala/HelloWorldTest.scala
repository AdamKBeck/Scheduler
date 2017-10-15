package scheduler

import org.scalatest.FlatSpec

class HelloWorldTest extends FlatSpec {
	behavior of "Example test 1"
	it should "example statement" in {
		var h = HelloWorld(3)
		assert(h.getX == 3)
	}
}
