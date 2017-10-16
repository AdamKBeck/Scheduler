package scheduler

import org.scalatest.FlatSpec

class DependencyTest extends FlatSpec {

	behavior of "dependencyType"
	it should "return a correct Dependency.Type case object" in {
		val sampleDependency = Dependency(Dependency.BEGIN_BEGIN, 1, 3)
		assert(sampleDependency.dependencyType == Dependency.BEGIN_BEGIN)
	}
}
