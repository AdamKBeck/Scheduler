package scheduler

import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer


class DependencyExceptionTest extends FlatSpec {

	// Bad data: Job does not have dependencies
	behavior of "verify"
	it should "Test with a job with no dependencies" in {
		val job = Job(Set(), 3, 1)
		assert(!DependencyException.verify(job, Dependency.END_BEGIN, 2))
	}

	// Good data: min normal configuration,
	it should "Test with one dependency in the job" in {
		val dependency = Dependency(Dependency.END_BEGIN, 1, 2)

		val job = Job(Set(dependency), 4, 1)
		assert(DependencyException.verify(job, Dependency.END_BEGIN, 2))
	}

	// Good data: average normal config
	it should "Test with 3 dependencies in the job" in {
		val dependencyA = Dependency(Dependency.END_BEGIN, 1, 2)
		val dependencyB = Dependency(Dependency.END_BEGIN, 1, 45)
		val dependencyC = Dependency(Dependency.END_BEGIN, 1, 5)

		val job = Job(Set(dependencyA, dependencyB, dependencyC), 4, 1)
		assert(DependencyException.verify(job, Dependency.END_BEGIN, 5))
	}

	// Good data: max normal config
	it should "Test with 100 dependencies in the job" in {
		val dependencyList = ListBuffer[Dependency]()

		for (i <- 1 to 100) {
			dependencyList += Dependency(Dependency.END_BEGIN, i, i)
		}

		val job = Job(dependencyList.toSet, 4, 1)
		assert(DependencyException.verify(job, Dependency.END_BEGIN, 34))
	}

	// Bad Data: too many dependencies
	it should "Test will all 4 types of dependencies on the job" in {
		val dependencyA = Dependency(Dependency.END_BEGIN, 1, 2)
		val dependencyB = Dependency(Dependency.END_END, 1, 2)
		val dependencyC = Dependency(Dependency.BEGIN_BEGIN, 1, 2)
		val dependencyD = Dependency(Dependency.BEGIN_END, 1, 2)

		val job = Job(Set(dependencyA, dependencyB, dependencyC, dependencyD), 4, 1)
		assert(DependencyException.verify(job, Dependency.BEGIN_END, 2))
	}
}
