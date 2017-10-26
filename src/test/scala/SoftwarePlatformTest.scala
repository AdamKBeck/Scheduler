package scheduler

import org.scalatest.{BeforeAndAfterEach, FlatSpec, PrivateMethodTester}

import scala.collection.mutable.ListBuffer

class SoftwarePlatformTest extends FlatSpec with BeforeAndAfterEach with PrivateMethodTester{

	private var schedule = clearSchedule
	private var job1 = Job(Set(), 4, 1) // Order matters as job1 comes first. Used for testing below
	private var job2 = Job(Set(), 5, 2)

	private def clearSchedule = ListBuffer[ListBuffer[Job]]()

	// Sets up a simple schedule with a 2 non-parallel jobs, no dependencies
	override def beforeEach(): Unit = {
		appendJobToSchedule(job1, schedule)
		appendJobToSchedule(job2, schedule)
	}

	override def afterEach(): Unit = {
		schedule = clearSchedule
	}

	// Appends a job to the end of a schedule in new separate list
	private def appendJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]]): Unit = {
		schedule += ListBuffer[Job](job)
	}

	// Appends a job to a list of given index in a schedule
	private def insertJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]], index: Int): Unit = {
		schedule(index) += job
	}

	// emptySchedule testing
	// Structured Basis: nominal case, nothing
	behavior of "emptySchedule"
	it should "test nominal" in {
		val emptySchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('emptySchedule)
		val schedule = SoftwarePlatform invokePrivate emptySchedule()
		assert(schedule.isEmpty)
	}

	// isPrecedingEndEndValid testing
	// Structured Basis: nominal case, all boolean conditions true
	// Good data: minimum normal configuration: 2 jobs
	behavior of "isPrecedingEndEndValid"
	it should "test nominal" in {
		val dependency = Dependency(Dependency.END_END, 1, 2)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)
		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingEndEndValid = PrivateMethod[Boolean]('isPrecedingEndEndValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingEndEndValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured Basis: the first if is false.
	it should "test with no End-End dependency" in {
		val isPrecedingEndEndValid = PrivateMethod[Boolean]('isPrecedingEndEndValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingEndEndValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Bad data: subschedule is Nil
	it should "test with bad data: Nil subschedule" in {
		val isPrecedingEndEndValid = PrivateMethod[Boolean]('isPrecedingEndEndValid)
		schedule = clearSchedule
		val validityResult = SoftwarePlatform invokePrivate isPrecedingEndEndValid(job2, job1, schedule, 0)
		assert(validityResult)
	}


	// Good data: max normal configuration, 100 jobs
	it should "test with max normal configuration: 100 jobs" in {
		for (i <- 0 to 98) {
			val job = Job(Set(), 4, i)
			appendJobToSchedule(job, schedule)
		}
		val isPrecedingEndEndValid = PrivateMethod[Boolean]('isPrecedingEndEndValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingEndEndValid(job2, job1, schedule, 0)
		assert(validityResult)
	}
}




