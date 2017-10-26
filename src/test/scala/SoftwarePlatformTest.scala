package scheduler

import org.scalatest.{BeforeAndAfterEach, FlatSpec, PrivateMethodTester}

import scala.collection.mutable.ListBuffer

class SoftwarePlatformTest extends FlatSpec with BeforeAndAfterEach with PrivateMethodTester{

	private var schedule = clearSchedule

	private var job1 = Job(Set(), 4, 1) // Order matters as job1 comes first. Used for testing below
	private var job2 = Job(Set(), 5, 2)
	private var job3 = Job(Set(), 6, 3)
	private var job4 = Job(Set(), 6, 4)

	/* I often need to create a cleared schedule for these testing classes. So why not use emptySchedule()?
	 * I believe it's semantic coupling to assume that emptySchedule works, as we are testing it. Therefore,
	 * I made the same method in here, but renamed to a similar but different name so I'm able to test emptySchedule */
	private def clearSchedule = ListBuffer[ListBuffer[Job]]()

	// Sets up a simple schedule with a 2 non-parallel jobs, no dependencies
	override def beforeEach(): Unit = {
		appendJobToSchedule(job1, schedule)
		appendJobToSchedule(job2, schedule)

		val dependencyA = Dependency(Dependency.END_END, 3, 4)
		val dependencyB = Dependency(Dependency.END_BEGIN, 3, 4)
		val dependencyC = Dependency(Dependency.BEGIN_END, 3, 4)
		val dependencyD = Dependency(Dependency.BEGIN_BEGIN, 3, 4)

		val dependencyE = Dependency(Dependency.END_END, 3, 4)
		val dependencyF = Dependency(Dependency.END_BEGIN, 3, 4)
		val dependencyG = Dependency(Dependency.BEGIN_END, 3, 4)
		val dependencyH = Dependency(Dependency.BEGIN_BEGIN, 3, 4)

		job3 = Job(Set(dependencyA, dependencyB, dependencyC, dependencyD), 6, 1)
		job4  = Job(Set(dependencyE, dependencyF, dependencyG, dependencyH), 6, 2)
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
	it should "test nominal, min normal config" in {
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

	// isPrecedingBeginEndValid testing
	// Structured basis: nominal case: all boolean conditions are true
	// Good data: minimum normal configuration, 2 jobs
	behavior of "isPrecedingBeginEndValid"
	it should "test nominal, min normal config" in {
		val dependency = Dependency(Dependency.BEGIN_END, 1, 2)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)
		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingBeginEndValid = PrivateMethod[Boolean]('isPrecedingBeginEndValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingBeginEndValid(jobB, jobA, schedule, 0)
		assert(validityResult)
	}

	// Structured basis: the first if is false
	it should "test with no Begin-End dependency" in {
		val isPrecedingBeginEndValid = PrivateMethod[Boolean]('isPrecedingBeginEndValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingBeginEndValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Bad data: subschedule is Nil
	it should "test with bad data: Nil subschedule" in {
		val isPrecedingBeginEndValid = PrivateMethod[Boolean]('isPrecedingBeginEndValid)
		schedule = clearSchedule
		val validityResult = SoftwarePlatform invokePrivate isPrecedingBeginEndValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Good data: max normal configuration, 100 jobs
	it should "test with max normal configuration: 100 jobs" in {
		for (i <- 0 to 98) {
			val job = Job(Set(), 4, i)
			appendJobToSchedule(job, schedule)
		}
		val isPrecedingBeginEndValid = PrivateMethod[Boolean]('isPrecedingBeginEndValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingBeginEndValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// isPrecedingDurationsValid testing
	// Structured basis: nominal case: all boolean conditions are true
	// Good data: minimum normal configurations: 2 jobs
	behavior of "isPrecedingDurationsValid"
	it should "test nominal, min normal config" in {
		val dependencyA = Dependency(Dependency.BEGIN_END, 1, 2)
		val dependencyB = Dependency(Dependency.END_END, 1, 2)

		val jobA = Job(Set(dependencyA, dependencyB), 6, 1)
		val jobB = Job(Set(), 6, 2)
		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingDurationsValid = PrivateMethod[Boolean]('isPrecedingDurationsValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDurationsValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured basis: first if condition false, second true
	it should "test with no Begin-End dependency, invalid End-End dependency" in {
		val dependency = Dependency(Dependency.END_END, 1, 2)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)
		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingDurationsValid = PrivateMethod[Boolean]('isPrecedingDurationsValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDurationsValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured basis: second if condition false, first true
	it should "test with no End-End dependency, invalid Begin-End dependency" in {
		val dependency = Dependency(Dependency.BEGIN_END, 1, 3)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)
		val jobC = Job(Set(), 5, 3)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)
		appendJobToSchedule(jobC, schedule)

		val isPrecedingDurationsValid = PrivateMethod[Boolean]('isPrecedingDurationsValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDurationsValid(jobC, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured basis: both if conditions false
	it should "test with no End-End dependency, no Begin-END dependency" in {
		val isPrecedingDurationsValid = PrivateMethod[Boolean]('isPrecedingDurationsValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDurationsValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Bad data: subschedule is Nil
	it should "test with bad data: Nil subschedule" in {
		val isPrecedingDurationsValid = PrivateMethod[Boolean]('isPrecedingDurationsValid)
		schedule = clearSchedule
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDurationsValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Good data: max normal configuration, 100 jobs
	it should "test with max normal configuration: 100 jobs" in {
		for (i <- 0 to 98) {
			val job = Job(Set(), 4, i)
			appendJobToSchedule(job, schedule)
		}
		val isPrecedingDurationsValid = PrivateMethod[Boolean]('isPrecedingDurationsValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDurationsValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// isPrecedingDependencyValid testing
	// Structured Basis: nominal case, all boolean conditions are true
	// Good data: minimum normal configuration, 2 jobs
	behavior of "isPrecedingDependencyValid"
	it should "test nominal, min normal config" in {
		val dependencyA = Dependency(Dependency.END_BEGIN, 1, 2)
		val dependencyB = Dependency(Dependency.BEGIN_BEGIN, 1, 2)

		val jobA = Job(Set(dependencyA, dependencyB), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured Basis: first if condition true, second false
	it should "test with no END_BEGIN, but a BEGIN_BEGIN dependency" in {
		val dependencyB = Dependency(Dependency.BEGIN_BEGIN, 1, 2)

		val jobA = Job(Set(dependencyB), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured Basis: first if condition false, second true
	it should "test with no BEGIN_BEGIN, but a END_BEGIN dependency" in {
		val dependencyB = Dependency(Dependency.END_BEGIN, 1, 2)

		val jobA = Job(Set(dependencyB), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured Basis: both first if conditions false, invalid preceding durations
	it should "test with no BEGIN_BEGIN or END_BEGIN, invalid preceding durations" in {
		val dependencyB = Dependency(Dependency.END_END, 1, 2)

		val jobA = Job(Set(dependencyB), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(jobB, jobA, schedule, 0)
		assert(!validityResult)
	}

	// Structured Basis: both first if conditions false, valid preceding durations
	it should "test with no BEGIN_BEGIN or END_BEGIN, valid preceding durations" in {
		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Bad data: subschedule is Nil
	it should "test with bad data: Nil subschedule" in {
		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		schedule = clearSchedule
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// Good data: max normal configuration, 100 jobs
	it should "test with max normal configuration: 100 jobs" in {
		for (i <- 0 to 98) {
			val job = Job(Set(), 4, i)
			appendJobToSchedule(job, schedule)
		}
		val isPrecedingDependencyValid = PrivateMethod[Boolean]('isPrecedingDependencyValid)
		val validityResult = SoftwarePlatform invokePrivate isPrecedingDependencyValid(job2, job1, schedule, 0)
		assert(validityResult)
	}

	// arePrecedingDependenciesValid testing
	// Structured basis: nominal case, all boolean conditions are true
	// Good data: minimum normal config, 1 job
	behavior of "arePrecedingDependenciesValid"
	it should "test nominal, min normal config" in {
		val dependency = Dependency(Dependency.BEGIN_BEGIN, 1, 2)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)

		val arePrecedingDependenciesValid = PrivateMethod[Boolean]('arePrecedingDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate arePrecedingDependenciesValid(schedule, jobB)
		assert(!validityResult)
	}

	// Structured Basis: first if condition is false
	it should "test with valid preceding dependencies" in {
		val arePrecedingDependenciesValid = PrivateMethod[Boolean]('arePrecedingDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate arePrecedingDependenciesValid(schedule, job2)
		assert(validityResult)
	}

	// Bad data: subschedule is Nil
	// Structured Basis: first for condition is false
	it should "test with bad data: Nil subschedule, no jobs in schedule" in {
		val arePrecedingDependenciesValid = PrivateMethod[Boolean]('arePrecedingDependenciesValid)
		schedule = clearSchedule
		val validityResult = SoftwarePlatform invokePrivate arePrecedingDependenciesValid(schedule, job2)
		assert(validityResult)
	}

	// Good data: max normal configuration, 100 jobs
	// Structured Basis: second for condition is false
	it should "test with max normal configuration: 100 jobs, no dependencies" in {
		for (i <- 0 to 98) {
			val job = Job(Set(), 4, i)
			appendJobToSchedule(job, schedule)
		}
		val arePrecedingDependenciesValid = PrivateMethod[Boolean]('arePrecedingDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate arePrecedingDependenciesValid(schedule, job2)
		assert(validityResult)
	}

	//isParallelEndEndValid testing
	// Structured Basis: nominal case, all boolean conditions are true
	// Good data: normal configuration, one END_END dependency on a job
	behavior of "isParallelEndEndValid"
	it should "test nominal, normal configuration" in {
		val dependency = Dependency(Dependency.END_END, 1, 2)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		insertJobToSchedule(jobB, schedule, 0)

		val isParallelEndEndValid = PrivateMethod[Boolean]('isParallelEndEndValid)
		val validityResult = SoftwarePlatform invokePrivate isParallelEndEndValid(jobA, jobB)
		assert(validityResult)
	}

	// Structured Basis: if statement false, no end end dependency
	// Good data: minimum configuration, two jobs with no dependencies each
	it should "test with no END_END dependency, min config" in {
		val isParallelEndEndValid = PrivateMethod[Boolean]('isParallelEndEndValid)
		val validityResult = SoftwarePlatform invokePrivate isParallelEndEndValid(job1, job2)
		assert(validityResult)
	}

	// Bad data: 4*n dependencies
	it should "test with bad data: 4*n dependencies" in {
		appendJobToSchedule(job3, schedule)
		insertJobToSchedule(job4, schedule, 0)

		val isParallelEndEndValid = PrivateMethod[Boolean]('isParallelEndEndValid)
		val validityResult = SoftwarePlatform invokePrivate isParallelEndEndValid(job3, job4)
		assert(validityResult)
	}

	// areParallelDependenciesValid testing
	// Structured Basis: nominal case, all boolean conditions are true
	// Good data: min configuration, list of one job
	behavior of "areParallelDependeneciesValid"
	it should "test nominal, min normal configuration" in {
		schedule = clearSchedule
		appendJobToSchedule(job1, schedule)

		val areParallelDependenciesValid = PrivateMethod[Boolean]('areParallelDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate areParallelDependenciesValid(job1, ListBuffer(job1))
		assert(validityResult)
	}

	// Structured Basis: second if: first condition true, second false
	it should "test with END_BEGIN dependency on the job, no End_End" in {
		val dependency = Dependency(Dependency.END_BEGIN, 1, 2)

		val jobA = Job(Set(dependency), 6, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		insertJobToSchedule(jobB, schedule, 0)

		val areParallelDependenciesValid = PrivateMethod[Boolean]('areParallelDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate areParallelDependenciesValid(jobB, ListBuffer(jobA))
		assert(!validityResult)
	}

	// Structured Basis: second if: first condition false, second true
	it should "test with no END_BEGIN dependency on the job, invalid End_End" in {
		val dependency = Dependency(Dependency.END_END, 1, 2)

		val jobA = Job(Set(dependency), 3, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		insertJobToSchedule(jobB, schedule, 0)

		val areParallelDependenciesValid = PrivateMethod[Boolean]('areParallelDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate areParallelDependenciesValid(jobB, ListBuffer(jobA))
		assert(!validityResult)
	}

	// Structured Basis: second if: first condition false, second false
	it should "test with no END_BEGIN dependency on the job, no End_End" in {
		val jobA = Job(Set(), 3, 1)
		val jobB = Job(Set(), 6, 2)

		schedule = clearSchedule
		appendJobToSchedule(jobA, schedule)
		insertJobToSchedule(jobB, schedule, 0)

		val areParallelDependenciesValid = PrivateMethod[Boolean]('areParallelDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate areParallelDependenciesValid(jobB, ListBuffer(jobA))
		assert(validityResult)
	}

	// Bad data: empty list
	// Structured Basis: for condition false
	it should "test with Nil jobList, no jobs in job list" in {
		val areParallelDependenciesValid = PrivateMethod[Boolean]('areParallelDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate areParallelDependenciesValid(job1, ListBuffer())
		assert(validityResult)
	}

	// Good data: max normal config, 100 jobs
	it should "test with max normal config" in {
		val jobsList = ListBuffer[Job]()
		for (i <- 1 to 100) {
			jobsList += Job(Set(), 5, i)
		}
		val areParallelDependenciesValid = PrivateMethod[Boolean]('areParallelDependenciesValid)
		val validityResult = SoftwarePlatform invokePrivate areParallelDependenciesValid(job1, jobsList)
		assert(validityResult)
	}

	// isValid testing
	// Structured Basis: nominal case, all boolean conditions true
	// Good data: average case, a few jobs, some parallel, some sequential, with dependencies
	behavior of "isValid"
	it should "test nominal, average case" in {
		schedule = clearSchedule
		val dependencyA = Dependency(Dependency.END_BEGIN, 1, 2)
		val dependencyB = Dependency(Dependency.BEGIN_BEGIN, 3, 2)

		val jobA = Job(Set(dependencyA), 6, 1)
		val jobB = Job(Set(), 6, 2)
		val jobC = Job(Set(dependencyB), 6, 3)

		appendJobToSchedule(jobC, schedule)
		appendJobToSchedule(jobB, schedule)
		insertJobToSchedule(jobA, schedule, 1)

		val isValid = PrivateMethod[Boolean]('isValid)
		val validityResult = SoftwarePlatform invokePrivate isValid(schedule)
		assert(!validityResult)
	}

	// Structured Basis: first condition false, second condition true
	it should "test with valid parallel dependencies, invalid preceding dependnecies" in {
		schedule = clearSchedule
		val dependencyA = Dependency(Dependency.END_BEGIN, 1, 2)

		val jobA = Job(Set(dependencyA), 6, 1)
		val jobB = Job(Set(), 6, 2)
		val jobC = Job(Set(), 6, 3)

		appendJobToSchedule(jobC, schedule)
		appendJobToSchedule(jobB, schedule)
		insertJobToSchedule(jobA, schedule, 1)

		val isValid = PrivateMethod[Boolean]('isValid)
		val validityResult = SoftwarePlatform invokePrivate isValid(schedule)
		assert(!validityResult)
	}

	// Structured Basis: first condition true, second condition false
	it should "test with invalid parallel dependencies, valid preceding dependnecies" in {
		schedule = clearSchedule
		val dependency = Dependency(Dependency.END_END, 3, 2)

		val jobA = Job(Set(), 6, 1)
		val jobB = Job(Set(), 6, 2)
		val jobC = Job(Set(dependency), 4, 3)

		appendJobToSchedule(jobC, schedule)
		appendJobToSchedule(jobB, schedule)
		insertJobToSchedule(jobA, schedule, 1)

		val isValid = PrivateMethod[Boolean]('isValid)
		val validityResult = SoftwarePlatform invokePrivate isValid(schedule)
		assert(!validityResult)
	}

	// Structured Basis: if statement false
	// Good data: min nominal case, a schedule of 1 job
	it should "test with valid parallel dependencies, valid preceding dependencies" in {
		schedule = clearSchedule
		appendJobToSchedule(job1, schedule)

		val isValid = PrivateMethod[Boolean]('isValid)
		val validityResult = SoftwarePlatform invokePrivate isValid(schedule)
		assert(validityResult)
	}

	// Bad data: Empty schedule
	// Structured basis: no jobs in schedule
	it should "test with a Nil schedule, no jobs in schedule" in {
		val isValid = PrivateMethod[Boolean]('isValid)
		val validityResult = SoftwarePlatform invokePrivate isValid(ListBuffer[ListBuffer[Job]]())
		assert(validityResult)
	}

	// Good data: max nominal case, 100 jobs
	it should "test with max normal configuration, 100 jobs" in {
		for (i <- 3 to 100) {
			appendJobToSchedule(Job(Set(), 4, i), schedule)
		}

		val isValid = PrivateMethod[Boolean]('isValid)
		val validityResult = SoftwarePlatform invokePrivate isValid(schedule)
		assert(validityResult)
	}

	//jobListDuration testing
	// Structured Basis: nominal case, all boolean conditions true
	// Good data: min nominal case, 1 job in the schedule
	behavior of "jobListDuration"
	it should "test nominal, min normal configuration" in {
		schedule = clearSchedule
		appendJobToSchedule(Job(Set(), 4, 1), schedule)

		val jobListDuration = PrivateMethod[Int]('jobListDuration)
		val duration = SoftwarePlatform invokePrivate jobListDuration(schedule)
		assert(duration == 4)
	}

	// Structured Basis: if statement false
	// Good data: average case. A few jobs in both parallel and sequential with each other
	it should "test with more than one job in the schedule" in {
		schedule = clearSchedule
		appendJobToSchedule(Job(Set(), 4, 1), schedule)
		appendJobToSchedule(Job(Set(), 5, 2), schedule)
		insertJobToSchedule(Job(Set(), 5, 3), schedule, 0)

		val jobListDuration = PrivateMethod[Int]('jobListDuration)
		val duration = SoftwarePlatform invokePrivate jobListDuration(schedule)
		assert(duration == 10)
	}

	// Structured Basis: for condition false
	// Bad data: No jobs in schedule
	it should "test with Nil schedule, no jobs in schedule" in {
		schedule = clearSchedule

		val jobListDuration = PrivateMethod[Int]('jobListDuration)
		val duration = SoftwarePlatform invokePrivate jobListDuration(schedule)
		assert(duration == 0)

	}

	// Good data: max nominal case, 100 jobs
	it should "test with max nominal case, 100 jobs" in {
		schedule = clearSchedule
		for (i <- 1 to 100) {
			appendJobToSchedule(Job(Set(), 1, i), schedule)
		}

		val jobListDuration = PrivateMethod[Int]('jobListDuration)
		val duration = SoftwarePlatform invokePrivate jobListDuration(schedule)
		assert(duration == 100)
	}

	//invalidDependentJobs testing
	// Structured Basis: nominal case, all boolean conditions true
	// Good data: min configuration, 1 job depends on another job, nothing else
	behavior of "invalidDependentJobs"
	it should "test nominal, min config" in {
		val dependency = Dependency(Dependency.BEGIN_BEGIN, 1, 2)

		val jobA = Job(Set(dependency), 4, 1)
		val jobB = Job(Set(), 5, 2)

		val jobsList = List(jobA)

		val invalidDependentJobs = PrivateMethod[List[Job]]('invalidDependentJobs)
		val list = SoftwarePlatform invokePrivate invalidDependentJobs(jobB, jobsList)
		assert(list.contains(jobA) && list.size == 2)
	}

	// Structured Basis: if statement false
	// Bad data: No jobs in jobList relate to the passed job in question
	it should "test with no jobs relating to the given job" in {
		val dependency = Dependency(Dependency.BEGIN_BEGIN, 1, 3)

		val jobA = Job(Set(dependency), 4, 1)
		val jobB = Job(Set(), 5, 2)

		val jobsList = List(jobA)

		val invalidDependentJobs = PrivateMethod[List[Job]]('invalidDependentJobs)
		val list = SoftwarePlatform invokePrivate invalidDependentJobs(jobB, jobsList)
		assert(list.size == 1 && list.contains(jobB))
	}

	// Structured Basis: for condition is false
	// Bad data: Nil list
	it should "test with Nil list, no jobs in the job list" in {
		val jobA = Job(Set(), 5, 2)
		val jobsList = List()

		val invalidDependentJobs = PrivateMethod[List[Job]]('invalidDependentJobs)
		val list = SoftwarePlatform invokePrivate invalidDependentJobs(jobA, jobsList)
		assert(list.size == 1 && list.contains(jobA))

	}

	// Good data: average config case, a few jobs that relate to the given job
	// Good data: max config case, 100 jobs
	it should "test with average relation nominal case, max #jobs normal config" in {
		val dependencyA = Dependency(Dependency.BEGIN_BEGIN, 1, 4)
		val dependencyB = Dependency(Dependency.BEGIN_BEGIN, 2, 4)
		val dependencyC = Dependency(Dependency.BEGIN_BEGIN, 3, 4)

		val jobA = Job(Set(dependencyA), 4, 1)
		val jobB = Job(Set(dependencyB), 4, 2)
		val jobC = Job(Set(dependencyC), 4, 3)
		val jobD = Job(Set(), 4, 4)

		val jobsList = List(jobA, jobB, jobC)


		for (i <- 5 to 100) {
			Job(Set(), 4, i) :: jobsList
		}

		val invalidDependentJobs = PrivateMethod[List[Job]]('invalidDependentJobs)
		val list = SoftwarePlatform invokePrivate invalidDependentJobs(jobD, jobsList)
		assert(list.size == 4 && list.contains(jobA) && list.contains(jobB) && list.contains(jobC)
		&& list.contains(jobD))
	}

	// minimumDurationSchedule testing
	// Structured Basis: nominal case, all boolean conditions true
	// Good data: min config, 2 schedules
	behavior of "minDurationSchedule"
	it should "test nominal, min config, 2 schedules" in {
		val scheduleA = schedule
		val scheduleB = clearSchedule
		appendJobToSchedule(Job(Set(), 23, 4), scheduleB)

		val scheduleList = List(scheduleB, scheduleA)

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(scheduleList)
		assert(list == scheduleA)
	}

	// Structured Basis: First if statement false
	// Bad data: Empty schedules list
	it should "test with Nil schedules" in {
		val nilSchedules: List[ListBuffer[ListBuffer[Job]]] = Nil

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(nilSchedules)
		assert(list == clearSchedule)
	}

	// Structured Basis: For loop condition false
	// Bad data: only one schedule
	it should "Test one schedule, list size of 1" in {
		val schedule = clearSchedule
		appendJobToSchedule(Job(Set(), 23, 4), schedule)

		val scheduleList = List(schedule)

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(scheduleList)
		assert(list == schedule)
	}

	// Structured Basis: second if statement false
	// Boundary condition: duration > minDuration
	// Good data: average normal case, more than 1 schedule, not too many
	it should "Test with first schedule being the minimum, duration greater than minDuration, average normal case" in {
		val scheduleA = schedule
		val scheduleB = clearSchedule
		appendJobToSchedule(Job(Set(), 23, 4), scheduleB)

		val scheduleList = List(scheduleA, scheduleB)

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(scheduleList)
		assert(list == scheduleA)
	}

	// Boundary condition: duration = minDuration
	it should "Test with first schedule being the minimum, duration equals minDuration" in {
		val scheduleA = clearSchedule
		appendJobToSchedule(Job(Set(), 4, 1), scheduleA)
		val scheduleB = clearSchedule
		appendJobToSchedule(Job(Set(), 4, 4), scheduleB)

		val scheduleList = List(scheduleA, scheduleB)

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(scheduleList)
		assert(list == scheduleA)
	}

	// Boundary condition: duration < minDuration
	it should "Test with first schedule being the minimum, duration less than minDuration" in {
		val scheduleA = clearSchedule
		appendJobToSchedule(Job(Set(), 4, 1), scheduleA)

		val scheduleB = clearSchedule
		appendJobToSchedule(Job(Set(), 3, 4), scheduleB)

		val scheduleList = List(scheduleA, scheduleB)

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(scheduleList)
		assert(list == scheduleB)
	}

	// Good data: maximum normal config, 20 schedules
	it should "Test with max normal config, 20 schedules" in {
		val buffer = ListBuffer[ListBuffer[ListBuffer[Job]]]()
		buffer += clearSchedule
		val job = Job(Set(), 0, 1)
		appendJobToSchedule(job, buffer(0))

		for (i <- 1 to 20) {
			buffer += clearSchedule
			appendJobToSchedule(Job(Set(), i, i), buffer(i))
		}

		val minimumDurationSchedule = PrivateMethod[ListBuffer[ListBuffer[Job]]]('minimumDurationSchedule)
		val list = SoftwarePlatform invokePrivate minimumDurationSchedule(buffer.toList)
		assert(list(0)(0) == job)
	}
}




