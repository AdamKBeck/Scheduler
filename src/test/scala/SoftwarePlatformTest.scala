package scheduler

import org.scalatest.FlatSpec
import scala.collection.mutable.ListBuffer

class SoftwarePlatformTest extends FlatSpec {
	behavior of "jobListDuration for a job list of single elements"
	it should "return the sum of each element's duration as the list duration" in {
		val job1 = Job(Set(), 7, 1)
		val job2 = Job(Set(), 8, 2)
		val schedule = ListBuffer[ListBuffer[Job]]()
		appendJobToSchedule(job1, schedule)
		appendJobToSchedule(job2, schedule)

		assert(SoftwarePlatform.jobListDuration(schedule) == 15)
	}

	behavior of "jobListDuration for a job list of chained elements"
	it should "return the sum of (single element + max{chained elements})" in {
		val job1 = Job(Set(), 7, 1)
		val job2 = Job(Set(), 8, 2)
		val job3 = Job(Set(), 8, 4)
		val schedule = ListBuffer[ListBuffer[Job]]()
		appendJobToSchedule(job1, schedule)
		appendJobToSchedule(job2, schedule)
		insertJobToSchedule(job3, schedule, 0)

		assert(SoftwarePlatform.jobListDuration(schedule) == 16)

	}

	behavior of "circularDependency"
	it should "return a set of jobs depending on a target job" in {
		val dependency = Dependency(Dependency.BEGIN_BEGIN, 10, 2)
		val dependency2 = Dependency(Dependency.END_END, 4, 2)

		val job1 = Job(Set(dependency), 5, 10)
		val job2 = Job(Set(), 3, 2)
		val job3 = Job(Set(dependency2), 4, 5)
		val job4 = Job(Set(), 4, 2)
		val jobList = List(job1, job3, job4)

		val circularDependency = SoftwarePlatform.invalidDependentJobs(job2, jobList)

		for (job <- circularDependency) {
			val dependencies = job.dependencies

			for (d <- dependencies) {
				assert(d.dependencyID == job2.id)
			}
		}
	}

	behavior of "isValid"
	it should "return correctly for simple valid schedules, incorrect parallel and incorrect sequential schedules" in {

		// Simple valid schedule
		val job1 = Job(Set(), 7, 1)
		val job2 = Job(Set(), 8, 2)
		val job3 = Job(Set(), 8, 4)
		val schedule = ListBuffer[ListBuffer[Job]]()
		appendJobToSchedule(job1, schedule)
		appendJobToSchedule(job2, schedule)
		insertJobToSchedule(job3, schedule, 0)

		assert(SoftwarePlatform.isValid(schedule))

		// Incorrect parallel schedule
		val dependency = Dependency(Dependency.BEGIN_BEGIN, 5, 2)
		val job4 = Job(Set(dependency), 10, 5)
		insertJobToSchedule(job4, schedule, 0)
		assert(!SoftwarePlatform.isValid(schedule))

	}

	behavior of "isValid for parallel dependencies"
	it should "return correctly" in {
		val dependency = Dependency(Dependency.END_END, 1, 2)
		val job = Job(Set(dependency), 10, 1)
		val job2 = Job(Set(), 11, 2)
		val schedule = ListBuffer[ListBuffer[Job]]()
		schedule += ListBuffer[Job]()
		insertJobToSchedule(job, schedule, 0)
		insertJobToSchedule(job2, schedule, 0)
		assert(!SoftwarePlatform.isValid(schedule))
	}


	behavior of "isValid for correct dependencies but possibly incorrect durations"
	it should "return correctly" in {
		// isPrecedingEEValid
		val dependency = Dependency(Dependency.END_END, 1, 2)
		val job = Job(Set(dependency), 10, 1)
		val job2 = Job(Set(), 10, 2)
		val schedule = ListBuffer[ListBuffer[Job]]()
		appendJobToSchedule(job, schedule)
		appendJobToSchedule(job2, schedule)
		assert(!SoftwarePlatform.isValid(schedule))


		// isPrecedingBEValid
		val dependency3 = Dependency(Dependency.END_END, 3, 2)

		val job4 = Job(Set(), 0, 2)
		val job5 = Job(Set(dependency3), 12, 1)

		val schedule2 = ListBuffer[ListBuffer[Job]]()
		appendJobToSchedule(job4, schedule2)
		insertJobToSchedule(job5, schedule2, 0)
		assert(SoftwarePlatform.isValid(schedule2))
		val jobList = List(job4, job5)
	}


	behavior of "estimated Delivery time"
	it should "Return the minimum delivery time for a list of jobs" in {
		val job1 = Job(Set(), 7, 1)
		val job2 = Job(Set(), 8, 2)
		val job3 = Job(Set(), 8, 4)
		val jobList = List(job1, job2, job3)

		assert(SoftwarePlatform.estimateDeliveryTime(jobList) == 8)
	}

	// Appends a job to the end of a schedule in new separate list
	def appendJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]]): Unit = {
		schedule += ListBuffer[Job](job)
	}

	// Appends a job to a list of given index in a schedule
	def insertJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]], index: Int): Unit = {
		schedule(index) += job
	}

}
