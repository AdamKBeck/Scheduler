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
		val job1 = Job(Set(dependency), 5, 10)
		val job2 = Job(Set(), 3, 2)
		val job3 = Job(Set(), 4, 5)
		val jobList = List(job1, job3)

		val circularDependency = SoftwarePlatform.circularDependency(job2, jobList)

		for (job <- circularDependency) {
			val dependencies = job.dependencies

			for (d <- dependencies) {
				assert(d.dependencyID == job2.id)
			}
		}
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
