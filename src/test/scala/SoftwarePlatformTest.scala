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

	def appendJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]]): Unit = {
		schedule += ListBuffer[Job](job)
	}
}
