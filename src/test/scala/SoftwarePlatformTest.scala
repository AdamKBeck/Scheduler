package scheduler

import org.scalatest.{BeforeAndAfterEach, FlatSpec, PrivateMethodTester}

import scala.collection.mutable.ListBuffer

class SoftwarePlatformTest extends FlatSpec with BeforeAndAfterEach{

	var schedule = emptySchedule

	private def emptySchedule = ListBuffer[ListBuffer[Job]]()

	// Sets up a simple schedule with a 2 non-parallel jobs, no dependencies
	override def beforeEach(): Unit = {
		val jobA = Job(Set(), 7, 1)
		val jobB = Job(Set(), 3, 2)
		appendJobToSchedule(jobA, schedule)
		appendJobToSchedule(jobB, schedule)
	}

	override def afterEach(): Unit = {
	}

	// Appends a job to the end of a schedule in new separate list
	def appendJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]]): Unit = {
		schedule += ListBuffer[Job](job)
	}

	// Appends a job to a list of given index in a schedule
	def insertJobToSchedule(job: Job, schedule: ListBuffer[ListBuffer[Job]], index: Int): Unit = {
		schedule(index) += job
	}

	// emptySchedule testing
	
}
