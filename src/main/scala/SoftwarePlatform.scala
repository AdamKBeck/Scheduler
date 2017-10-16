package scheduler

import scala.collection.mutable.ListBuffer

case class SoftwarePlatform() {

}

object SoftwarePlatform {

	/* Input: Assignments 'jobs' with their durations and requirements
	 * Output: Estimated delivery time or a circular requirement
	 *
	 * jobs' is a List because the order matters. Circular requirement needs the jobs we've
	 * encountered, which can be accomplished by splicing our list at the index we are currently on
	 * when looping through the list.
	 */
	def estimatedDeliveryTime(jobs: List[Job]): Int = {
		???
	}

	/* Input: Job 'job', a job list of lists 'schedule', and the duration of the list 'scheduleDuration'
	 * Output: Returns a job list of lists, the valid ordering of least duration of 'job' into 'schedule'
	 */
	def bestValidOrdering(job: Job, schedule: ListBuffer[ListBuffer[Job]], scheduleDuration: Int): ListBuffer[ListBuffer[Job]] = {
		???
	}

	/* Input: Job 'job', a set of assignments 'Jobs'
	 * Output: Returns a set of jobs which forms a circular dependency
	 */
	def circularDependency(job: Job, jobs: Set[Job]): Set[Job] = {
		???
	}

	/* Input: A job list of list of jobs 'schedule'
	 * Output: The duration of schedule
	 */
	def jobListDuration(schedule: ListBuffer[ListBuffer[Job]], scheduleDuration: Int) = {
		
	}
}
