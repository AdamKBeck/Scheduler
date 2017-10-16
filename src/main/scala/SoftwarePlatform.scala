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
		// if jobs is empty return 0
		// let L be a new list of lists
		// Insert J[0] to L
		// totalDuration <- J[0].duration

		//for each j in J where j not equal to J[0]
		//	L <- bestValidOrdering(j, L, totalDuration)
		//	if L is empty return circularDependency(j, list of jobs we've encountered so far)
		//	else totalDuration <- jobListDuration(L)

		// return jobListDuration(L)

		???
	}

	/* Input: Job 'job', a job list of lists 'schedule', and the duration of the list 'scheduleDuration'
	 * Output: Returns a job list of lists, the valid ordering of least duration of 'job' into 'schedule'
	 */
	def bestValidOrdering(job: Job, schedule: ListBuffer[ListBuffer[Job]], scheduleDuration: Int): ListBuffer[ListBuffer[Job]] = {
		// maxOrdering <- d + j.duration // helpful for our min block below
		// numInsertions = L.length + 1
		// let minDurationList be a new list of lists

		// minDurationList <- min {
		//	for i <- 1 to numInsertions
		//		tempDurationList <- L
		//		Slide j into the next available tempDurationList location
		//		Call isListValid(tempDurationList), proceed if verifies
		//		jobListDUuration(tempDurationList)

		// return the duration of minDurationList
		???
	}

	/* Input: Job 'job', a set of assignments 'Jobs'
	 * Output: Returns a set of jobs which forms a circular dependency
	 */
	def circularDependency(job: Job, jobs: Set[Job]): Set[Job] = {
		// Let l be a new list
		val circularJobs = ListBuffer[Job]()

		// Append j to l
		circularJobs += job

		// for each job in J
		for (j <- jobs) {
			// append job to l if job depends on j
			val jobDependencies = j.dependencies.filter(_.dependencyID == job.id)

			if (jobDependencies.nonEmpty) {
				circularJobs += j
			}

		}

		// return l
		circularJobs.toSet
	}

	/* Input: A job list of list of jobs 'schedule'
	 * Output: The duration of schedule
	 */
	def jobListDuration(schedule: ListBuffer[ListBuffer[Job]]): Int = {
		// totalDuration <- 0
		var totalDuration = 0

		// for l in L
		for (jobList <- schedule) {
			// if l is of length 1
			if (jobList.size == 1) {
				// totalDuration <- totalDuration + the duration of the job in l
				totalDuration += jobList.head.duration
			}
			// else totalDuration <- totalDuration + max{duration of jobs in l}
			else {
				totalDuration += jobList.map(_.duration).max
			}
		}

		//	return totalDuration
		totalDuration

	}

	/* Input: A job lists of lists 'schedule'
	 * Output: Boolean whether or not 'schedule' has jobs which have valid dependencies on each other
	 */
	def isListValid(schedule: ListBuffer[ListBuffer[Job]]): Boolean = {
		// for each l in L
		// 		Check if each job j in L is in a valid spot relative to the jobs in L. Return false if not valid

		// return true

		???
	}
}
