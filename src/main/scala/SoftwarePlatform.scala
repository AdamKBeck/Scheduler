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
	def estimateDeliveryTime(jobs: List[Job]): Int = {
		// if jobs is empty return 0
		if (jobs.isEmpty) {
			return 0 // Early termination to reduce method nesting for Dan's suggestion. Otherwise, if statement has the common path.
		}

		// let L be a new list of lists
		var schedule = emptySchedule

		// Insert J[0] to L
		schedule += ListBuffer[Job]()
		schedule.head += jobs.head

		// totalDuration <- J[0].duration
		var totalDuration = jobs.head.duration

		//for each j in J where j not equal to J[0]
		for (job <- jobs.tail) {
			//	L <- bestValidOrdering(j, L, totalDuration)
			schedule = bestValidOrdering(job, schedule, totalDuration)
			//	if L is empty return circularDependency(j, list of jobs we've encountered so far)
			if (schedule.isEmpty) {
				val jobsEncountered = jobs.take(jobs.indexOf(job))
				throw DependencyException.CIRCULAR_DEPENDENCY(invalidDependentJobs(job, jobsEncountered))
			}
			//	else totalDuration <- jobListDuration(L)
			else {
				totalDuration = jobListDuration(schedule)
			}
		}
		// return jobListDuration(L)
		jobListDuration(schedule)
	}

	/* Input: Job 'job', a job list of lists 'schedule', and the duration of the list 'scheduleDuration'
	 * Output: Returns a job list of lists, the valid ordering of least duration of 'job' into 'schedule'
	 */
	private def bestValidOrdering(job: Job, schedule: ListBuffer[ListBuffer[Job]], scheduleDuration: Int): ListBuffer[ListBuffer[Job]] = {
		// maxOrdering <- d + j.duration // helpful for our min block below
		var minDuration = scheduleDuration + job.duration // We set our running min Duration to the max possible duration to

		// numInsertions = L.length + 1 (Not +1, error found in class)
		val numInsertions = schedule.length

		// let minDurationList be a new list of lists
		var minDurationList = emptySchedule

		// minDurationList <- min {
		//	for i <- 1 to numInsertions
		for (insertionIndex <- 0 until numInsertions) {
			// tempDurationList <- L
			// Inserts a job around a specified index. Returns the insertion of least duration
			val scheduleCopy = bestValidInsertionAroundSlot(job, schedule, insertionIndex)

			// Call isListValid(tempDurationList), proceed if verifies
			if (scheduleCopy.nonEmpty && isValid(scheduleCopy)) {
				// jobListDuration(tempDurationList)
				val duration = jobListDuration(scheduleCopy)
				if (duration < minDuration) {
					minDuration = duration
					minDurationList = scheduleCopy
				}
			}
		}
		// }

		// return the duration of minDurationList (error as discussed in class, return the list itself not the duration which is in estimated delivery time)
		minDurationList
	}

	/* Helper method for bestValidOrdering. Inserts job around specified index into a schedule.
	 * Named as a function because it returns a schedule of least duration for the three cases of inserting around a slot.
	 */
	private def bestValidInsertionAroundSlot(job: Job, schedule: ListBuffer[ListBuffer[Job]], index: Int): ListBuffer[ListBuffer[Job]] = {
		// Create a list of lists containing just the job for inserting before and after
		val listsOfJob = emptySchedule
		listsOfJob += ListBuffer[Job]()
		listsOfJob.head += job

		// Append job to a parallel list at the specified index for inserting in parallel
		val listsOfParallelJob = emptySchedule
		listsOfParallelJob += schedule(index) ++ ListBuffer[Job](job)

		// Insert the job before, in parallel, and after the specified slot index
		val insertBefore = schedule.slice(0, index) ++ listsOfJob ++ schedule.slice(index, schedule.length)
		val insertParallel = schedule.slice(0, index) ++ listsOfParallelJob ++ schedule.slice(index+1, schedule.length)
		val insertAfter = schedule.slice(0, index+1) ++ listsOfJob ++ schedule.slice(index+1, schedule.length)

		val schedules = scala.collection.mutable.Set[ListBuffer[ListBuffer[Job]]]()
		schedules += insertBefore += insertParallel += insertAfter

		// Remove invalid schedules
		schedules.filter(isValid(_))

		// Return the insertion of least duration, or an empty list if nothing was valid
		if (schedules.isEmpty) {
			emptySchedule
		}
		else {
			// Find the minimum duration of potentially 3 valid schedules
			minimumDurationSchedule(schedules.toList)
		}

	}

	// Helper method for bestValidInsertionAroundSlot, finds the minimum duration schedule out of a list of schedules
	private def minimumDurationSchedule(schedules: List[ListBuffer[ListBuffer[Job]]]): ListBuffer[ListBuffer[Job]] = {
		if (schedules.nonEmpty) {
			var minDurationSchedule = schedules.head
			var minDuration = jobListDuration(schedules.head)

			for (schedule <- schedules.tail) {
				val duration = jobListDuration(schedule)
				if (jobListDuration(schedule) < minDuration) {
					minDuration = duration
					minDurationSchedule = schedule
				}
			}

			minDurationSchedule
		}
		else {
			emptySchedule
		}
	}

	/* Input: Job 'job', a set of assignments 'Jobs'
	 * Output: Returns a set of jobs which forms a circular dependency
	 */
	private def invalidDependentJobs(job: Job, jobs: List[Job]): List[Job] = {
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
		circularJobs.toList
	}

	/* Input: A job list of list of jobs 'schedule'
	 * Output: The duration of schedule
	 */
	private def jobListDuration(schedule: ListBuffer[ListBuffer[Job]]): Int = {
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
	private def isValid(schedule: ListBuffer[ListBuffer[Job]]): Boolean = {
		var scheduleValidity = true // By default, an empty schedule is already valid

		// for each l in L
		for ((jobList, listIndex) <- schedule.zipWithIndex) {
			// Check if each job j in L is in a valid spot relative to the jobs in L. Return false if not valid
			for (job <- jobList) {
				if (!areParallelDependenciesValid(job, jobList) || !arePrecedingDependenciesValid(schedule.slice(0, listIndex+1), job)) {
					scheduleValidity = false
				}
			}
		}

		// return true
		scheduleValidity
	}


	 /* Helper method for isListValid. Checks if any job in the list has an invalid dependency on the passed job, otherwise
	  * return true */
	private def areParallelDependenciesValid(job: Job, jobList: ListBuffer[Job]): Boolean = {
		var parallelDependenciesValidity = true // True by default, even if the list is empty
		for (j <- jobList) {
			if (j.id == job.id) {

			}
			else if (DependencyException.verify(j, Dependency.END_BEGIN, job.id) || !isParallelEndEndValid(job, j)) {
				parallelDependenciesValidity = false
			}
		}

		parallelDependenciesValidity
	}

	/* Helper method for isParallelListInvalid. Checks if an End-End dependency
	 * from thisJob to thatJob is valid based on their durations.
	 */
	private def isParallelEndEndValid(thisJob: Job, thatJob: Job): Boolean = {
		if (DependencyException.verify(thisJob, Dependency.END_END, thatJob.id)) {
			thisJob.duration >= thatJob.duration
		}

		// If there is no EE exception in between the two jobs, it is satisfied
		else {
			true
		}
	}

	// Helper for isListValid. Checks if preceding dependencies are valid on a passed job
	private def arePrecedingDependenciesValid(subschedule: ListBuffer[ListBuffer[Job]], job: Job): Boolean = {
		// Others can't have end-begin or begin begin
		for (jobList <- subschedule) {
			for ((j, thatJobIndex) <- jobList.zipWithIndex) {
				if (!isPrecedingDependencyValid(job, j, subschedule, thatJobIndex)) {
					return false
				}
			}
		}

		true

	}

	/* Helper method for isPrecendingDependencies Valid. Checks all types of dependencies against thisJob,
	 * and sees if any are of an invalid type or in an invalid place. Named as a procedure as its primary purpose
	 * is to check against preceding dependencies and invalid places.
	 */
	private def isPrecedingDependencyValid(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (DependencyException.verify(thatJob, Dependency.END_BEGIN, thisJob.id) ||
			DependencyException.verify(thatJob, Dependency.BEGIN_BEGIN, thisJob.id)) {
			false
		}

		else if (!isPrecedingDurationsValid(thisJob, thatJob, subschedule, jobIndex)) {
			false
		}

		else {
			true
		}
	}

	/* Helper method for verifyPrecedingDependencies. Checks if the duration of a job 'thatjob' coming before
	 * 'thisjob' is a valid duration (i.e. it is not too large depending on the type of dependency is has on 'thisJob')
	 */
	private def isPrecedingDurationsValid(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (!isPrecedingBeginEndValid(thisJob, thatJob, subschedule, jobIndex) ||
			!isPrecedingEndEndValid(thisJob, thatJob, subschedule, jobIndex)) {
			false
		}
		else {
			true
		}
	}

	/* Helper method for isPrecedingDurationsValid. Checks if a begin-end dependency from a preceding job
	 * to a given job is valid (i.e. if the duration extends past where that given job starts)
	 */
	private def isPrecedingBeginEndValid(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (DependencyException.verify(thatJob, Dependency.BEGIN_END, thisJob.id)) {
			thatJob.duration >= jobListDuration(subschedule.slice(jobIndex, subschedule.size))
		}

		else {
			true
		}
	}

	/* Explanation for why I did not make these methods above and below into one method (PrecedingBE and PRecedingEE methods ^ and V )
	 * With  more paramaters to make it more extensible, a if statement to differentiate between the paramater would bring the total method
	 * complexity up to 4, which is risky.
	 *
	 * Additionally, I wanted to highlight the difference between how EE and BE durations are determined to be valid by separating these methods out.
	 * Notice how BE just gets the duration of a preceding job up to the job in question. EE gets the duration from the preceding job up to
	 * AND INCLUDING the duration of the job in question
	 *
	 * I will acknowledge that these methods look very similar, but combining them could cause people to think that they do the exact same thing, just
	 * for different types of dependencies.
	 */

	/* Helper method for isPrecedingDurationsValid. Checks if an end-end dependency from a preceding job to a given job
	 * is valid (i.e. if the duration is extends past where the given job ends
	 */
	private def isPrecedingEndEndValid(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (DependencyException.verify(thatJob, Dependency.END_END, thisJob.id)) {
			thatJob.duration >= jobListDuration(subschedule.slice(jobIndex, subschedule.size)) + thisJob.duration
		}

		else {
			true
		}
	}

	// Returns an empty job schedule
	private def emptySchedule = ListBuffer[ListBuffer[Job]]()

}
