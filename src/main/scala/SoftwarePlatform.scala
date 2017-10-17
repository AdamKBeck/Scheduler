package scheduler

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

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
		if (jobs.isEmpty) {
			return 0
		}
		// let L be a new list of lists
		var schedule = ListBuffer[ListBuffer[Job]]()

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
				val jobsEncountered = jobs.slice(0, jobs.indexOf(job))

				throw DependencyException.CIRCULAR_DEPENDENCY(circularDependency(job, jobsEncountered))
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
	def bestValidOrdering(job: Job, schedule: ListBuffer[ListBuffer[Job]], scheduleDuration: Int): ListBuffer[ListBuffer[Job]] = {
		// maxOrdering <- d + j.duration // helpful for our min block below
		var minDuration = scheduleDuration + job.duration // We set our running min Duration to the max possible duration to

		// numInsertions = L.length + 1 (Not +1, error found in class)
		val numInsertions = schedule.length //TODO: Parallel processes might affect this

		// let minDurationList be a new list of lists
		var minDurationList = ListBuffer[ListBuffer[Job]]()

		// minDurationList <- min {
		//	for i <- 1 to numInsertions
		for (insertionIndex <- 0 until numInsertions) {
			// tempDurationList <- L
			// Inserts a job around a specified index. Returns the insertion of least duration
			val scheduleCopy = bestValidInsertionAroundSlot(job, schedule, insertionIndex)

			// Call isListValid(tempDurationList), proceed if verifies
			if (scheduleCopy.nonEmpty || isValid(scheduleCopy)) {
				// jobListDuration(tempDurationList)
				val duration = jobListDuration(scheduleCopy)
				if (duration < minDuration) {
					minDuration = duration
					minDurationList = scheduleCopy
				}
			}
		}
		// }

		// return the duration of minDurationList (error as discussed in class, return the list itself not the duration)
		minDurationList
	}

	/* Helper method for bestValidOrdering. Inserts job around specified index into a schedule.
	 * Named as a function because it returns a schedule of least duration for the three cases of inserting around a slot.
	 * Returns the schedule of least duration.
	 */
	def bestValidInsertionAroundSlot(job: Job, schedule: ListBuffer[ListBuffer[Job]], index: Int): ListBuffer[ListBuffer[Job]] = {
		// Create a list of lists containing just the job for inserting before and after
		val listsOfJob = ListBuffer[ListBuffer[Job]]()
		listsOfJob += ListBuffer[Job]()
		listsOfJob.head += job

		// Append job to a parallel list at the specified index for inserting in parallel
		val listsOfParallelJob = ListBuffer[ListBuffer[Job]]()
		listsOfParallelJob += schedule(index)
		listsOfParallelJob.head += job

		// Insert the job before, in parallel, and after the specified slot index
		val insertBefore = schedule.slice(0, index) ++ listsOfJob ++ schedule.slice(index, schedule.length)
		val insertParallel = schedule.slice(0, index) ++ listsOfParallelJob ++ schedule.slice(index+1, schedule.length)
		val insertAfter = schedule.slice(0, index+1) ++ listsOfJob ++ schedule.slice(index+1, schedule.length)

		val validSchedules = scala.collection.mutable.Set[ListBuffer[ListBuffer[Job]]]()
		validSchedules += insertBefore += insertParallel += insertAfter

		// Remove invalid schedules
		for (s <- validSchedules) {
			if (!isValid(s)) {
				validSchedules -= s
			}
		}

		// Return the insertion of least duration, or an empty list if nothing was valid
		if (validSchedules.isEmpty) {
			new ListBuffer[ListBuffer[Job]]()
		}
		else {
			validSchedules.reduceLeft(minSchedule)
		}

	}

	// Helper method for bestValidInsertAroundSlot, specifically for reduceLeft. Function returns the minimum schedule based on duration
	def minSchedule(s1: ListBuffer[ListBuffer[Job]], s2: ListBuffer[ListBuffer[Job]]): ListBuffer[ListBuffer[Job]]= {
		if (jobListDuration(s1) < jobListDuration(s2)) s1 else s2
	}

	/* Input: Job 'job', a set of assignments 'Jobs'
	 * Output: Returns a set of jobs which forms a circular dependency
	 */
	def circularDependency(job: Job, jobs: List[Job]): List[Job] = {
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
	//TODO: rename to isScheduleValid?
	def isValid(schedule: ListBuffer[ListBuffer[Job]]): Boolean = {
		// for each l in L
		for ((jobList, listIndex) <- schedule.zipWithIndex) {
			// Check if each job j in L is in a valid spot relative to the jobs in L. Return false if not valid
			for ((job, jobIndex) <- jobList.zipWithIndex) {
				if (!isParallelDependenciesValid(job, jobIndex, jobList) || !isPrecedingDependenciesValid(schedule.slice(0, listIndex), job, jobIndex)) {}
			}

		}

		// return true
		true
	}


	 /* Helper method for isListValid. Checks if any job in the list has an invalid dependency on the passed job, otherwise
		* return true */
	def isParallelDependenciesValid(job: Job, index: Int, jobList: ListBuffer[Job]): Boolean = {
		for (j <- jobList) {
			if (j.id == job.id) {

			}
			else {
				if (isContainingDependency(j, Dependency.BEGIN_BEGIN) || !isParallelEEValid(job, j)) {
					false //TODO: check if the begin begin dependency is on 'job' itself. we currently just check if a job has that type of dependency. do this for all methods below
				}
			}
		}

		true
	}

	// Helper method for isParallelListInvalid. Checks if a job has contains a given dependency type
	def isContainingDependency(job: Job, dependencyType: Dependency.Type): Boolean = {
		for (dependency <- job.dependencies) {
			if (dependency.dependencyType == dependencyType) {
				return true
			}
		}

		false
	}

	/* Helper method for isParallelListInvalid. Checks if an End-End dependency
	 * from thisJob to thatJob is valid based on their durations.
	 */
	def isParallelEEValid(thisJob: Job, thatJob: Job): Boolean = {
		for (dependency <- thisJob.dependencies) {
			if (dependency.dependencyType == Dependency.END_END) {
				if (dependency.dependencyID == thatJob.id && thisJob.duration < thatJob.duration) {
					return false
				}
			}
		}

		true
	}

	// Helper for isListValid. Checks if preceding dependencies are valid on a passed job
	def isPrecedingDependenciesValid(subschedule: ListBuffer[ListBuffer[Job]], job: Job, jobIndex: Int): Boolean = {
		// Others can't have end-begin or begin begin
		for (jobList <- subschedule) {
			for (j <- jobList) {
				if (!verifyPrecedingDependencies(job, j, subschedule, jobIndex)) {
					return false
				}
			}
		}

		true

	}

	/* Helper method for isPrecendingDependencies Valid. Checks all types of dependencies against thisJob,
	 * and sees if any are of an invalid type or in an invalid place. Named as a procedure as its primary purpose
	 * is to check against a wide array of dependencies and invalid places.
	 */
	def verifyPrecedingDependencies(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (isPrecedingJobContaining(thatJob, Dependency.END_BEGIN, Dependency.BEGIN_BEGIN)) {
			return false
		}

		if (!isPrecedingDurationsValid(thisJob, thatJob, subschedule, jobIndex)) {
			return false
		}

		true
	}


	/* Helper method for verifyPrecedingDependencies. Checks if a job  contains any of two dependency types.
	 */
	def isPrecedingJobContaining(job: Job, dependency: Dependency.Type, otherDependency: Dependency.Type): Boolean = {
		for (d <- job.dependencies) {
			if (d.dependencyType == dependency || d.dependencyType == otherDependency) {
				return true
			}
		}

		false
	}

	/* Helper method for verifyPrecedingDependencies. Checks if the duration of a job 'thatjob' coming before
	 * 'thisjob' is a valid duration (i.e. it is not too large depending on the type of dependency is has on 'thisJob')
	 */
	def isPrecedingDurationsValid(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (!isPrecedingBEValid(thatJob, subschedule, jobIndex) || !isPrecedingEEValid(thisJob, thatJob, subschedule, jobIndex)) {
			return false
		}

		true
	}

	/* Helper method for isPrecidingDuraitonsValid. Checks if a begin-end dependency from a preceding job
	 * to a given job is valid (i.e. if the duration extends past where that given job starts)
	 */
	def isPrecedingBEValid(thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (isPrecedingJobContaining(thatJob, Dependency.BEGIN_END, Dependency.BEGIN_END)) { //TODO: fix this double paramater
			thatJob.duration > jobListDuration(subschedule.slice(jobIndex, subschedule.size))
		}

		true

	}

	/* Helper method for isPrecedingDurationsValid. Checks if an end-end dependency from a preceding job to a given job
	 * is valid (i.e. if the duration is extends past where the given job ends
	 */
	def isPrecedingEEValid(thisJob: Job, thatJob: Job, subschedule: ListBuffer[ListBuffer[Job]], jobIndex: Int): Boolean = {
		if (isPrecedingJobContaining(thatJob, Dependency.END_END, Dependency.END_END)) { // TODO: fix double paramater
			thatJob.duration > jobListDuration(subschedule.slice(jobIndex, subschedule.size)) + thisJob.duration
		}

		true
	}
}
