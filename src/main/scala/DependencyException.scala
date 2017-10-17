package scheduler

case class DependencyException() {

}

object DependencyException {
	//TODO: verify methods to avoid repeated code

	// Checks whether a job contains a dependency type on a passed job id
	def verify(job: Job, dependency: Dependency.Type, id: Int): Boolean = {
		for (d <- job.dependencies) {
			if (d.dependencyType == dependency && d.dependencyID == id) {
				return true
			}
		}
		/* Function is placed here instead of SoftwarePlatform because we may want to throw
		 * an exception for future use of this project. (e.g. throw an exception from an
		 * overloaded verify function in here */
		false
	}

	sealed abstract class ErrorCode(val code: String) extends Exception(code) {

	}

	case class CIRCULAR_DEPENDENCY(jobs: List[Job]) extends ErrorCode(jobs.toString)
}
