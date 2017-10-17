package scheduler

case class DependencyException() {

}

object DependencyException {
	//TODO: verify methods to avoid repeated code

	// Checks whether a job contains a dependency type on a passed job id
	def verify(job: Job, dependency: Dependency.Type, id: Int): Boolean = {
		for (d <- job.dependencies) {
			if (d.dependencyType == dependency && d.dependencyID == id) {
				return false
			}
		}
		
		true
	}

	sealed abstract class ErrorCode(val code: String) extends Exception(code) {

	}

	case class CIRCULAR_DEPENDENCY(jobs: List[Job]) extends ErrorCode(jobs.toString)
}
