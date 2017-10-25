package scheduler

case class DependencyException() {

}

object DependencyException {
	//TODO: verify methods to avoid repeated code

	// Checks whether a job contains a dependency type on a passed job id
	def verify(job: Job, dependency: Dependency.Type, id: Int): Boolean = {
		job.dependencies.exists(d => d.dependencyType == dependency && d.dependencyID == id)
	}

	sealed abstract class ErrorCode(val code: String) extends Exception(code) {

	}

	case class CIRCULAR_DEPENDENCY(jobs: List[Job]) extends ErrorCode(jobs.toString)
	// For future use, more custom exceptions would be added here
}
