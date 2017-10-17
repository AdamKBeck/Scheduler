package scheduler

case class DependencyException() {

}

object DependencyException {
	//TODO: verify methods to avoid repeated code

	sealed abstract class ErrorCode(val code: String) extends Exception(code) {

	}

	case class CIRCULAR_DEPENDENCY(jobs: List[Job]) extends ErrorCode(jobs.toString)
}
