package scheduler

case class Job(private val _dependencies: (List[Int], List[Dependency.Type], List[Int]), _duration: Int) {
	def dependencies = _dependencies
	def duration = _duration
}

object Job extends App {
	printf("Test")
}
