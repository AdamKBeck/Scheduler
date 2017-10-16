package scheduler

case class Job(private val _dependencies: Set[Dependency], _duration: Int, _id: Int) {
	def dependencies = _dependencies

	def duration = _duration

	def id = _id // unique to each job
}

object Job extends App {

}

