package scheduler

case class Dependency(private val _dependencyType: Dependency.Type, _jobID: Int, _dependencyID: Int) {
	def dependencyType = _dependencyType

	def jobID = _jobID

	def dependencyID = _dependencyID
}

object Dependency {
	sealed abstract class Type{}

	case object BEGIN_BEGIN extends Type
	case object BEGIN_END extends Type
	case object END_BEGIN extends Type
	case object END_END extends Type
}
