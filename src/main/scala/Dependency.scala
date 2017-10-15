package scheduler

class Dependency(_dependencyType: Dependency.Type) {
	def dependencyType = _dependencyType
}

object Dependency {
	sealed abstract class Type{}

	case object BEGIN_BEGIN extends Type
	case object BEGIN_END extends Type
	case object END_BEGIN extends Type
	case object END_END extends Type
}
