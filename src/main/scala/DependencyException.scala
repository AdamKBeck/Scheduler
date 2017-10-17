package scheduler

import scala.collection.mutable.ListBuffer

case class DependencyException(jobList: Set[Job]) extends Exception (jobList.toString){
// TODO: rename as DependencyException modeled after Parser Exception, with verify methods to aid SoftWarePlatform
}
