package scheduler

import scala.collection.mutable.ListBuffer

case class CircularDependencyException(jobList: Set[Job]) extends Exception (jobList.toString){

}
